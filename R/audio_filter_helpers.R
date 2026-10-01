#' Build the FFmpeg sound filters: channel, loudness, anonymization
#'
#' One builder for the sound filters of every export (viewer clips and sound
#' files, act cuts): the channel selection, the loudness normalization and the
#' anonymization of time ranges by beep, noise, mute or a voice distortion.
#' The result is either a simple chain (\code{af}, for \code{-af}) or a filter
#' graph (\code{graph}) that reads \code{[0:a]} and writes \code{[aout]}. The
#' beep and the noise are generated inside the graph, so no further input is
#' needed.
#'
#' Order: channel first, then the anonymization, then the loudness (in front of
#' the timed volume the loudness filter shifted its time base). The timed volume
#' is evaluated in blocks of 64 samples (1.5 ms), so a range starts and ends
#' where it is annotated.
#'
#' @param channel Character; \code{"stereo"} (default) keeps the channels,
#'   \code{"left"} / \code{"right"} put the left / right channel on both sides
#'   (a stereo file with the same signal twice).
#' @param mono Logical; \code{TRUE} writes one channel (the mix of both, or the
#'   chosen side).
#' @param normalize Logical; \code{TRUE} applies the loudness filter of the
#'   option \code{act.ffmpeg.audio.loudnorm} (nothing if it is not set).
#' @param ranges Data frame with columns \code{start} and \code{end} (seconds
#'   from the start of the output) or \code{NULL}: the ranges to anonymize.
#'   \code{NULL} switches the anonymization off; an empty data frame keeps it
#'   on without ranges (a distortion with scope \code{"all"} still applies).
#' @param mode Character; \code{"beep"}, \code{"noise"}, \code{"mute"} or
#'   \code{"distort"}. Default is the option
#'   \code{act.ffmpeg.audio.anony_filter} (\code{"silence"} is read as
#'   \code{"mute"}), else \code{"beep"}.
#' @param strength Numeric between 0 and 1; loudness of beep and noise.
#' @param distort List or \code{NULL}; the voice distortion effects
#'   (\code{pitch}, \code{vibrato}, \code{tremolo}, \code{robot},
#'   \code{flanger}, each a list with \code{on} and its parameters) and
#'   \code{scope} (\code{"segments"} or \code{"all"}).
#' @param beepFreq Numeric; frequency of the beep in Hz. Default is the option
#'   \code{act.ffmpeg.audio.anony_beep_freq}.
#'
#' @return A list with \code{af} (character or \code{NULL}) and \code{graph}
#'   (character or \code{NULL}); at most one of them is set.
#'
#' @seealso \link{helper_ffmpeg_args_audio}, \link{helper_ffmpeg_args_clip}
#'
#' @export
#'
#' @examples
#' act::helper_audio_filter_parts(channel = "left",
#'                                ranges = data.frame(start = 1, end = 2.5))
#'
helper_audio_filter_parts <- function(channel   = "stereo",
                                      mono      = FALSE,
                                      normalize = FALSE,
                                      ranges    = NULL,
                                      mode      = .audio_default_mode(),
                                      strength  = 0.8,
                                      distort   = NULL,
                                      beepFreq  = getOption("act.ffmpeg.audio.anony_beep_freq", 800)) {
	mode <- if (identical(mode, "silence")) "mute" else mode
	ln <- getOption("act.ffmpeg.audio.loudnorm")
	ln <- if (isTRUE(normalize) && !is.null(ln) && nzchar(ln)) ln else NULL
	pre <- .audio_channel_pan(channel, mono)
	pre_str <- if (length(pre)) paste0(pre, ",") else ""
	tail_ln <- if (is.null(ln)) "[aout]" else paste0("[amixed];[amixed]", ln, "[aout]")

	enabled <- !is.null(ranges) && is.data.frame(ranges)
	active  <- enabled && nrow(ranges) > 0
	cond <- if (active) paste(sprintf("between(t,%.3f,%.3f)", ranges$start, ranges$end), collapse = "+") else NULL
	inrange <- if (active) sprintf("gt(%s,0)", cond) else NULL

	if (active && mode %in% c("beep", "noise")) {
		tone <- if (mode == "beep")
			sprintf("sine=frequency=%s:sample_rate=44100", format(as.numeric(beepFreq)))
			else "anoisesrc=color=white:sample_rate=44100"
		base <- if (mode == "beep") 0.5 else 0.3
		graph <- sprintf(paste0("[0:a]%sasetnsamples=n=64,volume='if(%s,0,1)':eval=frame[a0];",
			"%s,asetnsamples=n=64,volume='if(%s,%.4f,0)':eval=frame[a1];",
			"[a0][a1]amix=inputs=2:duration=first:normalize=0%s"),
			pre_str, inrange, tone, inrange, base * strength, tail_ln)
		return(list(af = NULL, graph = graph))
	}

	if (mode == "distort" && enabled) {
		dist  <- .audio_distort_filter(distort)
		scope <- distort$scope %||% "segments"
		if (!nzchar(dist)) {
			parts <- c(pre, ln)
			return(list(af = if (length(parts)) paste(parts, collapse = ",") else NULL, graph = NULL))
		}
		if (identical(scope, "all") || !active) {
			parts <- c(pre, dist, ln)
			return(list(af = paste(parts, collapse = ","), graph = NULL))
		}
		graph <- sprintf(paste0("[0:a]%sasplit=2[c][d];",
			"[c]aresample=44100,asetnsamples=n=64,volume='if(%s,0,1)':eval=frame[a0];",
			"[d]%s,asetnsamples=n=64,volume='if(%s,1,0)':eval=frame[a1];",
			"[a0][a1]amix=inputs=2:duration=first:normalize=0%s"),
			pre_str, inrange, dist, inrange, tail_ln)
		return(list(af = NULL, graph = graph))
	}

	parts <- pre
	if (active && mode == "mute") {
		parts <- c(parts, "asetnsamples=n=64", sprintf("volume=enable='%s':volume=0", cond))
	}
	parts <- c(parts, ln)
	list(af = if (length(parts)) paste(parts, collapse = ",") else NULL, graph = NULL)
}


#' Build a combined FFmpeg audio filter string
#'
#' Constructs the audio filter arguments for an FFmpeg command that optionally
#' combines loudness normalization (\code{loudnorm}) and audio anonymization
#' (\code{silence}, \code{beep}, or \code{noise}) over specified time ranges.
#' The filters are built by \link{helper_audio_filter_parts}; the
#' anonymization comes first, the normalization last. The beep and the noise
#' are generated inside the filter graph.
#'
#' Audio filter and beep frequency are read from options:
#' \code{act.ffmpeg.audio.anony_filter},
#' \code{act.ffmpeg.audio.anony_beep_freq},
#' \code{act.ffmpeg.audio.loudnorm}.
#'
#' @param audio_normalize Logical. If \code{TRUE} and option
#'   \code{act.ffmpeg.audio.loudnorm} is set, the loudnorm filter is applied.
#'   Default is \code{FALSE}.
#' @param audio_anonymize \code{data.frame} with columns \code{start} and
#'   \code{end} (numeric, seconds relative to cut start), or \code{NULL} for
#'   no anonymization. Default is \code{NULL}.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{type}}{\code{"none"}, \code{"simple"}, or
#'       \code{"filter_complex"}}
#'     \item{\code{af_string}}{For \code{"simple"}: the full \code{-af "..."}
#'       argument string. \code{""} otherwise.}
#'     \item{\code{fc_parts}}{For \code{"filter_complex"}: character vector of
#'       filter graph segments. \code{character(0)} otherwise.}
#'     \item{\code{out_label}}{For \code{"filter_complex"}: the final audio
#'       stream label, e.g. \code{"[aout]"}. \code{""} otherwise.}
#'     \item{\code{map_a}}{For \code{"filter_complex"}: the \code{-map}
#'       argument, e.g. \code{"-map \"[aout]\""}. \code{""} otherwise.}
#'   }
#'
#' @export
helper_audio_filter_build <- function(
	audio_normalize = FALSE,
	audio_anonymize = NULL
) {
	anony_type <- getOption("act.ffmpeg.audio.anony_filter")
	ranges <- if (!is.null(anony_type)) audio_anonymize else NULL
	parts <- helper_audio_filter_parts(
		channel   = "stereo",
		mono      = FALSE,
		normalize = audio_normalize,
		ranges    = ranges,
		mode      = .audio_default_mode(),
		strength  = 0.8,
		distort   = NULL,
		beepFreq  = getOption("act.ffmpeg.audio.anony_beep_freq", 800)
	)
	if (!is.null(parts$graph)) {
		return(list(
			type      = "filter_complex",
			af_string = "",
			fc_parts  = strsplit(parts$graph, ";", fixed = TRUE)[[1]],
			out_label = "[aout]",
			map_a     = '-map "[aout]"'
		))
	}
	if (!is.null(parts$af)) {
		return(list(
			type      = "simple",
			af_string = sprintf('-af "%s"', parts$af),
			fc_parts  = character(0),
			out_label = "",
			map_a     = ""
		))
	}
	list(type = "none", af_string = "", fc_parts = character(0), out_label = "", map_a = "")
}


# ===== INTERNAL HELPERS =====

.audio_default_mode <- function() {
	m <- getOption("act.ffmpeg.audio.anony_filter")
	if (is.null(m) || !nzchar(m)) return("beep")
	if (identical(m, "silence")) "mute" else m
}

.audio_channel_pan <- function(channel, mono) {
	channel <- channel %||% "stereo"
	if (identical(channel, "left"))  return(if (isTRUE(mono)) "pan=mono|c0=c0" else "pan=stereo|c0=c0|c1=c0")
	if (identical(channel, "right")) return(if (isTRUE(mono)) "pan=mono|c0=c1" else "pan=stereo|c0=c1|c1=c1")
	if (isTRUE(mono)) return("pan=mono|c0=0.5*c0+0.5*c1")
	NULL
}

.audio_distort_filter <- function(distort) {
	d <- distort %||% list()
	parts <- character(0)
	if (isTRUE(d$pitch$on)) {
		f <- max(0.5, min(2, d$pitch$factor %||% 0.9))
		parts <- c(parts, sprintf("asetrate=44100*%.4f,aresample=44100,atempo=%.4f", f, 1 / f))
	}
	if (isTRUE(d$vibrato$on))
		parts <- c(parts, sprintf("vibrato=f=%.2f:d=%.2f", d$vibrato$freq %||% 5, d$vibrato$depth %||% 0.5))
	if (isTRUE(d$tremolo$on))
		parts <- c(parts, sprintf("tremolo=f=%.2f:d=%.2f", d$tremolo$freq %||% 5, d$tremolo$depth %||% 0.5))
	if (isTRUE(d$robot$on)) {
		ri <- max(0, min(1, d$robot$intensity %||% 0.8))
		parts <- c(parts, sprintf("afftfilt=real='hypot(re,im)*cos((random(0)*2-1)*3.14159*%.2f)':imag='hypot(re,im)*sin((random(0)*2-1)*3.14159*%.2f)':win_size=512:overlap=0.75", ri, ri))
	}
	if (isTRUE(d$flanger$on))
		parts <- c(parts, sprintf("flanger=depth=%.2f", 2 + (d$flanger$intensity %||% 0.5) * 8))
	if (length(parts) == 0) return("")
	paste(c(parts, "aresample=44100"), collapse = ",")
}
