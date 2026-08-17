#' Build a combined FFmpeg audio filter string
#'
#' Constructs the audio filter arguments for an FFmpeg command that optionally
#' combines loudness normalization (\code{loudnorm}) and audio anonymization
#' (\code{silence}, \code{beep}, or \code{noise}) over specified time ranges.
#'
#' The filter order is always: normalization first, then anonymization.
#' For \code{silence}, a simple \code{-af} chain suffices. For \code{beep}
#' and \code{noise}, a \code{filter_complex} graph is required because a
#' synthetic audio source must be mixed in.
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
	loudnorm    <- getOption("act.ffmpeg.audio.loudnorm")
	anony_type  <- getOption("act.ffmpeg.audio.anony_filter")
	beep_freq   <- getOption("act.ffmpeg.audio.anony_beep_freq", 1000L)

	use_normalize <- isTRUE(audio_normalize) && !is.null(loudnorm) && nzchar(loudnorm)
	use_anony     <- !is.null(audio_anonymize) &&
	                 is.data.frame(audio_anonymize) &&
	                 nrow(audio_anonymize) > 0 &&
	                 !is.null(anony_type)

	empty <- list(
		type      = "none",
		af_string = "",
		fc_parts  = character(0),
		out_label = "",
		map_a     = ""
	)

	if (!use_normalize && !use_anony) {
		return(empty)
	}

	if (!use_anony) {
		return(list(
			type      = "simple",
			af_string = sprintf('-af "%s"', loudnorm),
			fc_parts  = character(0),
			out_label = "",
			map_a     = ""
		))
	}

	enable_expr <- .audio_enable_expr(audio_anonymize)

	if (anony_type == "silence") {
		af_parts <- character(0)
		if (use_normalize) af_parts <- c(af_parts, loudnorm)
		af_parts <- c(af_parts, sprintf("volume=enable='%s':volume=0", enable_expr))
		return(list(
			type      = "simple",
			af_string = sprintf('-af "%s"', paste(af_parts, collapse = ",")),
			fc_parts  = character(0),
			out_label = "",
			map_a     = ""
		))
	}

	fc_parts <- character(0)

	if (use_normalize) {
		fc_parts <- c(fc_parts, sprintf("[0:a]%s[anorm]", loudnorm))
		input_label <- "[anorm]"
	} else {
		input_label <- "[0:a]"
	}

	fc_parts <- c(fc_parts,
		sprintf("%svolume=enable='%s':volume=0[amuted]", input_label, enable_expr)
	)

	if (anony_type == "beep") {
		fc_parts <- c(fc_parts,
			sprintf("sine=frequency=%d,volume='%s'[asrc]", as.integer(beep_freq), enable_expr),
			"[amuted][asrc]amix=inputs=2:normalize=0[aout]"
		)
	} else {
		fc_parts <- c(fc_parts,
			sprintf("anoisesrc=amplitude=0.04,volume='%s'[asrc]", enable_expr),
			"[amuted][asrc]amix=inputs=2:normalize=0[aout]"
		)
	}

	list(
		type      = "filter_complex",
		af_string = "",
		fc_parts  = fc_parts,
		out_label = "[aout]",
		map_a     = '-map "[aout]"'
	)
}

# Build FFmpeg between() enable expression from a data.frame with start/end columns
.audio_enable_expr <- function(ranges) {
	paste(
		vapply(seq_len(nrow(ranges)), function(i) {
			sprintf("between(t,%s,%s)", ranges$start[i], ranges$end[i])
		}, character(1)),
		collapse = "+"
	)
}
