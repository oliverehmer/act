#' Helper: Build the FFmpeg arguments for an image output
#'
#' Builds the argument vector (without the program name) for one FFmpeg call
#' that writes a single image (JPG or PNG). All image outputs of act and iclo
#' are built here, so the rules for reading video sources apply in one place:
#' \itemize{
#'   \item The seek is done in two stages: a fast seek to one second before
#'   the target in front of the input, then an exact seek of the rest after
#'   it (\code{-ss t-1 -i ... -ss 1}). A single seek in front of the input
#'   (\code{-ss t -i}) can miss the frames that are displayed just before a
#'   keyframe but decoded after it (B-frames): it then delivers the keyframe
#'   instead.
#'   With a filter, the rest is cut by a \code{trim} at the start of the
#'   filter chain instead, so the filter only processes the target frame.
#'   \item The seek target is the time as players that honour the edit list
#'   of the file show it (ELAN with AVFoundation, browsers): the edit list
#'   offset of the video track is added, and the frame that is on screen at
#'   that moment is taken (not the next one). With the option
#'   \code{act.media.timeline = "raw"} (ELAN with VLC) the offset is not
#'   added. Camera files with B-frames
#'   carry an offset of their reorder delay (e.g. 2 frames), cuts made with
#'   stream copy may carry any offset.
#'   \item Video inputs (mp4, mov, m4a, m4v) get \code{-ignore_editlist 1
#'   -avoid_negative_ts make_zero} in front of \code{-i}. Without these flags a
#'   file with an edit list delivers a black frame. Image inputs never get them
#'   (the image demuxer rejects the option).
#'   \item JPG outputs are written with \code{-q:v} derived from
#'   \code{imageQuality}.
#' }
#'
#' @param output Character; path of the image to write. The extension decides
#'   the format: \code{jpg}/\code{jpeg} or \code{png}.
#' @param input Character; path of the source file (video or image). Its
#'   extension decides whether the edit list flags are set, so the real path
#'   must be given (also when the call is written into a cut list).
#' @param startsec Numeric or \code{NULL}; seek position in seconds, on the
#'   timeline of the file as players show it.
#' @param duration Numeric or \code{NULL}; length of the input window in
#'   seconds (\code{-t} before \code{-i}).
#' @param inputsExtra Character vector or \code{NULL}; further inputs (e.g. a
#'   palette image) appended after the main input.
#' @param videoFilter Character or \code{NULL}; a simple filter chain
#'   (\code{-vf}).
#' @param filterComplex Character or \code{NULL}; a filter graph
#'   (\code{-filter_complex}). Cannot be combined with \code{videoFilter}.
#' @param filterMap Character or \code{NULL}; output label of the filter graph
#'   that is written (\code{-map}).
#' @param imageQuality Numeric; JPG quality from 1 (worst) to 100 (best).
#'   Default is the option \code{act.ffmpeg.image.quality}.
#' @param maxHeight Integer or \code{NULL}; scale the image down to this height
#'   (never up). Only together with \code{videoFilter} or without a filter.
#' @param videoOffset Numeric or \code{NULL}; edit list offset of the video
#'   track in seconds, e.g. column \code{video.editlist.offset} of
#'   \link{media_metadata_read}. \code{NULL} reads it from the file.
#' @param videoFps Numeric or \code{NULL}; frame rate of the video, e.g.
#'   column \code{video.fps}. \code{NULL} reads it from the file.
#'
#' @return Character vector of FFmpeg arguments.
#'
#' @seealso \link{helper_ffmpeg_run}, \link{helper_ffmpeg_path}
#'
#' @export
#'
#' @examples
#' act::helper_ffmpeg_args(output = "still.jpg", input = "video.mp4", startsec = 12.5)
#'
helper_ffmpeg_args <- function(output,
                               input         = NULL,
                               startsec      = NULL,
                               duration      = NULL,
                               inputsExtra   = NULL,
                               videoFilter   = NULL,
                               filterComplex = NULL,
                               filterMap     = NULL,
                               imageQuality  = getOption("act.ffmpeg.image.quality", 100),
                               maxHeight     = NULL,
                               videoOffset   = NULL,
                               videoFps      = NULL) {
	if (missing(output) || length(output) != 1 || is.na(output) || !nzchar(output)) {
		cli::cli_abort("Parameter {.arg output} is missing.")
	}
	if (!is.null(videoFilter) && !is.null(filterComplex)) {
		cli::cli_abort("Use either {.arg videoFilter} or {.arg filterComplex}, not both.")
	}
	if (!is.null(maxHeight) && !is.null(filterComplex)) {
		cli::cli_abort("{.arg maxHeight} cannot be combined with {.arg filterComplex}.")
	}
	out_ext <- tolower(tools::file_ext(output))
	if (!out_ext %in% c("jpg", "jpeg", "png")) {
		cli::cli_abort("Output format {.val {out_ext}} is not supported (jpg, jpeg, png).")
	}

	args <- c("-hide_banner", "-loglevel", "error")
	seek_out <- NULL
	if (!is.null(input)) {
		if (!is.null(startsec)) {
			target   <- max(0, as.numeric(startsec))
			if (length(.ffmpeg_input_flags(input))) {
				timing <- .ffmpeg_seek_timing(input, videoOffset, videoFps)
				target <- target + timing$offset
				if (is.finite(timing$frame_dur)) target <- target - timing$frame_dur * 0.99
				target <- max(0, target)
			}
			pre_sec  <- max(0, target - 1)
			seek_out <- target - pre_sec
			args <- c(args, "-ss", .ffmpeg_seconds(pre_sec))
			if (!is.null(duration)) args <- c(args, "-t", .ffmpeg_seconds(seek_out + as.numeric(duration)))
		} else if (!is.null(duration)) {
			args <- c(args, "-t", .ffmpeg_seconds(as.numeric(duration)))
		}
		args <- c(args, .ffmpeg_input_flags(input), "-i", input)
	}
	for (extra in inputsExtra) {
		args <- c(args, .ffmpeg_input_flags(extra), "-i", extra)
	}

	trim_vf  <- if (!is.null(seek_out)) sprintf("trim=start=%s", .ffmpeg_seconds(seek_out)) else NULL
	scale_vf <- .ffmpeg_scale_filter(maxHeight)
	vf <- c(scale_vf, videoFilter)
	vf <- vf[!is.na(vf) & nzchar(vf)]
	if (length(vf) > 0) {
		args <- c(args, "-vf", paste(c(trim_vf, vf), collapse = ","))
	} else if (!is.null(filterComplex)) {
		fc <- if (is.null(trim_vf)) filterComplex
			else gsub("[0:v]", paste0("[0:v]", trim_vf, ","), filterComplex, fixed = TRUE)
		args <- c(args, "-filter_complex", fc)
		if (!is.null(filterMap)) args <- c(args, "-map", filterMap)
	} else if (!is.null(seek_out)) {
		args <- c(args, "-ss", .ffmpeg_seconds(seek_out))
	}
	args <- c(args, "-frames:v", "1")
	if (out_ext %in% c("jpg", "jpeg")) {
		args <- c(args, "-q:v", as.character(.ffmpeg_quality_qv(imageQuality)))
	}
	c(args, "-update", "1", "-y", output)
}


#' Helper: Build the FFmpeg arguments for a video clip
#'
#' Builds the argument vector (without the program name) for one FFmpeg call
#' that writes a video clip (MP4). The rules of \link{helper_ffmpeg_args}
#' apply to the picture: exact two-stage seek, edit list flags for video
#' inputs, and the picture of a clip starts with the frame that players
#' honouring the edit list (ELAN with AVFoundation) show at \code{startsec}.
#' The sound is read from a second input of the same file (or
#' \code{audioInput}) with its own seek: its edit list is honoured, so the
#' encoder delay at the start of AAC tracks is skipped like ELAN does, and
#' picture and sound are in sync as in ELAN.
#' An AAC encoder shifts its output by 1024 samples, which no player
#' compensates in a file without edit list; the first 1024 samples are
#' therefore cut at the very end of the sound chain, so the clip plays at the
#' right time. Filters (e.g. an anonymization in a graph) still see the time
#' of the clip, so their ranges stay exact. The sound keeps the sample rate of the source (a
#' loudness filter would otherwise raise it and shift that delay). In a filter graph the sound of
#' the source is therefore addressed as \code{[0:a]} as usual - it is
#' rewritten to the sound input.
#'
#' Every clip is written as H.264 with \code{yuv420p} (PowerPoint plays
#' nothing else), with \code{-use_editlist 0 -movflags +faststart}, without
#' B-frames (\code{-bf 0}): with B-frames the picture of a file without edit
#' list starts two frames after the sound. The frame rate is set explicitly
#' (\code{-r}): after the trim the encoder would otherwise assume the time
#' base as frame rate and, with a bit rate, write grey frames at the start.
#'
#' @param output Character; path of the clip (mp4, mov or m4v).
#' @param input Character; path of the source video.
#' @param startsec Numeric; start in seconds on the timeline of the file as
#'   players show it.
#' @param duration Numeric; length in seconds.
#' @param inputsExtra List or \code{NULL}; further inputs after the video,
#'   each a path or a complete argument vector (e.g.
#'   \code{c("-f", "lavfi", "-t", "3", "-i", "sine")}). Their indices in a
#'   filter graph follow the video (1, 2, ...); the sound input comes last.
#' @param videoFilter Character or \code{NULL}; a simple video filter chain.
#' @param filterComplex Character or \code{NULL}; a filter graph using
#'   \code{[0:v]} and \code{[0:a]}.
#' @param videoMap Character or \code{NULL}; output label of the picture in
#'   \code{filterComplex}.
#' @param audioMap Character or \code{NULL}; output label of the sound in
#'   \code{filterComplex}. \code{NULL} takes the sound of the source.
#' @param audioFilter Character or \code{NULL}; filter chain for the sound
#'   of the source (\code{-af}), when \code{audioMap} is \code{NULL}.
#' @param maxHeight,maxWidth Integer or \code{NULL}; scale the picture down
#'   to fit into this height and/or width (never up). Defaults are the
#'   options \code{act.ffmpeg.video.max_height} and
#'   \code{act.ffmpeg.video.max_width}.
#' @param videoBitrate Character; video bit rate, used when \code{videoCrf}
#'   is \code{NULL}. Default is the option \code{act.ffmpeg.video.bitrate}.
#' @param videoCrf Numeric or \code{NULL}; constant rate factor instead of a
#'   bit rate. Default is the option \code{act.ffmpeg.video.crf}.
#' @param keyframeInterval Integer; distance of keyframes in frames. Default
#'   is the option \code{act.ffmpeg.video.keyframe_interval}.
#' @param audioCodecArgs Character vector; codec arguments of the sound.
#' @param metadataArgs Character vector or \code{NULL}; further output
#'   arguments, e.g. from \link{helper_metadata_ffmpeg_argv}.
#' @param videoOffset,videoFps See \link{helper_ffmpeg_args}.
#' @param audioInput Character or \code{NULL}; file the sound is taken from.
#'   \code{NULL} takes it from \code{input}.
#' @param videoCopy Logical; copy the video stream instead of encoding it
#'   (fast, but the clip starts at the keyframe before \code{startsec} and no
#'   filter is possible).
#' @param withAudio Logical; \code{FALSE} writes the picture only.
#'
#' @return Character vector of FFmpeg arguments.
#'
#' @seealso \link{helper_ffmpeg_args}, \link{helper_ffmpeg_run}
#'
#' @export
#'
#' @examples
#' act::helper_ffmpeg_args_clip(output = "cut.mp4", input = "video.mp4",
#'                              startsec = 12.5, duration = 4)
#'
helper_ffmpeg_args_clip <- function(output,
                                    input,
                                    startsec,
                                    duration,
                                    inputsExtra      = NULL,
                                    videoFilter      = NULL,
                                    filterComplex    = NULL,
                                    videoMap         = NULL,
                                    audioMap         = NULL,
                                    audioFilter      = NULL,
                                    maxHeight        = getOption("act.ffmpeg.video.max_height"),
                                    maxWidth         = getOption("act.ffmpeg.video.max_width"),
                                    videoBitrate     = getOption("act.ffmpeg.video.bitrate", "8M"),
                                    videoCrf         = getOption("act.ffmpeg.video.crf"),
                                    keyframeInterval = getOption("act.ffmpeg.video.keyframe_interval", 25),
                                    audioCodecArgs   = c("-c:a", "aac", "-b:a", getOption("act.ffmpeg.audio.bitrate", "192k")),
                                    metadataArgs     = NULL,
                                    videoOffset      = NULL,
                                    videoFps         = NULL,
                                    audioInput       = NULL,
                                    videoCopy        = FALSE,
                                    withAudio        = TRUE) {
	if (missing(output) || length(output) != 1 || is.na(output) || !nzchar(output)) {
		cli::cli_abort("Parameter {.arg output} is missing.")
	}
	if (!tolower(tools::file_ext(output)) %in% c("mp4", "mov", "m4v")) {
		cli::cli_abort("Output format {.val {tools::file_ext(output)}} is not supported (mp4, mov, m4v).")
	}
	if (!is.null(videoFilter) && !is.null(filterComplex)) {
		cli::cli_abort("Use either {.arg videoFilter} or {.arg filterComplex}, not both.")
	}
	if (isTRUE(videoCopy) && (!is.null(videoFilter) || !is.null(filterComplex))) {
		cli::cli_abort("{.arg videoCopy} cannot be combined with a filter.")
	}
	startsec <- max(0, as.numeric(startsec))
	duration <- as.numeric(duration)
	timing   <- .ffmpeg_seek_timing(input, videoOffset, videoFps)
	raw_line <- identical(getOption("act.media.timeline", "editlist"), "raw")

	args <- c("-hide_banner", "-loglevel", "error")

	if (isTRUE(videoCopy)) {
		args <- c(args, "-ss", .ffmpeg_seconds(startsec + timing$offset), "-t", .ffmpeg_seconds(duration),
		          .ffmpeg_input_flags(input), "-i", input)
		seek_out <- NULL
	} else {
		target <- startsec + timing$offset
		if (is.finite(timing$frame_dur)) target <- target - timing$frame_dur * 0.99
		target   <- max(0, target)
		pre_sec  <- max(0, target - 1)
		seek_out <- target - pre_sec
		args <- c(args, "-ss", .ffmpeg_seconds(pre_sec), "-t", .ffmpeg_seconds(seek_out + duration),
		          .ffmpeg_input_flags(input), "-i", input)
	}
	for (extra in inputsExtra) {
		args <- c(args, if (length(extra) == 1) c(.ffmpeg_input_flags(extra), "-i", extra) else extra)
	}
	audio_idx <- 1L + length(inputsExtra)
	audio_src <- audioInput %||% input
	audio_aac  <- isTRUE(withAudio) && any(audioCodecArgs == "aac")
	audio_rate <- if (audio_aac) .ffmpeg_audio_rate(audio_src) else NA_real_
	audio_lead <- if (audio_aac) 1024 / (if (is.finite(audio_rate)) audio_rate else 48000) else 0
	if (isTRUE(withAudio)) {
		args <- c(args, "-ss", .ffmpeg_seconds(startsec), "-t", .ffmpeg_seconds(duration + audio_lead),
		          if (raw_line) .ffmpeg_input_flags(audio_src), "-i", audio_src)
	}
	lead_af <- if (audio_lead > 0)
		sprintf("atrim=start=%s,asetpts=PTS-STARTPTS", .ffmpeg_seconds(audio_lead)) else NULL

	trim_vf  <- if (!is.null(seek_out))
		sprintf("trim=start=%s,setpts=PTS-STARTPTS", .ffmpeg_seconds(seek_out)) else NULL
	scale_vf <- .ffmpeg_scale_filter(maxHeight, maxWidth)
	audio_label <- sprintf("[%d:a]", audio_idx)

	af <- c(audioFilter, lead_af)
	af <- if (length(af)) paste(af, collapse = ",") else NULL
	if (isTRUE(videoCopy)) {
		args <- c(args, "-map", "0:v:0", "-map", paste0(audio_idx, ":a?"), "-c:v", "copy")
		if (!is.null(af)) args <- c(args, "-af", af)
	} else {
		if (!is.null(filterComplex)) {
			fc <- gsub("[0:v]", paste0("[0:v]", trim_vf, ","), filterComplex, fixed = TRUE)
			fc <- gsub("[0:a]", audio_label, fc, fixed = TRUE)
			vmap <- videoMap %||% "[out]"
			if (!is.null(scale_vf)) {
				fc <- paste0(fc, ";", vmap, scale_vf, "[vmax]")
				vmap <- "[vmax]"
			}
			amap <- audioMap
			if (!is.null(amap) && !is.null(lead_af)) {
				fc <- paste0(fc, ";", amap, lead_af, "[alead]")
				amap <- "[alead]"
			}
			args <- c(args, "-filter_complex", fc, "-map", vmap)
		} else {
			vf <- c(trim_vf, scale_vf, videoFilter)
			vf <- vf[!is.na(vf) & nzchar(vf)]
			args <- c(args, "-vf", paste(vf, collapse = ","), "-map", "0:v:0")
			amap <- audioMap
		}
		if (!isTRUE(withAudio)) {
			args <- c(args, "-an")
		} else if (!is.null(amap)) {
			args <- c(args, "-map", amap)
		} else {
			args <- c(args, "-map", paste0(audio_idx, ":a?"))
			if (!is.null(af)) args <- c(args, "-af", af)
		}
		quality <- if (!is.null(videoCrf)) c("-crf", as.character(videoCrf)) else c("-b:v", as.character(videoBitrate))
		args <- c(args, "-c:v", "libx264", quality, "-pix_fmt", "yuv420p", "-bf", "0",
		          "-g", as.character(as.integer(keyframeInterval)),
		          if (is.finite(timing$frame_dur)) c("-r", sprintf("%.6g", 1 / timing$frame_dur)))
	}
	c(args, if (isTRUE(withAudio)) audioCodecArgs, if (is.finite(audio_rate)) c("-ar", as.character(audio_rate)),
	  "-t", .ffmpeg_seconds(duration), metadataArgs,
	  "-use_editlist", "0", "-movflags", "+faststart", "-y", output)
}


#' Helper: Build the FFmpeg arguments for a sound file
#'
#' Builds the argument vector (without the program name) for one FFmpeg call
#' that writes a sound file (wav, mp3 or m4a) from a sound or video source.
#' The sound of an MP4/MOV source is read with its edit list honoured, as ELAN
#' plays it (with \code{act.media.timeline = "raw"} it is ignored, as VLC
#' does). A wav is copied sample by sample when no filter is applied;
#' otherwise it is written with the bit depth and sample rate of the source.
#' mp3 and m4a are encoded with \code{act.ffmpeg.audio.bitrate}.
#'
#' @param output Character; path of the sound file (wav, mp3 or m4a).
#' @param input Character; path of the source.
#' @param startsec Numeric; start in seconds.
#' @param duration Numeric; length in seconds.
#' @param audioFilter Character or \code{NULL}; a simple filter chain
#'   (\code{-af}), e.g. \code{af} of \link{helper_audio_filter_parts}.
#' @param filterComplex Character or \code{NULL}; a filter graph reading
#'   \code{[0:a]} and writing \code{audioMap}, e.g. \code{graph} of
#'   \link{helper_audio_filter_parts}.
#' @param audioMap Character; output label of \code{filterComplex}.
#' @param audioBitrate Character; bit rate of mp3 and m4a. Default is the
#'   option \code{act.ffmpeg.audio.bitrate}.
#' @param metadataArgs Character vector or \code{NULL}; further output
#'   arguments.
#'
#' @return Character vector of FFmpeg arguments.
#'
#' @seealso \link{helper_audio_filter_parts}, \link{helper_ffmpeg_args_clip},
#'   \link{helper_ffmpeg_run}
#'
#' @export
#'
#' @examples
#' act::helper_ffmpeg_args_audio(output = "cut.wav", input = "sound.wav",
#'                               startsec = 12.5, duration = 4)
#'
helper_ffmpeg_args_audio <- function(output,
                                     input,
                                     startsec,
                                     duration,
                                     audioFilter   = NULL,
                                     filterComplex = NULL,
                                     audioMap      = "[aout]",
                                     audioBitrate  = getOption("act.ffmpeg.audio.bitrate", "192k"),
                                     metadataArgs  = NULL) {
	if (missing(output) || length(output) != 1 || is.na(output) || !nzchar(output)) {
		cli::cli_abort("Parameter {.arg output} is missing.")
	}
	out_ext <- tolower(tools::file_ext(output))
	if (!out_ext %in% c("wav", "mp3", "m4a")) {
		cli::cli_abort("Output format {.val {out_ext}} is not supported (wav, mp3, m4a).")
	}
	if (!is.null(audioFilter) && !is.null(filterComplex)) {
		cli::cli_abort("Use either {.arg audioFilter} or {.arg filterComplex}, not both.")
	}
	raw_line <- identical(getOption("act.media.timeline", "editlist"), "raw")
	rate     <- .ffmpeg_audio_rate(input)
	filtered <- !is.null(audioFilter) || !is.null(filterComplex)

	args <- c("-hide_banner", "-loglevel", "error",
	          "-ss", .ffmpeg_seconds(max(0, as.numeric(startsec))), "-t", .ffmpeg_seconds(as.numeric(duration)),
	          if (raw_line) .ffmpeg_input_flags(input), "-i", input, "-vn")
	if (!is.null(filterComplex)) {
		args <- c(args, "-filter_complex", filterComplex, "-map", audioMap)
	} else {
		args <- c(args, "-map", "0:a:0")
		if (!is.null(audioFilter)) args <- c(args, "-af", audioFilter)
	}
	codec <- switch(out_ext,
		wav = if (!filtered && .ffmpeg_audio_is_pcm(input)) c("-c:a", "copy")
		      else c("-c:a", .ffmpeg_pcm_codec(input)),
		mp3 = c("-c:a", "libmp3lame", "-b:a", as.character(audioBitrate)),
		m4a = c("-c:a", "aac", "-b:a", as.character(audioBitrate), "-movflags", "+faststart"))
	c(args, codec, if (filtered && is.finite(rate)) c("-ar", as.character(rate)),
	  "-t", .ffmpeg_seconds(as.numeric(duration)), metadataArgs, "-y", output)
}


#' Helper: Run FFmpeg and check the written file
#'
#' Runs FFmpeg with an argument vector (as built by \link{helper_ffmpeg_args})
#' and checks the result at the file: FFmpeg ends with status 0 even when it
#' wrote nothing (e.g. a seek behind the end of the file). A file of the same
#' name is removed first, so an old file never counts as a new result.
#'
#' @param args Character vector; FFmpeg arguments without the program name.
#' @param output Character; path of the file the call is expected to write.
#' @param what Character; short description used in the failure message.
#' @param quiet Logical; if \code{TRUE} no message is printed on failure.
#'
#' @return Logical; \code{TRUE} if the file was written and is not empty.
#'
#' @seealso \link{helper_ffmpeg_args}, \link{helper_ffmpeg_path}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' a <- act::helper_ffmpeg_args(output = "still.jpg", input = "video.mp4", startsec = 12.5)
#' act::helper_ffmpeg_run(a, output = "still.jpg")
#' }
#'
helper_ffmpeg_run <- function(args, output, what = "ffmpeg", quiet = FALSE) {
	if (file.exists(output)) unlink(output)
	dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
	res <- tryCatch(
		processx::run(helper_ffmpeg_path(), args = args, error_on_status = FALSE),
		error = function(e) list(status = -1L, stderr = conditionMessage(e))
	)
	ok <- identical(as.integer(res$status), 0L) && file.exists(output) &&
		isTRUE(file.size(output) > 0)
	if (ok) return(TRUE)
	if (file.exists(output)) unlink(output)
	if (!isTRUE(quiet)) {
		reason <- if (!identical(as.integer(res$status), 0L)) "ffmpeg exited with an error"
			else if (!file.exists(output)) "ffmpeg wrote no output file"
			else "ffmpeg wrote an empty output file"
		err <- stringr::str_trim(res$stderr %||% "")
		message(sprintf("%s failed - %s%s\n  ffmpeg %s%s", what, reason,
			.ffmpeg_seek_hint(args), paste(args, collapse = " "),
			if (nzchar(err)) paste0("\n  ", err) else ""))
	}
	FALSE
}


# ===== INTERNAL HELPERS =====

.ffmpeg_input_flags <- function(path) {
	if (length(path) == 1 && !is.na(path) &&
	    stringr::str_detect(path, stringr::regex("\\.(mp4|m4a|m4v|mov)$", ignore_case = TRUE))) {
		c("-ignore_editlist", "1", "-avoid_negative_ts", "make_zero")
	} else {
		character(0)
	}
}

.ffmpeg_quality_qv <- function(quality) {
	q <- suppressWarnings(as.numeric(quality)[1])
	if (is.na(q)) q <- 100
	q <- min(100, max(1, q))
	as.integer(round(2 + (100 - q) * 29 / 99))
}

.ffmpeg_seconds <- function(x) {
	s <- formatC(x, format = "f", digits = 6)
	s <- sub("0+$", "", s)
	sub("\\.$", "", s)
}

.ffmpeg_scale_filter <- function(maxHeight, maxWidth = NULL) {
	h <- if (is.null(maxHeight)) NA_integer_ else suppressWarnings(as.integer(maxHeight)[1])
	w <- if (is.null(maxWidth))  NA_integer_ else suppressWarnings(as.integer(maxWidth)[1])
	h_ok <- length(h) == 1 && !is.na(h) && h > 0
	w_ok <- length(w) == 1 && !is.na(w) && w > 0
	if (h_ok && w_ok) {
		return(sprintf("scale=w=min(iw\\,%d):h=min(ih\\,%d):force_original_aspect_ratio=decrease:force_divisible_by=2", w, h))
	}
	if (h_ok) return(sprintf("scale=-2:min(ih\\,%d)", h))
	if (w_ok) return(sprintf("scale=min(iw\\,%d):-2", w))
	NULL
}

.ffmpeg_seek_hint <- function(args) {
	tryCatch({
		ii <- which(args == "-i")
		ss <- which(args == "-ss")
		if (!length(ii) || !length(ss) || ss[1] > ii[1]) return("")
		src <- args[[ii[1] + 1L]]
		sk  <- suppressWarnings(as.numeric(args[[ss[1] + 1L]]))
		if (!file.exists(src) || !is.finite(sk)) return("")
		out <- .metadata_ffprobe_run(c("-v", "error", "-show_entries", "format=duration",
			"-of", "csv=p=0", src))$out
		dur <- suppressWarnings(as.numeric(out[1]))
		if (!is.finite(dur)) return("")
		sprintf("\n  seek %.3fs in a file of %.3fs%s", sk, dur,
			if (sk > dur) " - BEHIND THE END (offset wrong?)" else "")
	}, error = function(e) "")
}

.ffmpeg_cmd_line <- function(args, os = c("mac", "win"),
                             executable = getOption("act.path.ffmpeg", "ffmpeg"),
                             inputVariable = NULL) {
	os <- match.arg(os)
	if (!is.null(inputVariable)) args[args == inputVariable] <- "INFILEPATH"
	if (is.null(executable) || !nzchar(executable)) executable <- "ffmpeg"
	simple <- "^[A-Za-z0-9_.:+=,@/-]+$"
	if (os == "mac") {
		q <- ifelse(args == "INFILEPATH", '"$PATH_INPUT"',
			ifelse(stringr::str_detect(args, simple),
				args, shQuote(args, type = "sh")))
		paste(c(paste0('"', executable, '"'), q), collapse = " ")
	} else {
		a <- ifelse(startsWith(args, "/"), gsub("/", "\\", args, fixed = TRUE), args)
		a <- gsub("%", "%%", a, fixed = TRUE)
		q <- ifelse(args == "INFILEPATH", '"%PATH_INPUT%"',
			ifelse(stringr::str_detect(args, simple),
				a, paste0('"', gsub('"', '""', a, fixed = TRUE), '"')))
		paste(c(paste0('"', executable, '"'), q), collapse = " ")
	}
}

.exif_cmd_line <- function(argv, os = c("mac", "win")) {
	os <- match.arg(os)
	if (length(argv) == 0) return(character(0))
	if (os == "mac") {
		paste(c("command -v exiftool >/dev/null 2>&1 && exiftool", shQuote(argv, type = "sh"), "|| :"), collapse = " ")
	} else {
		a <- ifelse(startsWith(argv, "/"), gsub("/", "\\", argv, fixed = TRUE), argv)
		a <- gsub("%", "%%", a, fixed = TRUE)
		paste(c("where exiftool >nul 2>nul && exiftool", paste0('"', gsub('"', '""', a, fixed = TRUE), '"')), collapse = " ")
	}
}

.FFMPEG_TIMING_CACHE <- new.env(parent = emptyenv())

.ffmpeg_audio_stream <- function(path) {
	if (!file.exists(path)) return(list(codec = NA_character_, fmt = NA_character_, bits = NA_real_))
	key <- paste("astream", normalizePath(path, mustWork = FALSE), sep = "|")
	hit <- .FFMPEG_TIMING_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	out <- tryCatch(.metadata_ffprobe_run(c("-v", "error", "-select_streams", "a:0",
		"-show_entries", "stream=codec_name,sample_fmt,bits_per_raw_sample,bits_per_sample",
		"-of", "default=noprint_wrappers=1", path))$out, error = function(e) character(0))
	val <- function(k) { v <- sub(paste0("^", k, "="), "", grep(paste0("^", k, "="), out, value = TRUE)[1]); if (is.na(v)) NA_character_ else v }
	bits <- suppressWarnings(as.numeric(val("bits_per_raw_sample")))
	if (!is.finite(bits)) bits <- suppressWarnings(as.numeric(val("bits_per_sample")))
	res <- list(codec = val("codec_name"), fmt = val("sample_fmt"), bits = bits)
	assign(key, res, envir = .FFMPEG_TIMING_CACHE)
	res
}

.ffmpeg_audio_is_pcm <- function(path) {
	codec <- .ffmpeg_audio_stream(path)$codec
	!is.na(codec) && startsWith(codec, "pcm_")
}

.ffmpeg_pcm_codec <- function(path) {
	st <- .ffmpeg_audio_stream(path)
	if (!is.na(st$codec) && startsWith(st$codec, "pcm_")) {
		if (st$codec %in% c("pcm_s16le", "pcm_s24le", "pcm_s32le", "pcm_f32le", "pcm_f64le")) return(st$codec)
		if (is.finite(st$bits) && st$bits == 24) return("pcm_s24le")
		if (is.finite(st$bits) && st$bits == 32) return("pcm_s32le")
	}
	"pcm_s16le"
}

.ffmpeg_audio_rate <- function(path) {
	if (!file.exists(path)) return(NA_real_)
	key <- paste("rate", normalizePath(path, mustWork = FALSE), sep = "|")
	hit <- .FFMPEG_TIMING_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	out <- tryCatch(.metadata_ffprobe_run(c("-v", "error", "-select_streams", "a:0",
		"-show_entries", "stream=sample_rate", "-of", "csv=p=0", path))$out, error = function(e) character(0))
	rate <- suppressWarnings(as.numeric(out[1]))
	res <- if (length(rate) == 1 && is.finite(rate) && rate > 0) rate else NA_real_
	assign(key, res, envir = .FFMPEG_TIMING_CACHE)
	res
}

.ffmpeg_seek_timing <- function(input, videoOffset = NULL, videoFps = NULL) {
	timeline <- getOption("act.media.timeline", "editlist")
	if (!timeline %in% c("editlist", "raw")) {
		cli::cli_abort("Option {.code act.media.timeline} must be {.val editlist} or {.val raw}, not {.val {timeline}}.")
	}
	offset <- suppressWarnings(as.numeric(videoOffset)[1])
	fps    <- suppressWarnings(as.numeric(videoFps)[1])
	if ((length(offset) == 0 || !is.finite(offset) || length(fps) == 0 || !is.finite(fps) || fps <= 0) &&
	    file.exists(input)) {
		from_file <- .ffmpeg_video_timing(input)
		if (length(offset) == 0 || !is.finite(offset)) offset <- from_file$offset
		if (length(fps) == 0 || !is.finite(fps) || fps <= 0) fps <- 1 / from_file$frame_dur
	}
	if (length(offset) == 0 || !is.finite(offset)) offset <- 0
	list(offset    = if (identical(timeline, "raw")) 0 else offset,
	     frame_dur = if (length(fps) == 1 && is.finite(fps) && fps > 0) 1 / fps else NA_real_)
}

.ffmpeg_video_timing <- function(path) {
	info <- file.info(path)
	if (is.na(info$size)) return(list(offset = 0, frame_dur = NA_real_))
	key <- paste(normalizePath(path, mustWork = FALSE), info$size, as.numeric(info$mtime), sep = "|")
	hit <- .FFMPEG_TIMING_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	offset <- tryCatch(.mp4_video_editlist_offset(path), error = function(e) 0)
	fps <- tryCatch(.metadata_probe_fps(path), error = function(e) NA_real_)
	res <- list(offset = if (is.finite(offset)) offset else 0,
	            frame_dur = if (is.finite(fps) && fps > 0) 1 / fps else NA_real_)
	assign(key, res, envir = .FFMPEG_TIMING_CACHE)
	res
}

.mp4_video_editlist_offset <- function(path) {
	.mp4_editlist_offset(path, handler = "vide")
}

.mp4_editlist_offset <- function(path, handler = c("vide", "soun")) {
	handler <- match.arg(handler)
	con <- file(path, "rb")
	on.exit(close(con), add = TRUE)
	file_size <- file.info(path)$size
	pos <- 0
	moov <- NULL
	while (pos + 8 <= file_size) {
		seek(con, where = pos)
		h <- .mp4_box_header(con)
		if (is.null(h)) break
		if (h$size <= 0) h$size <- file_size - pos
		if (identical(h$type, "moov")) {
			seek(con, where = pos + h$header)
			moov <- readBin(con, "raw", n = h$size - h$header)
			break
		}
		pos <- pos + h$size
	}
	if (is.null(moov)) return(0)
	movie_ts <- NA_real_
	for (b in .mp4_children(moov)) {
		if (b$type == "mvhd") movie_ts <- .mp4_mvhd_timescale(b$data)
	}
	for (trak in Filter(function(b) b$type == "trak", .mp4_children(moov))) {
		kids <- .mp4_children(trak$data)
		mdia <- Filter(function(b) b$type == "mdia", kids)
		if (!length(mdia)) next
		mkids <- .mp4_children(mdia[[1]]$data)
		hdlr <- Filter(function(b) b$type == "hdlr", mkids)
		if (!length(hdlr) || rawToChar(hdlr[[1]]$data[9:12]) != handler) next
		mdhd <- Filter(function(b) b$type == "mdhd", mkids)
		media_ts <- .mp4_mdhd_timescale(mdhd[[1]]$data)
		edts <- Filter(function(b) b$type == "edts", kids)
		if (!length(edts)) return(0)
		elst <- Filter(function(b) b$type == "elst", .mp4_children(edts[[1]]$data))
		if (!length(elst)) return(0)
		e <- .mp4_elst_entries(elst[[1]]$data)
		if (!nrow(e)) return(0)
		empty <- e$media_time < 0
		lead <- cumprod(empty) == 1
		delay <- sum(e$segment_duration[lead]) / movie_ts
		first <- which(!empty)[1]
		media_time <- if (is.na(first)) 0 else e$media_time[first] / media_ts
		return(media_time - delay)
	}
	0
}

.mp4_u32 <- function(r) sum(as.numeric(as.integer(r)) * 256^(3:0))
.mp4_u64 <- function(r) .mp4_u32(r[1:4]) * 2^32 + .mp4_u32(r[5:8])
.mp4_s32 <- function(r) { v <- .mp4_u32(r); if (v >= 2^31) v - 2^32 else v }
.mp4_s64 <- function(r) { hi <- .mp4_u32(r[1:4]); v <- hi * 2^32 + .mp4_u32(r[5:8]); if (hi >= 2^31) v - 2^64 else v }

.mp4_box_header <- function(con) {
	r <- readBin(con, "raw", n = 8)
	if (length(r) < 8) return(NULL)
	size <- .mp4_u32(r[1:4])
	type <- rawToChar(r[5:8])
	header <- 8
	if (size == 1) {
		size <- .mp4_u64(readBin(con, "raw", n = 8))
		header <- 16
	}
	list(size = size, type = type, header = header)
}

.mp4_children <- function(data) {
	out <- list()
	pos <- 1
	n <- length(data)
	while (pos + 7 <= n) {
		size <- .mp4_u32(data[pos:(pos + 3)])
		type <- rawToChar(data[(pos + 4):(pos + 7)])
		header <- 8
		if (size == 1) { size <- .mp4_u64(data[(pos + 8):(pos + 15)]); header <- 16 }
		if (size == 0) size <- n - pos + 1
		if (size < header || pos + size - 1 > n) break
		out[[length(out) + 1]] <- list(type = type,
			data = if (size > header) data[(pos + header):(pos + size - 1)] else raw(0))
		pos <- pos + size
	}
	out
}

.mp4_mvhd_timescale <- function(d) {
	if (as.integer(d[1]) == 1) .mp4_u32(d[21:24]) else .mp4_u32(d[13:16])
}

.mp4_mdhd_timescale <- function(d) {
	if (as.integer(d[1]) == 1) .mp4_u32(d[21:24]) else .mp4_u32(d[13:16])
}

.mp4_elst_entries <- function(d) {
	version <- as.integer(d[1])
	n <- .mp4_u32(d[5:8])
	step <- if (version == 1) 20 else 12
	rows <- lapply(seq_len(n), function(i) {
		p <- 9 + (i - 1) * step
		if (version == 1) {
			c(segment_duration = .mp4_u64(d[p:(p + 7)]), media_time = .mp4_s64(d[(p + 8):(p + 15)]))
		} else {
			c(segment_duration = .mp4_u32(d[p:(p + 3)]), media_time = .mp4_s32(d[(p + 4):(p + 7)]))
		}
	})
	if (!length(rows)) return(data.frame(segment_duration = numeric(0), media_time = numeric(0)))
	as.data.frame(do.call(rbind, rows))
}
