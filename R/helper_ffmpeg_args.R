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
#'   that moment is taken (not the next one). Camera files with B-frames
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
                               maxHeight     = NULL) {
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
			if (length(.ffmpeg_input_flags(input)) && file.exists(input)) {
				timing <- .ffmpeg_video_timing(input)
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

.ffmpeg_scale_filter <- function(maxHeight) {
	h <- suppressWarnings(as.integer(maxHeight)[1])
	if (is.null(maxHeight) || length(h) == 0 || is.na(h) || h <= 0) return(NULL)
	sprintf("scale=-2:min(ih\\,%d)", h)
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
		if (!length(hdlr) || rawToChar(hdlr[[1]]$data[9:12]) != "vide") next
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
