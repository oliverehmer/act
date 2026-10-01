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
#' @param startsec Numeric or \code{NULL}; seek position in seconds.
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
