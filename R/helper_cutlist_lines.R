#' Write the lines of a terminal script (cut list)
#'
#' One writer for every line of the render scripts that act and iclo save
#' with \link{helper_cutlist_save}: ffmpeg and exiftool calls, folders,
#' deletions, comments, messages and calls of other scripts. The caller
#' collects the parts of each line (for ffmpeg the argument vector of
#' \link{helper_ffmpeg_args}, \link{helper_ffmpeg_args_clip} or
#' \link{helper_ffmpeg_args_audio}); this function quotes them for the shell
#' of the target system. Arguments that need no quoting stay as they are.
#'
#' Entry types (\code{type}) and their \code{args}:
#' \itemize{
#'   \item \code{"ffmpeg"}: the ffmpeg arguments (without the executable).
#'   \item \code{"exiftool"}: the exiftool arguments, as from
#'     \link{helper_metadata_exif_argv}; the call is skipped when exiftool is
#'     not installed.
#'   \item \code{"mkdir"}: folders to create (with parents).
#'   \item \code{"rm"}: files to delete (no error when missing).
#'   \item \code{"comment"}: one text, written as a comment line.
#'   \item \code{"echo"}: one text, printed when the script runs.
#'   \item \code{"script"}: one script file to run.
#'   \item \code{"blank"}: an empty line.
#' }
#'
#' On Windows paths starting with \code{/} are written with backslashes and
#' \code{\%} is doubled.
#'
#' @param entries List of entries, each a list with \code{type} and
#'   \code{args} (character vector).
#' @param os Character; \code{"mac"} (POSIX sh, also Linux) or \code{"win"}
#'   (cmd).
#' @param executable Character; how the script calls ffmpeg. Default is the
#'   option \code{act.cutlist.ffmpeg} (\code{"ffmpeg"}, found via the PATH of
#'   the machine that runs the script).
#' @param inputVariable Character or \code{NULL}; an ffmpeg argument equal to
#'   this value is written as the script variable of the input file
#'   (\code{$PATH_INPUT} / \code{\%PATH_INPUT\%}).
#' @param header Logical; \code{TRUE} starts a Windows script with
#'   \code{@echo off} and the UTF-8 code page. No effect for \code{"mac"}.
#'
#' @return Character vector with the script lines.
#'
#' @seealso \link{helper_cutlist_save}
#'
#' @export
#'
#' @examples
#' act::helper_cutlist_lines(list(
#'   list(type = "comment", args = "==== clip 1 ===="),
#'   list(type = "mkdir", args = "/tmp/my clips"),
#'   list(type = "ffmpeg", args = c("-i", "/tmp/in.mp4", "-t", "2", "-y", "/tmp/my clips/out.mp4"))),
#'   os = "mac")
helper_cutlist_lines <- function(entries,
                                 os            = c("mac", "win"),
                                 executable    = getOption("act.cutlist.ffmpeg", "ffmpeg"),
                                 inputVariable = NULL,
                                 header        = FALSE) {
	os <- match.arg(os)
	out <- if (os == "win" && isTRUE(header)) c("@echo off", "chcp 65001 >nul") else character(0)
	for (e in entries) {
		a <- as.character(e$args %||% character(0))
		line <- switch(e$type,
			ffmpeg   = .ffmpeg_cmd_line(a, os, executable = executable, inputVariable = inputVariable),
			exiftool = .exif_cmd_line(a, os),
			mkdir    = if (os == "mac") paste("mkdir -p", paste(.cutlist_quote(a, "mac"), collapse = " "))
			           else vapply(.cutlist_quote(a, "win"), function(q) sprintf("if not exist %s mkdir %s", q, q), ""),
			rm       = if (!length(a)) character(0)
			           else if (os == "mac") paste("rm -f", paste(.cutlist_quote(a, "mac"), collapse = " "))
			           else paste("del /f /q", paste(.cutlist_quote(a, "win"), collapse = " "), "2>nul"),
			comment  = if (os == "mac") paste0("#", a[1]) else paste0("REM ", a[1]),
			echo     = if (os == "mac") paste("echo", shQuote(a[1], type = "sh")) else paste("echo", .cutlist_win_path(a[1])),
			script   = if (os == "mac") paste("sh", .cutlist_quote(a[1], "mac"))
			           else paste0("call \"", .cutlist_win_path(a[1]), "\""),
			blank    = "",
			cli::cli_abort("Unknown cut list entry type {.val {e$type}}."))
		out <- c(out, unname(line))
	}
	out
}
