#' Reveal a file in the file manager of the operating system
#'
#' Opens the folder of the file and selects the file: Finder on macOS,
#' Explorer on Windows. On Linux only the folder is opened (there is no
#' common way to select a file).
#'
#' @param path Character string; path to the file (or folder).
#'
#' @return Logical; \code{TRUE} if the command was issued, \code{FALSE} if
#'   the path does not exist (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' helper_file_reveal("/path/to/file.eaf")
#' }
helper_file_reveal <- function(path) {
	if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return(invisible(FALSE))
	path <- normalizePath(path, mustWork = FALSE)
	if (!file.exists(path)) return(invisible(FALSE))
	os <- .detect_os()
	if (os == "macos") {
		system2("open", c("-R", shQuote(path)), wait = FALSE)
	} else if (os == "windows") {
		# explorer wants backslashes; /select, selects the file
		win <- gsub("/", "\\\\", path, fixed = TRUE)
		system2("explorer.exe", paste0("/select,", shQuote(win)), wait = FALSE)
	} else {
		folder <- if (dir.exists(path)) path else dirname(path)
		system2("xdg-open", shQuote(folder), wait = FALSE, stdout = FALSE, stderr = FALSE)
	}
	invisible(TRUE)
}

#' Open a file in ELAN or Praat
#'
#' Starts the application with the file. The location of the application
#' comes from the options \code{act.path.elan} and \code{act.path.praat}
#' unless \code{pathApp} is given. ELAN opens \code{.eaf} files, Praat
#' opens \code{.TextGrid} files; the format is not checked here.
#'
#' @param path Character string; path to the annotation file.
#' @param app Character string; \code{"elan"} or \code{"praat"}.
#' @param pathApp Character string or \code{NULL}; path to the application.
#'   If \code{NULL} the corresponding option is used.
#'
#' @return Logical; \code{TRUE} if the command was issued (invisibly).
#'   Aborts with a message if the file or the application is not found.
#' @export
#'
#' @examples
#' \dontrun{
#' helper_file_open_in("/path/to/file.eaf", app = "elan")
#' helper_file_open_in("/path/to/file.TextGrid", app = "praat")
#' }
helper_file_open_in <- function(path, app = c("elan", "praat"), pathApp = NULL) {
	app <- match.arg(app)
	if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path) || !file.exists(path))
		cli::cli_abort("File not found: {.path {path}}")
	if (is.null(pathApp)) {
		pathApp <- getOption(if (app == "elan") "act.path.elan" else "act.path.praat", default = "")
	}
	if (is.null(pathApp) || !nzchar(pathApp) || !file.exists(pathApp))
		cli::cli_abort("{toupper(substr(app, 1, 1))}{substr(app, 2, nchar(app))} not found. Set the option {.arg act.path.{app}} to the application.")
	path <- normalizePath(path, mustWork = TRUE)
	if (.detect_os() == "macos") {
		system2("open", c("-a", shQuote(pathApp), shQuote(path)), wait = FALSE)
	} else {
		system2(pathApp, shQuote(path), wait = FALSE, stdout = FALSE, stderr = FALSE)
	}
	invisible(TRUE)
}

#' Check whether an external application is available
#'
#' @param app Character string; \code{"elan"} or \code{"praat"}.
#'
#' @return Logical; \code{TRUE} if the option \code{act.path.elan} /
#'   \code{act.path.praat} points to an existing file.
#' @export
helper_file_app_available <- function(app = c("elan", "praat")) {
	app <- match.arg(app)
	p <- getOption(if (app == "elan") "act.path.elan" else "act.path.praat", default = "")
	!is.null(p) && length(p) == 1 && !is.na(p) && nzchar(p) && file.exists(p)
}
