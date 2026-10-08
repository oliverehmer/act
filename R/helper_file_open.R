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


# ===== PRAAT: OPEN A TEXTGRID WITH ITS SOUND AND A SELECTION =====
# Shared by transcripts_openin_praat() and search_openresult_inpraat(): writes
# the Praat script with the values filled in and runs it through sendpraat;
# starts Praat when it does not answer yet.
#   reload  TRUE removes a TextGrid object of the same name before reading the
#           file: a temporary copy changes between two calls, and Praat would
#           otherwise keep showing the old object. Never for an original file -
#           the open object may hold edits not yet saved in Praat.
.praat_open_selection <- function(pathTextGrid, pathLongSound, startSec, endSec,
								  play = FALSE, close = FALSE, reload = FALSE, delay = 0.5) {
	praat     <- .praat_binary(getOption("act.path.praat"))
	sendpraat <- getOption("act.path.sendpraat")
	if (is.null(sendpraat) || !nzchar(sendpraat) || !file.exists(sendpraat)) sendpraat <- ""
	if (praat == "" && sendpraat == "") {
		cli::cli_abort("Neither Praat nor sendpraat found. Please indicate the location of Praat in 'options(act.path.praat = ...)'.")
	}
	if (is.null(pathLongSound)) pathLongSound <- ""
	pathTextGrid <- normalizePath(pathTextGrid, winslash = "/", mustWork = FALSE)
	if (nzchar(pathLongSound)) pathLongSound <- normalizePath(pathLongSound, winslash = "/", mustWork = FALSE)

	praatScriptPath <- file.path(system.file("extdata", "praat", package = "act"), "OpenSelectionInPraat.praat")
	tx <- readLines(con = praatScriptPath, n = -1, warn = FALSE, encoding = "UTF-8")
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "PATHTEXTGRID",   replacement = pathTextGrid)
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "PATHLONGSOUND",  replacement = pathLongSound)
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "SELSTARTSEC",    replacement = as.character(startSec))
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "SELENDSEC",      replacement = as.character(endSec))
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "PLAYSELECTION",  replacement = if (isTRUE(play)) "1" else "0")
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "RELOADTEXTGRID", replacement = if (isTRUE(reload)) "1" else "0")
	tx <- stringi::stri_replace_all_fixed(str = tx, pattern = "CLOSEEDITOR",    replacement = if (isTRUE(close)) "1" else "0")

	tempScriptPath <- tempfile(pattern = "openselection", tmpdir = tempdir(), fileext = ".praat")
	tempScriptCon <- file(tempScriptPath, open = "wb")
	writeLines(enc2utf8(tx), con = tempScriptCon, sep = "\n", useBytes = TRUE)
	close(tempScriptCon)
	tempScriptPath <- normalizePath(tempScriptPath, winslash = "/", mustWork = FALSE)

	# Praat 7 runs a script sent by sendpraat only when a later message arrives
	# (measured 08.10.2026 with Praat 7.0.02); its own --send runs it at once.
	# No waiting: without a running Praat, --send becomes the Praat that stays
	# open. The script stays until tempdir() is cleaned, as Praat reads it later.
	Sys.sleep(delay)
	if (praat != "") {
		system2(praat, c("--send", shQuote(tempScriptPath)), stdout = FALSE, stderr = FALSE, wait = FALSE)
		return(invisible(TRUE))
	}
	cmd  <- sprintf("%s praat \"runScript: \\\"%s\\\"\"", shQuote(sendpraat), tempScriptPath)
	rslt <- system(cmd, intern = FALSE, ignore.stderr = TRUE, ignore.stdout = TRUE, wait = TRUE)
	invisible(rslt == 0)
}

# The Praat executable: on macOS the binary inside Praat.app, "" if not found.
.praat_binary <- function(path) {
	if (is.null(path) || !nzchar(path)) return("")
	if (stringr::str_detect(path, "(?i)\\.app/?$")) path <- file.path(sub("/$", "", path), "Contents", "MacOS", "Praat")
	if (!file.exists(path) || dir.exists(path)) return("")
	path
}
