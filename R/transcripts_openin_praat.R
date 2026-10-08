#' Open transcript in 'Praat'
#'
#' The function opens a transcript in the 'Praat' TextGrid editor together with
#' its sound and selects a time range. 'Praat' is remote controlled with a
#' 'Praat' script.
#'
#' It can open the original .TextGrid file (if the transcript was read from one
#' and it still exists) or write a .TextGrid file. A TextGrid written by this
#' function replaces an object of the same name that is already open in 'Praat',
#' so the editor always shows the current transcript. An original file is never
#' replaced in 'Praat': the open object may hold edits that are not saved yet.
#'
#' 'Praat' receives the script through its own command line option \code{--send}
#' (from 'Praat' 6.1 on): set the path to 'Praat' with 'options(act.path.praat = ...)'.
#' If 'Praat' is not running, it is started. Only if the path to 'Praat' is not set,
#' 'sendpraat' is used ('options(act.path.sendpraat = ...)', see \code{vignette("installation-sendpraat")}).
#' 'Praat' 7 runs a 'sendpraat' message late, so the path to 'Praat' is the better choice.
#'
#' @param t Transcript object.
#' @param openOriginal Logical; if \code{TRUE} the original .TextGrid file is opened if the transcript was read from one and it still exists. Otherwise a .TextGrid file is written.
#' @param filePathOut Character string, optional. Where the .TextGrid file is written if the original is not used. If \code{NULL} a temporary file is created.
#' @param startSec Double, optional; start of the selection in 'Praat'. If \code{NULL} the selection starts at 0.
#' @param endSec Double, optional; end of the selection in 'Praat'. If \code{NULL} the selection ends at the end of the transcript.
#' @param play Logical; if \code{TRUE} the selection is played.
#' @param filterMediaFile Vector of character strings; each element is a regular expression. They are checked one after the other; the first existing media file that matches is opened as sound. The default order is uncompressed audio > compressed audio.
#' @param delay Double; time in seconds to wait before the script is handed to 'Praat'.
#'
#' @return Logical; \code{TRUE} if the script was handed to 'Praat' (invisibly).
#' @seealso \link{transcripts_openin_elan}, \link{search_openresult_inpraat}
#'
#' @export
#'
#' @examples
#' library(act)
#'
#' # You can only use this function if you have installed 'Praat'
#' # and located it properly in the package options.
#' \dontrun{
#' act::transcripts_openin_praat(t = examplecorpus@transcripts[[1]], startSec = 1, endSec = 3)
#' }
transcripts_openin_praat <- function(t,
									 openOriginal    = FALSE,
									 filePathOut     = NULL,
									 startSec        = NULL,
									 endSec          = NULL,
									 play            = FALSE,
									 filterMediaFile = c('(?i).*\\.(aiff|aif|wav)$', '(?i).*\\.mp3$'),
									 delay           = 0.5) {

	.assert_transcript(t, missing = missing(t))

	#--- TextGrid: the original if wanted and present, else written
	path_textgrid <- ""
	if (isTRUE(openOriginal) && length(t@file.path) == 1 && !is.na(t@file.path) &&
	    stringr::str_to_lower(tools::file_ext(t@file.path)) == "textgrid" && file.exists(t@file.path)) {
		path_textgrid <- t@file.path
	}
	reload <- FALSE
	if (path_textgrid == "") {
		if (!is.null(filePathOut)) {
			if (!dir.exists(dirname(filePathOut))) {
				cli::cli_abort("Destination folder does not exist: {.path {dirname(filePathOut)}}")
			}
			path_textgrid <- filePathOut
		} else {
			path_textgrid <- file.path(tempdir(), stringr::str_c(t@name, ".TextGrid", collapse = ""))
		}
		act::export_textgrid(t, path_textgrid)
		reload <- TRUE
		if (isTRUE(openOriginal)) {
			cli::cli_inform("Original .TextGrid file has not been found. A .TextGrid file has been created.")
		}
	}

	#--- sound
	path_longsound <- if (nrow(t@media) > 0) media_path_to_existing_file(t, filterMediaFile = filterMediaFile) else NULL
	if (is.null(path_longsound)) {
		path_longsound <- ""
		cli::cli_warn("No sound file found - the TextGrid is opened without sound.")
	}

	#--- selection
	if (is.null(startSec) || is.na(startSec)) startSec <- 0
	if (is.null(endSec) || is.na(endSec)) endSec <- if (length(t@length.sec) == 1 && t@length.sec > 0) t@length.sec else startSec

	invisible(.praat_open_selection(pathTextGrid  = path_textgrid,
	                                pathLongSound = path_longsound,
	                                startSec      = startSec,
	                                endSec        = endSec,
	                                play          = play,
	                                close         = FALSE,
	                                reload        = reload,
	                                delay         = delay))
}
