#' Example corpus with its media files
#'
#' Returns the example corpus (\link{examplecorpus}) with its media files
#' linked. The media (about 64 MB of wav and mp4 files) are not part of the
#' package: at the first call they are downloaded from GitHub (option
#' \code{act.examplecorpusURL}) into a folder of act on this computer
#' (\code{tools::R_user_dir("act", "data")}); later calls use them from
#' there without a download.
#'
#' @param folder Character string; a folder that holds the media files of the
#'   example corpus (e.g. a download of your own). \code{NULL} (default): the
#'   folder of act on this computer, downloaded when it is empty.
#' @param download Logical; if \code{FALSE}, nothing is downloaded (the media
#'   that are found are linked).
#' @param verbose Logical; if \code{TRUE}, progress is shown.
#'
#' @return Corpus object: the example corpus with its media files.
#' @export
#'
#' @examples
#' \dontrun{
#' library(act)
#' x <- act::examplecorpus_media()
#' x@transcripts[[1]]@media
#' }
examplecorpus_media <- function(folder   = NULL,
								download = TRUE,
								verbose  = TRUE) {
	if (is.null(folder)) folder <- file.path(tools::R_user_dir("act", "data"), "examplecorpus")
	media_regex <- "\\.(wav|mp4|mp3|aif|aiff|mov)$"
	found <- if (dir.exists(folder)) list.files(folder, pattern = media_regex, recursive = TRUE, ignore.case = TRUE) else character(0)
	if (!length(found) && isTRUE(download)) {
		url <- getOption("act.examplecorpusURL")
		if (is.null(url) || !nzchar(url)) cli::cli_abort("No download address: option {.code act.examplecorpusURL} is not set.")
		if (verbose) cli::cli_alert_info("Downloading the media of the example corpus (about 64 MB) to {.path {folder}} ...")
		dir.create(folder, recursive = TRUE, showWarnings = FALSE)
		zip <- tempfile(fileext = ".zip")
		on.exit(unlink(zip), add = TRUE)
		ok <- tryCatch(utils::download.file(url, zip, mode = "wb", quiet = !verbose) == 0, error = function(e) FALSE)
		if (!isTRUE(ok)) cli::cli_abort("The media of the example corpus could not be downloaded from {.url {url}}.")
		utils::unzip(zip, exdir = folder)
		found <- list.files(folder, pattern = media_regex, recursive = TRUE, ignore.case = TRUE)
	}
	if (!length(found)) {
		cli::cli_abort("No media files in {.path {folder}}.")
	}
	x <- act::examplecorpus
	x <- act::media_assign(x,
						   searchPaths         = folder,
						   searchSubfolders    = TRUE,
						   filterFile          = "",
						   namesExtractPattern = "",
						   transcriptNames     = NULL,
						   deleteExistingMedia = TRUE,
						   onlyUniqueFiles     = TRUE,
						   readMetadata        = FALSE)
	if (verbose) cli::cli_alert_success("Example corpus with {length(found)} media file{?s} from {.path {folder}}")
	x
}
