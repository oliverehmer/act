#' Helper: Export cured transcripts
#'
#' Exports only those transcripts from a corpus that were modified by
#' \link{transcripts_cure_single} (either during import or via an explicit
#' cure call). A transcript is considered cured if its most recent history
#' entry of type \code{transcripts_cure_single} contains at least one
#' non-zero correction counter.
#'
#' All exported files are written flat into \code{folderOutput}, without
#' sub-folders.
#'
#' @param x Corpus object.
#' @param folderOutput Character string; path to the output folder.
#' @param formats Character vector; annotation formats to export. Defaults to \code{"textgrid"}. See \link{corpus_export} for supported formats.
#'
#' @return Invisibly, a character vector with the names of the exported transcripts.
#'
#' @seealso \link{transcripts_cure_single}, \link{corpus_export}
#'
#' @export
#'
helper_corpus_export_cured <- function(x,
									   folderOutput,
									   formats = "textgrid") {

	.assert_corpus(x, missing = missing(x))
	if (missing(folderOutput) || is.null(folderOutput) || !nzchar(folderOutput)) {
		cli::cli_abort("Parameter {.arg folderOutput} is required.")
	}

	# identify cured transcripts
	cured.names <- character()
	for (tname in names(x@transcripts)) {
		tr <- x@transcripts[[tname]]
		if (length(tr@history) == 0L) next
		h <- tr@history[[length(tr@history)]]
		if (!identical(h$modification, "transcripts_cure_single")) next
		was.cured <- (
			isTRUE(h$annotationsTimesReversed.deleted.count > 0)
			|| isTRUE(h$annotationsTimesBelowZero.deleted.count > 0)
			|| isTRUE(h$annotationsTimesBelowZero.corrected.count > 0)
			|| isTRUE(h$annotationsOverlap.corrected.count > 0)
			|| isTRUE(h$transcriptLengthZero.corrected)
			|| isTRUE(h$annotationsZeroDuration.merged.count > 0)
			|| isTRUE(h$annotationsZeroDuration.extended.count > 0)
			|| isTRUE(h$annotationsZeroDuration.deleted.count > 0)
			|| isTRUE(h$tiersMissing.added.count > 0)
		)
		if (was.cured) {
			cured.names <- c(cured.names, tname)
		}
	}

	if (length(cured.names) == 0L) {
		cli::cli_alert_info("No cured transcripts found in corpus.")
		return(invisible(character()))
	}

	if (!dir.exists(folderOutput)) {
		dir.create(folderOutput, recursive = TRUE)
	}

	cli::cli_alert_info("Exporting {length(cured.names)} cured transcript(s) to {.path {folderOutput}}")

	act::corpus_export(
		x                     = x,
		folderOutput          = folderOutput,
		filterTranscriptNames = cured.names,
		filterTierNames       = NULL,
		formats               = formats,
		createMediaLinks      = FALSE,
		createFolderOutput    = FALSE,
		l                     = NULL
	)

	invisible(cured.names)
}
