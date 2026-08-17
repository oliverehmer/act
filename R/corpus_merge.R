#' Merge corpora into a target corpus
#'
#' Combines one or several source corpora into a target corpus.
#' The target corpus (\code{x}) is the winner for all configuration slots
#' (\code{normalization.matrix}, \code{import.*}). Source transcripts are added
#' to the target's transcript list.
#'
#' If a source corpus has a different \code{normalization.matrix} than the
#' target, its transcripts get their \code{normalization.signature} and
#' \code{fulltext.signature} invalidated, so that the next call to
#' \code{transcripts_update_normalization()} and
#' \code{transcripts_update_fulltexts()} will re-normalize and re-create the
#' fulltexts using the target's matrix.
#'
#' @param x Corpus object; the target corpus that receives the merged content.
#' @param insert Either a corpus object or a list of corpus objects to merge into \code{x}.
#' @param conflictTranscripts Character; how to handle name conflicts between target and source transcripts. One of \code{"error"} (abort, default), \code{"skip"} (keep target, ignore source), \code{"overwrite"} (replace target with source).
#'
#' @return Corpus object.
#'
#' @export
corpus_merge <- function(x,
                         insert,
                         conflictTranscripts = c("error", "skip", "overwrite")) {

	conflictTranscripts <- match.arg(conflictTranscripts)

	.assert_corpus(x, missing = missing(x))
	if (missing(insert))                       { cli::cli_abort("Parameter {.arg insert} is missing.") }

	if (methods::is(insert, "corpus")) {
		insert_list <- list(insert)
	} else if (is.list(insert) && length(insert) > 0L &&
	           all(vapply(insert, function(s) methods::is(s, "corpus"), logical(1)))) {
		insert_list <- insert
	} else {
		cli::cli_abort("Parameter {.arg insert} must be a {.cls corpus} object or a list of {.cls corpus} objects.")
	}

	transcripts_added_total <- 0L
	transcripts_skipped     <- character()
	transcripts_overwritten <- character()

	for (src in insert_list) {
		# Compare matrices by content (identical), not by serialization hash:
		# digest() can produce different hashes for semantically equal
		# data.frames depending on attributes / column order, which would
		# falsely invalidate signatures.
		matrix_diff <- !identical(x@normalization.matrix, src@normalization.matrix)

		src_names <- names(src@transcripts)
		tgt_names <- names(x@transcripts)
		conflicts <- intersect(src_names, tgt_names)

		if (length(conflicts) > 0) {
			if (conflictTranscripts == "error") {
				cli::cli_abort("Transcript name conflict during {.fn corpus_merge}: {.val {conflicts}}. Use {.code conflictTranscripts = \"skip\"} or {.code \"overwrite\"} to resolve.")
			} else if (conflictTranscripts == "skip") {
				src_names <- setdiff(src_names, conflicts)
				transcripts_skipped <- c(transcripts_skipped, conflicts)
			} else if (conflictTranscripts == "overwrite") {
				transcripts_overwritten <- c(transcripts_overwritten, conflicts)
			}
		}

		for (n in src_names) {
			tr <- src@transcripts[[n]]
			if (matrix_diff) {
				tr@normalization.signature <- character(0)
				tr@fulltext.signature      <- character(0)
			}
			x@transcripts[[n]] <- tr
		}
		transcripts_added_total <- transcripts_added_total + length(src_names)

		x@paths.annotation.files <- unique(c(x@paths.annotation.files, src@paths.annotation.files))
		x@paths.media.files      <- unique(c(x@paths.media.files, src@paths.media.files))

		if (nrow(src@import.results) > 0) {
			if (nrow(x@import.results) == 0) {
				x@import.results <- src@import.results
			} else {
				x@import.results <- rbind(x@import.results, src@import.results)
			}
		}
		if (nrow(src@media.assign.results) > 0) {
			if (nrow(x@media.assign.results) == 0) {
				x@media.assign.results <- src@media.assign.results
			} else {
				x@media.assign.results <- rbind(x@media.assign.results, src@media.assign.results)
			}
		}

		x@history <- c(x@history, src@history)
	}

	x@history[[length(x@history) + 1]] <- list(
		modification             = "corpus_merge",
		systime                  = Sys.time(),
		sources.count            = length(insert_list),
		transcripts.added.count  = transcripts_added_total,
		transcripts.skipped      = transcripts_skipped,
		transcripts.overwritten  = transcripts_overwritten,
		conflictTranscripts      = conflictTranscripts
	)

	return(x)
}
