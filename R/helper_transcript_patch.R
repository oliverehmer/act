#' Helper: Create a patch between two versions of a transcript
#'
#' Compares two versions of the same transcript object and records what changed
#' between them: the annotation rows that were removed and added, the tier table,
#' the media table and the length. The result can be applied in both directions
#' with \link{helper_transcript_patch_apply}, which makes it suitable for undo and
#' redo of arbitrary modifications.
#'
#' Annotation rows are compared over all their columns. If \code{tierNames} is
#' given, only the annotations of these tiers are compared; the annotations of all
#' other tiers are expected to be unchanged. Derived slots (the \code{fulltext.*}
#' slots and \code{@history}) are not part of the patch.
#'
#' @param before Transcript object; the version before the modification.
#' @param after Transcript object; the version after the modification.
#' @param tierNames Vector of character strings; names of the tiers whose
#'   annotations are compared. If left unspecified, all annotations are compared.
#'
#' @return List with the elements \code{annotations}, \code{tiers}, \code{media},
#'   \code{length.sec} (each \code{NULL} if unchanged) and \code{empty}
#'   (\code{TRUE} if nothing changed).
#'
#' @seealso \link{helper_transcript_patch_apply}, \link{helper_transcripts_compare}
#'
#' @export
#'
#' @example inst/examples/helper_transcript_patch_make.R
#'
helper_transcript_patch_make <- function(before, after, tierNames = NULL) {
	.assert_transcript(before, arg = "before", missing = missing(before))
	.assert_transcript(after, arg = "after", missing = missing(after))

	patch <- list(annotations = NULL, tiers = NULL, media = NULL, length.sec = NULL)

	a_before <- before@annotations
	a_after  <- after@annotations
	if (!identical(names(a_before), names(a_after))) {
		patch$annotations <- list(replace = TRUE, before = a_before, after = a_after)
	} else {
		rows_before <- seq_len(nrow(a_before))
		rows_after  <- seq_len(nrow(a_after))
		if (!is.null(tierNames)) {
			rows_before <- which(a_before$tierName %in% tierNames)
			rows_after  <- which(a_after$tierName %in% tierNames)
		}
		occ_before <- .patch_occurrence(.patch_row_keys(a_before[rows_before, , drop = FALSE]))
		occ_after  <- .patch_occurrence(.patch_row_keys(a_after[rows_after, , drop = FALSE]))
		removed_index <- rows_before[!(occ_before %in% occ_after)]
		added_index   <- rows_after[!(occ_after %in% occ_before)]
		if (length(removed_index) || length(added_index)) {
			patch$annotations <- list(
				replace       = FALSE,
				removed       = a_before[removed_index, , drop = FALSE],
				removed.index = removed_index,
				added         = a_after[added_index, , drop = FALSE],
				added.index   = added_index)
		}
	}

	if (!.patch_tables_equal(before@tiers, after@tiers)) {
		patch$tiers <- list(before = before@tiers, after = after@tiers)
	}
	if (!.patch_tables_equal(before@media, after@media)) {
		patch$media <- list(before = before@media, after = after@media)
	}
	if (!isTRUE(all.equal(before@length.sec, after@length.sec))) {
		patch$length.sec <- list(before = before@length.sec, after = after@length.sec)
	}

	patch$empty <- is.null(patch$annotations) && is.null(patch$tiers) &&
		is.null(patch$media) && is.null(patch$length.sec)
	patch
}

#' Helper: Apply a patch to a transcript
#'
#' Applies a patch created by \link{helper_transcript_patch_make} to a transcript
#' object. \code{"backward"} turns the version after the modification into the
#' version before it (undo), \code{"forward"} does the opposite (redo).
#'
#' Before anything is changed, the function checks that the transcript is in the
#' state the patch expects (the rows to be removed exist, the tier and media
#' tables match). If not, it stops with an error and the transcript is left
#' untouched.
#'
#' @param x Transcript object.
#' @param patch List; a patch created by \link{helper_transcript_patch_make}.
#' @param direction Character string; \code{"backward"} (undo) or
#'   \code{"forward"} (redo).
#'
#' @return Transcript object
#'
#' @seealso \link{helper_transcript_patch_make}
#'
#' @export
#'
#' @example inst/examples/helper_transcript_patch_make.R
#'
helper_transcript_patch_apply <- function(x, patch, direction = c("backward", "forward")) {
	.assert_transcript(x, arg = "x", missing = missing(x))
	direction <- match.arg(direction)
	if (!is.list(patch) || !all(c("annotations", "tiers", "media", "length.sec") %in% names(patch))) {
		cli::cli_abort("Parameter {.arg patch} needs to be a patch created by {.fn helper_transcript_patch_make}.")
	}
	backward <- identical(direction, "backward")

	p <- patch$annotations
	if (!is.null(p)) {
		if (isTRUE(p$replace)) {
			from <- if (backward) p$after else p$before
			to   <- if (backward) p$before else p$after
			if (!.patch_tables_equal(x@annotations, from)) {
				cli::cli_abort("The patch does not fit the transcript: the annotations differ from the expected state.")
			}
			annotations_new <- to
		} else {
			annotations_new <- .patch_rows_swap(
				x@annotations,
				drop      = if (backward) p$added else p$removed,
				put       = if (backward) p$removed else p$added,
				put_index = if (backward) p$removed.index else p$added.index)
		}
	} else {
		annotations_new <- x@annotations
	}

	tiers_new <- .patch_table_switch(x@tiers, patch$tiers, backward, "tier table")
	media_new <- .patch_table_switch(x@media, patch$media, backward, "media table")

	x@annotations <- annotations_new
	x@tiers       <- tiers_new
	x@media       <- media_new
	if (!is.null(patch$length.sec)) {
		x@length.sec <- if (backward) patch$length.sec$before else patch$length.sec$after
	}
	x
}

.patch_table_switch <- function(current, part, backward, label) {
	if (is.null(part)) return(current)
	from <- if (backward) part$after else part$before
	to   <- if (backward) part$before else part$after
	if (!.patch_tables_equal(current, from)) {
		cli::cli_abort("The patch does not fit the transcript: the {label} differs from the expected state.")
	}
	to
}

.patch_rows_swap <- function(a, drop, put, put_index) {
	if (!is.null(drop) && nrow(drop)) {
		if (!identical(names(drop), names(a))) {
			cli::cli_abort("The patch does not fit the transcript: the annotation columns differ.")
		}
		hit <- match(.patch_occurrence(.patch_row_keys(drop)),
					 .patch_occurrence(.patch_row_keys(a)))
		if (anyNA(hit)) {
			cli::cli_abort("The patch does not fit the transcript: {sum(is.na(hit))} annotation{?s} to be removed {?is/are} missing.")
		}
		a <- a[-hit, , drop = FALSE]
	}
	if (!is.null(put) && nrow(put)) {
		if (!identical(names(put), names(a))) {
			cli::cli_abort("The patch does not fit the transcript: the annotation columns differ.")
		}
		n_final <- nrow(a) + nrow(put)
		ord <- order(put_index)
		put <- put[ord, , drop = FALSE]
		pos <- as.integer(put_index[ord])
		if (anyDuplicated(pos) || any(pos < 1L | pos > n_final)) {
			pos <- seq.int(nrow(a) + 1L, n_final)
		}
		is_put <- logical(n_final)
		is_put[pos] <- TRUE
		index <- integer(n_final)
		index[is_put]  <- nrow(a) + seq_len(nrow(put))
		index[!is_put] <- seq_len(nrow(a))
		a <- rbind(a, put)[index, , drop = FALSE]
	}
	rownames(a) <- NULL
	a
}

.patch_tables_equal <- function(a, b) {
	if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
	if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) return(FALSE)
	identical(.patch_row_keys(a), .patch_row_keys(b))
}

.patch_row_keys <- function(df) {
	if (is.null(df) || !nrow(df)) return(character(0))
	cols <- lapply(df, function(v) {
		if (is.double(v)) {
			out <- sprintf("%.17g", v)
			out[is.na(v)] <- "NA"
			out
		} else {
			out <- as.character(v)
			out[is.na(v)] <- "␀"
			out
		}
	})
	do.call(paste, c(unname(cols), sep = "␟"))
}

.patch_occurrence <- function(keys) {
	if (!length(keys)) return(character(0))
	paste(keys, stats::ave(seq_along(keys), keys, FUN = seq_along), sep = "␞")
}
