#' Update full texts
#'
#' Creates/updates the full texts of the transcripts in a corpus.
#'The full text may be created in two different ways:
#'- The contents of a transcription will be joined consecutively based on the time information.
#'- The contents of each tier will be joined consecutively, and then the next tier will be joined.
#'
#' @param x Corpus object.
#' @param searchMode Deprecated. Both fulltext modes (bytime and bytier) are always created together; this parameter has no effect and is kept only for backward compatibility.
#' @param transcriptNames Vector of character strings; Names of the transcripts you want to update; leave empty if you want to process all transcripts that need an update.
#' @param tierNames Vector of character strings; Names of the tiers to include in the fulltext.
#' @param forceUpdate Logical; If \code{TRUE} fulltexts will be created in any case, if \code{FALSE} fulltexts will be only be created if there was a modification to the transcript since the last creation of the fulltexts.
#'
#' @return Corpus object.
#' @export
#'
#' @examples
#' library(act)
#'
#' examplecorpus <- act::transcripts_update_fulltexts(x=examplecorpus)
#'
transcripts_update_fulltexts <- function(x,
										 searchMode      = c("fulltext", "fulltext.bytier", "fulltext.bytime"),
										 transcriptNames = NULL,
										 tierNames       = NULL,
										 forceUpdate     = FALSE) {

	#=== check x object
	.assert_corpus(x, missing = missing(x))
	if (is.null(x@transcripts)) 	{
		cli::cli_warn("No transcripts found in corpus object x.")
		return(x)
	}

	# if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}

	# check arguments
	searchMode <- match.arg(searchMode)

	# cache options once (avoid repeated options() lookups inside the loop)
	sep_intervals <- getOption("act.separator_between_intervals")
	sep_tiers     <- getOption("act.separator_between_tiers")

	# compute current signature per transcript and decide which need update
	tier_filters <- lapply(transcriptNames, function(i) {
		tnames_t <- x@transcripts[[i]]@tiers$name
		if (!is.null(tierNames)) {
			tnames_t <- tnames_t[tnames_t %in% tierNames]
		}
		tnames_t
	})
	names(tier_filters) <- transcriptNames

	# Sort ann by startsec + tierName before hashing so the signature is
	# independent of row order. The fulltext computation itself sorts the ann
	# again later, so without this sort the signature stored in the cache
	# would not match a re-computed signature on the cached (already-sorted)
	# ann in a later session.
	current_sigs <- vapply(transcriptNames, function(i) {
		ann <- x@transcripts[[i]]@annotations
		if (is.null(ann) || nrow(ann) == 0) {
			content <- character(0); content.norm <- character(0)
			tier <- character(0); start <- numeric(0); end <- numeric(0)
		} else {
			ann <- ann[order(ann$startsec, ann$tierName), ]
			content      <- as.character(ann$content)
			content.norm <- if ("content.norm" %in% names(ann)) as.character(ann$content.norm) else character(0)
			tier         <- as.character(ann$tierName)
			start        <- as.numeric(ann$startsec)
			end          <- as.numeric(ann$endsec)
		}
		digest::digest(list(content, content.norm, tier, start, end,
		                    sep_tiers, sep_intervals,
		                    tier_filters[[i]]),
		               algo = "xxhash64")
	}, character(1))

	needs_update <- vapply(seq_along(transcriptNames), function(k) {
		if (forceUpdate) return(TRUE)
		tr <- x@transcripts[[transcriptNames[k]]]
		stored_sig <- tr@fulltext.signature
		if (length(stored_sig) == 0) return(TRUE)
		stored_sig != current_sigs[k]
	}, logical(1))
	transcriptNames.update <- transcriptNames[needs_update]
	current_sigs.update    <- current_sigs[needs_update]

	if (length(transcriptNames.update) == 0) return(x)

	#=== iterate through all transcripts that need an update
	helper_progress_set("Updating fulltexts", length(transcriptNames.update))

	for (k in seq_along(transcriptNames.update)) {
		helper_progress_tick()
		i <- transcriptNames.update[k]

		ann <- x@transcripts[[i]]@annotations
		tierNames.forthistranscript <- tier_filters[[i]]

		myFulltext.bytime.orig <- ""
		myFulltext.bytime.norm <- ""
		myFulltext.bytier.orig <- ""
		myFulltext.bytier.norm <- ""

		if (!is.null(ann) && nrow(ann) > 0) {

			# Pre-initialise the two *bytime.start columns to 0 (double).
			# Preserves the previous behaviour where cbind() added these
			# columns unconditionally, even when the bytime block does not run.
			ann$char.orig.bytime.start <- 0
			ann$char.norm.bytime.start <- 0

			res <- .fulltext_compute(
				ann              = ann,
				mode             = "bytime",
				tierNames_filter = tierNames.forthistranscript,
				sep_intervals    = sep_intervals,
				sep_tiers        = sep_tiers
			)
			ann                    <- res$ann
			myFulltext.bytime.orig <- res$fulltext_orig
			myFulltext.bytime.norm <- res$fulltext_norm

			res <- .fulltext_compute(
				ann              = ann,
				mode             = "bytier",
				tierNames_filter = tierNames.forthistranscript,
				sep_intervals    = sep_intervals,
				sep_tiers        = sep_tiers
			)
			ann                    <- res$ann
			myFulltext.bytier.orig <- res$fulltext_orig
			myFulltext.bytier.norm <- res$fulltext_norm

			x@transcripts[[i]]@annotations <- ann
		}

		x@transcripts[[i]]@fulltext.bytime.orig       <- myFulltext.bytime.orig
		x@transcripts[[i]]@fulltext.bytime.norm       <- myFulltext.bytime.norm
		x@transcripts[[i]]@fulltext.bytier.orig       <- myFulltext.bytier.orig
		x@transcripts[[i]]@fulltext.bytier.norm       <- myFulltext.bytier.norm
		x@transcripts[[i]]@fulltext.signature         <- current_sigs.update[k]
		x@transcripts[[i]]@fulltext.filter.tier.names <- tierNames.forthistranscript
	}

	return(x)
}


# Internal helper for transcripts_update_fulltexts.
#
# Sorts annotations in the requested order, computes separator + length
# + position vectors using local vec_* variables (avoiding the cbind()
# data.frame copies the previous implementation relied on), writes the
# four position columns for the given mode into the annotations data
# frame, and builds the two fulltext strings (orig + norm).
#
# @keywords internal
.fulltext_compute <- function(ann, mode, tierNames_filter,
							   sep_intervals, sep_tiers) {

	# sort
	if (mode == "bytime") {
		ann <- ann[order(ann$startsec, ann$tierName), ]
	} else {
		ann <- ann[order(ann$tierName, ann$startsec), ]
	}

	# tier filter
	include <- which(ann$tierName %in% tierNames_filter)

	n <- nrow(ann)

	# local vectors instead of cbind temp columns.
	# Use double (numeric) so the resulting position columns match the type
	# of the old cbind()-initialised columns bit for bit.
	vec_separator                 <- character(n)
	vec_char_separator            <- numeric(n)
	vec_char_content_orig         <- numeric(n)
	vec_char_content_norm         <- numeric(n)
	vec_char_content_orig_plussep <- numeric(n)
	vec_char_content_norm_plussep <- numeric(n)

	# same-tier flag: first element always FALSE, then compare to predecessor
	if (length(include) > 1) {
		included.sametier <- c(FALSE,
							   ann$tierName[include[-1]] ==
							   ann$tierName[include[-length(include)]])
	} else if (length(include) == 1) {
		included.sametier <- FALSE
	} else {
		included.sametier <- logical(0)
	}

	if (length(include) > 0) {
		# vectorized ifelse replaces unlist(lapply(..., function(x) if(x) ... else ...))
		included.separator <- ifelse(included.sametier, sep_intervals, sep_tiers)
		vec_separator[include]                 <- included.separator
		vec_char_separator[include]            <- nchar(vec_separator[include])
		vec_char_content_orig[include]         <- nchar(ann$content[include])
		vec_char_content_norm[include]         <- nchar(ann$content.norm[include])
		vec_char_content_orig_plussep[include] <- vec_char_separator[include] +
												   vec_char_content_orig[include]
		vec_char_content_norm_plussep[include] <- vec_char_separator[include] +
												   vec_char_content_norm[include]
	}

	# position columns (persisted in annotations)
	char_orig_end   <- cumsum(vec_char_content_orig_plussep)
	char_norm_end   <- cumsum(vec_char_content_norm_plussep)
	char_orig_start <- char_orig_end - vec_char_content_orig_plussep + 1
	char_norm_start <- char_norm_end - vec_char_content_norm_plussep + 1

	if (mode == "bytime") {
		ann$char.orig.bytime.start <- char_orig_start
		ann$char.orig.bytime.end   <- char_orig_end
		ann$char.norm.bytime.start <- char_norm_start
		ann$char.norm.bytime.end   <- char_norm_end
	} else {
		ann$char.orig.bytier.start <- char_orig_start
		ann$char.orig.bytier.end   <- char_orig_end
		ann$char.norm.bytier.start <- char_norm_start
		ann$char.norm.bytier.end   <- char_norm_end
	}

	# fulltext strings
	if (length(include) > 0) {
		fulltext_orig <- stringi::stri_c(vec_separator[include],
										  ann$content[include],
										  collapse = "")
		fulltext_norm <- stringi::stri_c(vec_separator[include],
										  ann$content.norm[include],
										  collapse = "")
	} else {
		fulltext_orig <- ""
		fulltext_norm <- ""
	}

	# trailing tier separator (matches original)
	fulltext_orig <- stringi::stri_c(fulltext_orig, sep_tiers)
	fulltext_norm <- stringi::stri_c(fulltext_norm, sep_tiers)

	list(ann = ann, fulltext_orig = fulltext_orig, fulltext_norm = fulltext_norm)
}
