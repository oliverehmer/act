#' Make concordance for search results
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#'
#' @return Search object.
#' @export
#'
#' @example inst/examples/search_concordance.R
#'
search_concordance <- function(x,
							   s,
							   searchNormalized=TRUE) {

	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))

	conccolnames <- c("concLeft2", "concLeft1", "concHit",
					  "concRight1", "concRight2",
					  "nrWordsLeft", "nrWordsHitPosition", "nrWordsHit",
					  "nrWordsRight", "nrWordsTotal")

	# Strip any existing concordance columns from results.
	keep_cols <- setdiff(colnames(s@results), conccolnames)
	temp      <- s@results[, keep_cols, drop = FALSE]
	n         <- nrow(temp)

	# Empty result: keep same schema as the previous implementation.
	if (n == 0) {
		concs <- data.frame(
			concLeft2          = character(0),
			concLeft1          = character(0),
			concHit            = character(0),
			concRight1         = character(0),
			concRight2         = character(0),
			nrWordsLeft        = integer(0),
			nrWordsHitPosition = integer(0),
			nrWordsHit         = integer(0),
			nrWordsRight       = integer(0),
			nrWordsTotal       = integer(0),
			stringsAsFactors   = FALSE
		)
		s@results <- cbind(temp, concs)
		return(s)
	}

	#=== cache options and build regex ONCE
	sep_words     <- getOption("act.separator_between_words")
	sep_intervals <- getOption("act.separator_between_intervals")
	sep_tiers     <- getOption("act.separator_between_tiers")

	regex_last  <- paste0("(?<concLeft1>[",  sep_words, sep_intervals,
						  sep_tiers, "]*[\\W]*$)")
	regex_first <- paste0("(?<concRight1>^[\\W]*[", sep_words,
						  sep_intervals, sep_tiers, "]*)")

	#=== pre-allocate result vectors
	concLeft2          <- character(n)
	concLeft1          <- character(n)
	concHit            <- as.character(temp$hit)
	concRight1         <- character(n)
	concRight2         <- character(n)

	# Character NA for word-count columns to match the old output (the old
	# implementation went through t(apply(..., MARGIN = 1)) which collapses
	# all columns to character).
	nrWordsLeft        <- rep(NA_character_, n)
	nrWordsHitPosition <- rep(NA_character_, n)
	nrWordsHit         <- rep(NA_character_, n)
	nrWordsRight       <- rep(NA_character_, n)
	nrWordsTotal       <- rep(NA_character_, n)

	if (s@search.mode == "fulltext") {
		#=== fulltext mode: group by (transcriptName, searchMode)
		# fetch the fulltext slot ONCE per group instead of once per hit.
		hit_pos    <- as.integer(temp$hit.pos.fulltext)
		hit_length <- as.integer(temp$hit.length)

		valid <- !is.na(temp$annotationID) &
				 !is.na(hit_pos) &
				 !is.na(hit_length)

		group_key  <- paste(as.character(temp$transcriptName),
							as.character(temp$searchMode), sep = "|")
		unique_grp <- unique(group_key[valid])

		for (g in unique_grp) {
			idx <- which(group_key == g & valid)
			if (length(idx) == 0) next

			tname <- as.character(temp$transcriptName[idx[1]])
			smode <- as.character(temp$searchMode[idx[1]])

			if (smode == "byTier") {
				if (isTRUE(s@search.normalized)) {
					fulltext <- x@transcripts[[tname]]@fulltext.bytier.norm
				} else {
					fulltext <- x@transcripts[[tname]]@fulltext.bytier.orig
				}
			} else {
				if (isTRUE(s@search.normalized)) {
					fulltext <- x@transcripts[[tname]]@fulltext.bytime.norm
				} else {
					fulltext <- x@transcripts[[tname]]@fulltext.bytime.orig
				}
			}

			if (is.na(fulltext) || length(fulltext) == 0) {
				cli::cli_abort("Please recreate full text.")
			}

			res <- .concordance_compute_vec(
				fulltext     = fulltext,
				hit_pos      = hit_pos[idx],
				hit_length   = hit_length[idx],
				hit_text     = concHit[idx],
				conc_width   = s@concordance.width,
				regex_last   = regex_last,
				regex_first  = regex_first
			)

			concLeft1[idx]  <- res$concLeft1
			concLeft2[idx]  <- res$concLeft2
			concRight1[idx] <- res$concRight1
			concRight2[idx] <- res$concRight2
			# word counts stay NA for fulltext mode
		}

	} else {
		#=== content mode: the "fulltext" of each hit is its annotation content
		hit_pos    <- as.integer(temp$hit.pos.content)
		hit_length <- as.integer(temp$hit.length)

		valid <- !is.na(temp$annotationID) &
				 !is.na(hit_pos) &
				 !is.na(hit_length)

		contents <- if (isTRUE(s@search.normalized))
						as.character(temp$content.norm)
					else
						as.character(temp$content)

		valid <- valid & !is.na(contents)

		idx <- which(valid)
		if (length(idx) > 0) {
			res <- .concordance_compute_vec_percontent(
				fulltexts   = contents[idx],
				hit_pos     = hit_pos[idx],
				hit_length  = hit_length[idx],
				hit_text    = concHit[idx],
				conc_width  = s@concordance.width,
				regex_last  = regex_last,
				regex_first = regex_first
			)
			concLeft1[idx]          <- res$concLeft1
			concLeft2[idx]          <- res$concLeft2
			concRight1[idx]         <- res$concRight1
			concRight2[idx]         <- res$concRight2
			nrWordsLeft[idx]        <- res$nrWordsLeft
			nrWordsHitPosition[idx] <- res$nrWordsHitPosition
			nrWordsHit[idx]         <- res$nrWordsHit
			nrWordsRight[idx]       <- res$nrWordsRight
			nrWordsTotal[idx]       <- res$nrWordsTotal
		}
	}

	concs <- data.frame(
		concLeft2          = concLeft2,
		concLeft1          = concLeft1,
		concHit            = concHit,
		concRight1         = concRight1,
		concRight2         = concRight2,
		nrWordsLeft        = nrWordsLeft,
		nrWordsHitPosition = nrWordsHitPosition,
		nrWordsHit         = nrWordsHit,
		nrWordsRight       = nrWordsRight,
		nrWordsTotal       = nrWordsTotal,
		stringsAsFactors   = FALSE
	)

	s@results <- cbind(temp, concs)
	return(s)
}


# Internal helper for search_concordance (fulltext mode).
#
# Vectorized concordance computation for ONE group of hits that share the
# SAME fulltext string. Operates on vectors of hit positions and lengths.
#
# @keywords internal
.concordance_compute_vec <- function(fulltext, hit_pos, hit_length, hit_text,
									  conc_width, regex_last, regex_first) {

	# substring() requires integer start/stop; concordance.width is often
	# stored as double, so cast explicitly.
	conc_width <- as.integer(conc_width)

	n   <- length(hit_pos)
	nc  <- as.integer(nchar(fulltext))

	concLeft1  <- character(n)
	concLeft2  <- character(n)
	concRight1 <- character(n)
	concRight2 <- character(n)

	#--- left context
	has_left    <- hit_pos > 1
	left_margin <- as.integer(pmax(0L, hit_pos - conc_width - 1L))
	left_end    <- as.integer(hit_pos - 1L)
	left_part   <- rep("", n)
	if (any(has_left)) {
		left_part[has_left] <- substring(fulltext, left_margin[has_left],
										  left_end[has_left])
		lp                <- left_part[has_left]
		cL1               <- stringr::str_extract(lp, regex_last)
		pos               <- stringr::str_locate(lp, regex_last)[, 1] - 1L
		cL2               <- stringr::str_sub(lp, 1L, pos)
		cL1[is.na(cL1)]   <- ""
		cL2[is.na(cL2)]   <- ""
		concLeft1[has_left] <- stringr::str_trim(cL1, side = "both")
		concLeft2[has_left] <- stringr::str_trim(cL2, side = "both")
	}

	#--- right context
	has_right    <- (hit_pos + hit_length) < nc
	right_start  <- as.integer(hit_pos + hit_length)
	right_end    <- as.integer(pmin(right_start + conc_width, nc))
	right_part   <- rep("", n)
	if (any(has_right)) {
		right_part[has_right] <- substring(fulltext, right_start[has_right],
											right_end[has_right])
		rp                <- right_part[has_right]
		cR1               <- stringr::str_extract(rp, regex_first)
		pos               <- stringr::str_locate(rp, regex_first)[, 2] + 1L
		cR2               <- stringr::str_sub(rp, pos, nchar(rp))
		cR1[is.na(cR1)]   <- ""
		cR2[is.na(cR2)]   <- ""
		concRight1[has_right] <- stringr::str_trim(cR1, side = "both")
		concRight2[has_right] <- stringr::str_trim(cR2, side = "both")
	}

	list(
		concLeft1  = concLeft1,
		concLeft2  = concLeft2,
		concRight1 = concRight1,
		concRight2 = concRight2
	)
}


# Internal helper for search_concordance (content mode).
#
# Like .concordance_compute_vec() but each hit has its own "fulltext"
# (the annotation content). Inputs fulltexts, hit_pos, hit_length are all
# vectors of the same length n. Additionally computes word-count columns.
#
# Word-count columns are returned as character to match the bit-for-bit
# output of the previous implementation (which went through
# t(apply(..., MARGIN = 1)) and thus collapsed everything to character).
#
# @keywords internal
.concordance_compute_vec_percontent <- function(fulltexts, hit_pos, hit_length,
												 hit_text, conc_width,
												 regex_last, regex_first) {

	conc_width <- as.integer(conc_width)

	n  <- length(hit_pos)
	nc <- as.integer(nchar(fulltexts))

	concLeft1  <- character(n)
	concLeft2  <- character(n)
	concRight1 <- character(n)
	concRight2 <- character(n)

	has_left    <- hit_pos > 1
	left_margin <- as.integer(pmax(0L, hit_pos - conc_width - 1L))
	left_end    <- as.integer(hit_pos - 1L)
	left_part   <- rep("", n)
	if (any(has_left)) {
		left_part[has_left] <- substring(fulltexts[has_left],
										  left_margin[has_left],
										  left_end[has_left])
		lp                <- left_part[has_left]
		cL1               <- stringr::str_extract(lp, regex_last)
		pos               <- stringr::str_locate(lp, regex_last)[, 1] - 1L
		cL2               <- stringr::str_sub(lp, 1L, pos)
		cL1[is.na(cL1)]   <- ""
		cL2[is.na(cL2)]   <- ""
		concLeft1[has_left] <- stringr::str_trim(cL1, side = "both")
		concLeft2[has_left] <- stringr::str_trim(cL2, side = "both")
	}

	has_right    <- (hit_pos + hit_length) < nc
	right_start  <- as.integer(hit_pos + hit_length)
	right_end    <- as.integer(pmin(right_start + conc_width, nc))
	right_part   <- rep("", n)
	if (any(has_right)) {
		right_part[has_right] <- substring(fulltexts[has_right],
											right_start[has_right],
											right_end[has_right])
		rp                <- right_part[has_right]
		cR1               <- stringr::str_extract(rp, regex_first)
		pos               <- stringr::str_locate(rp, regex_first)[, 2] + 1L
		cR2               <- stringr::str_sub(rp, pos, nchar(rp))
		cR1[is.na(cR1)]   <- ""
		cR2[is.na(cR2)]   <- ""
		concRight1[has_right] <- stringr::str_trim(cR1, side = "both")
		concRight2[has_right] <- stringr::str_trim(cR2, side = "both")
	}

	wL  <- as.integer(stringi::stri_count_words(left_part))
	wH  <- as.integer(stringi::stri_count_words(hit_text))
	wR  <- as.integer(stringi::stri_count_words(right_part))
	wHP <- wL + 1L
	wT  <- wL + wH + wR

	list(
		concLeft1          = concLeft1,
		concLeft2          = concLeft2,
		concRight1         = concRight1,
		concRight2         = concRight2,
		nrWordsLeft        = as.character(wL),
		nrWordsHitPosition = as.character(wHP),
		nrWordsHit         = as.character(wH),
		nrWordsRight       = as.character(wR),
		nrWordsTotal       = as.character(wT)
	)
}
