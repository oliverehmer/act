#' Search in full text of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#'
#' @return \code{Data.frame} data frame with search results.
#'
#' @export
#'
#' @example inst/examples/search_transcript_fulltext.R
#'
search_transcript_fulltext <- function(t, s) {
	helper_progress_tick()

	.assert_transcript(t, missing = missing(t))
	.assert_search(s, missing = missing(s))

	#==== cache options ONCE (old code read these per-hit inside a closure) ====
	sep_tiers     <- getOption("act.separator_between_tiers")
	sep_intervals <- getOption("act.separator_between_intervals")

	ann <- t@annotations

	search.results.byTime <- NULL
	search.results.byTier <- NULL

	#==== byTime ====
	if (s@search.mode == "fulltext" || s@search.mode == "fulltext.byTime") {
		search.results.byTime <- .search_fulltext_one_mode(
			t             = t,
			ann           = ann,
			s             = s,
			mode          = "byTime",
			sep_tiers     = sep_tiers,
			sep_intervals = sep_intervals
		)
	}

	#==== byTier ====
	if (s@search.mode == "fulltext" || s@search.mode == "fulltext.byTier") {
		search.results.byTier <- .search_fulltext_one_mode(
			t             = t,
			ann           = ann,
			s             = s,
			mode          = "byTier",
			sep_tiers     = sep_tiers,
			sep_intervals = sep_intervals
		)
		#=== delete results that are across tiers
		if (!is.null(search.results.byTier) && nrow(search.results.byTier) > 0) {
			search.results.byTier <-
				search.results.byTier[search.results.byTier$hit.span != "across tiers", ]
		}
	}

	#==== merge / dedup ====
	if (!is.null(search.results.byTime) && is.null(search.results.byTier)) {
		search.results <- search.results.byTime
	} else if (is.null(search.results.byTime) && !is.null(search.results.byTier)) {
		search.results <- search.results.byTier
	} else if (!is.null(search.results.byTime) && !is.null(search.results.byTier)) {
		search.results <- rbind(search.results.byTime, search.results.byTier)
		compare <- search.results[, c("annotationID", "hit.pos.content")]
		doubles <- duplicated(compare)
		search.results <- search.results[!doubles, ]
	} else {
		search.results <- NULL
	}

	#==== time section filter ====
	if (length(s@filter.section.startsec) != 0) {
		if (!is.na(s@filter.section.startsec)) {
			search.results <- search.results[search.results$endsec > s@filter.section.startsec, ]
		}
	}
	if (length(s@filter.section.endsec) != 0) {
		if (!is.na(s@filter.section.endsec)) {
			search.results <- search.results[search.results$startsec < s@filter.section.endsec, ]
		}
	}

	if (is.null(search.results)) return(NULL)

	search.results <- cbind(transcriptName = rep(t@name, times = nrow(search.results)),
							search.results)

	search.results
}


# Internal helper: runs the fulltext search for exactly one mode
# ("byTime" or "byTier") and returns a data.frame (or NULL if no hits).
#
# @keywords internal
.search_fulltext_one_mode <- function(t, ann, s, mode,
									   sep_tiers, sep_intervals) {

	if (mode == "byTime") {
		if (isTRUE(s@search.normalized)) {
			myFulltext <- t@fulltext.bytime.norm
			col_start  <- ann$char.norm.bytime.start
			col_end    <- ann$char.norm.bytime.end
		} else {
			myFulltext <- t@fulltext.bytime.orig
			col_start  <- ann$char.orig.bytime.start
			col_end    <- ann$char.orig.bytime.end
		}
	} else {
		if (isTRUE(s@search.normalized)) {
			myFulltext <- t@fulltext.bytier.norm
			col_start  <- ann$char.norm.bytier.start
			col_end    <- ann$char.norm.bytier.end
		} else {
			myFulltext <- t@fulltext.bytier.orig
			col_start  <- ann$char.orig.bytier.start
			col_end    <- ann$char.orig.bytier.end
		}
	}

	if (length(myFulltext) == 0) return(NULL)
	if (is.na(myFulltext))       return(NULL)

	if (!stringr::str_detect(myFulltext, s@pattern)) return(NULL)

	hit              <- unlist(stringr::str_extract_all(myFulltext, s@pattern))
	hit.length       <- stringr::str_length(hit)
	hit.nr           <- seq_along(hit)
	hit.pos.fulltext <- data.frame(stringr::str_locate_all(myFulltext, s@pattern),
								   stringsAsFactors = FALSE)$start

	matches.recordsetNrs <- sapply(hit.pos.fulltext,
								   function(hp) {
									   which(hp >= col_start & hp <= col_end)[1]
								   })

	search.results <- ann[matches.recordsetNrs, ]

	if (mode == "byTime") {
		if (isTRUE(s@search.normalized)) {
			hit.pos.content <- hit.pos.fulltext - search.results$char.norm.bytime.start + 1
			hit.pos.content <- hit.pos.content -
				(search.results$char.norm.bytime.end -
				 search.results$char.norm.bytime.start -
				 nchar(search.results$content.norm) + 1)
		} else {
			hit.pos.content <- hit.pos.fulltext - search.results$char.orig.bytime.start + 1
			hit.pos.content <- hit.pos.content -
				(search.results$char.orig.bytime.end -
				 search.results$char.orig.bytime.start -
				 nchar(search.results$content) + 1)
		}
	} else {
		if (isTRUE(s@search.normalized)) {
			hit.pos.content <- hit.pos.fulltext - search.results$char.norm.bytier.start + 1
			hit.pos.content <- hit.pos.content -
				(search.results$char.norm.bytier.end -
				 search.results$char.norm.bytier.start -
				 nchar(search.results$content.norm) + 1)
		} else {
			hit.pos.content <- hit.pos.fulltext - search.results$char.orig.bytier.start + 1
			hit.pos.content <- hit.pos.content -
				(search.results$char.orig.bytier.end -
				 search.results$char.orig.bytier.start -
				 nchar(search.results$content) + 1)
		}
	}

	hit.span <- .detect_hit_span_vec(hit, sep_tiers, sep_intervals)

	searchMode <- if (mode == "byTime") "byTime" else "byTier"

	cbind(search.results,
		  hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content,
		  searchMode = searchMode,
		  hit.span   = hit.span)
}


# Internal helper: vectorized hit span detection.
#
# A separator is considered "inside" the hit iff there is at least one
# character on each side of it (i.e. NOT at position 1 and NOT at the last
# position).
#
# @keywords internal
.detect_hit_span_vec <- function(hits, sep_tiers, sep_intervals) {
	n      <- length(hits)
	result <- rep("within interval", n)

	is_bad <- is.na(hits) | hits == ""
	result[is_bad] <- "error"

	good_mask <- !is_bad
	if (!any(good_mask)) return(result)

	good_hits <- hits[good_mask]
	good_lens <- nchar(good_hits)

	tier_positions <- stringr::str_locate_all(good_hits,
											   stringr::fixed(sep_tiers))
	has_tier_inside <- vapply(seq_along(tier_positions), function(i) {
		pos <- tier_positions[[i]]
		if (nrow(pos) == 0) return(FALSE)
		any(pos[, "start"] != 1L & pos[, "end"] != good_lens[i])
	}, logical(1))

	has_interval_inside <- logical(length(good_hits))
	not_yet <- !has_tier_inside
	if (any(not_yet)) {
		int_positions <- stringr::str_locate_all(good_hits[not_yet],
												  stringr::fixed(sep_intervals))
		not_yet_lens <- good_lens[not_yet]
		has_interval_inside_ny <- vapply(seq_along(int_positions), function(i) {
			pos <- int_positions[[i]]
			if (nrow(pos) == 0) return(FALSE)
			any(pos[, "start"] != 1L & pos[, "end"] != not_yet_lens[i])
		}, logical(1))
		has_interval_inside[not_yet] <- has_interval_inside_ny
	}

	result_good <- rep("within interval", length(good_hits))
	result_good[has_tier_inside]     <- "across tiers"
	result_good[has_interval_inside] <- "across intervals"
	result[good_mask] <- result_good

	result
}
