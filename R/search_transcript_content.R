#' Search in original content of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#'
#' @return \code{Data.frame} data frame with search results.
#'
#' @export
#'
#' @example inst/examples/search_transcript_content.R
#'
search_transcript_content <- function(t, s) {
	helper_progress_tick()

	.assert_transcript(t, missing = missing(t))
	.assert_search(s, missing = missing(s))

	temp <- NULL

	#==== tier filter ====
	tierNames.all <- t@tiers$name
	if (is.null(tierNames.all)) tierNames.all <- as.character()

	filterTierNames <- as.character()
	if (!is.null(s@filter.tier.names)) {
		if (length(s@filter.tier.names) == 0) {
			filterTierNames <- tierNames.all
		} else {
			filterTierNames <- intersect(tierNames.all, s@filter.tier.names)
		}
	}
	filterTierNames <- helper_tiers_filter_create(
		tierNames              = filterTierNames,
		filterTierIncludeRegEx = s@filter.tier.includeRegEx,
		filterTierExcludeRegEx = s@filter.tier.excludeRegEx
	)
	ann <- t@annotations[t@annotations$tierName %in% filterTierNames, ]

	#==== time section filter ====
	if (length(s@filter.section.startsec) != 0) {
		if (!is.na(s@filter.section.startsec)) {
			ann <- ann[(ann$endsec > s@filter.section.startsec), ]
		}
	}
	if (length(s@filter.section.endsec) != 0) {
		if (!is.na(s@filter.section.endsec)) {
			ann <- ann[(ann$startsec < s@filter.section.endsec), ]
		}
	}

	if (is.null(ann) || nrow(ann) == 0) return(NULL)

	#==== choose content column (normalized vs original) ====
	if (isTRUE(s@search.normalized)) {
		content_col <- ann$content.norm
	} else {
		content_col <- ann$content
	}

	if (length(content_col) == 0 || is.na(content_col[1])) return(NULL)

	#==== vectorized detection ====
	indices <- stringr::str_detect(content_col, s@pattern)
	if (!any(indices)) return(NULL)

	hits.pos   <- stringr::str_locate_all(content_col[indices], s@pattern)
	hits.match <- stringr::str_extract_all(content_col[indices], s@pattern)
	ann_ids    <- ann$annotationID[indices]

	#==== build matches.df vectorized (replaces rbind-in-loop) ====
	n_per     <- lengths(hits.match)
	positions <- do.call(rbind, hits.pos)

	annotationID_vec <- rep(ann_ids, n_per)
	hit_nr_vec       <- rep(seq_along(ann_ids), n_per)
	hit_vec          <- unlist(hits.match, use.names = FALSE)

	sResults <- data.frame(
		annotationID     = as.numeric(annotationID_vec),
		hit.pos.content  = as.numeric(positions[, 1L]),
		end              = as.numeric(positions[, 2L]),
		hit.nr           = as.numeric(hit_nr_vec),
		hit              = as.character(hit_vec),
		stringsAsFactors = FALSE
	)

	sResults$hit.length       <- as.numeric(stringr::str_length(sResults$hit))
	sResults$hit.pos.fulltext <- as.numeric(NA)
	sResults$searchMode       <- as.character("byTier")
	sResults$hit.span         <- as.character("within interval")
	sResults$end              <- NULL

	#==== merge with annotations ====
	temp <- merge(x = ann, y = sResults,
				  by.x = "annotationID", by.y = "annotationID",
				  all.y = TRUE)

	#==== prepend transcriptName ====
	if (!is.null(temp)) {
		if (nrow(temp) == 0) {
			temp <- cbind(transcriptName = character(0), temp)
		} else {
			temp <- cbind(transcriptName = rep(t@name, times = nrow(temp)), temp)
		}
	}
	return(temp)
}
