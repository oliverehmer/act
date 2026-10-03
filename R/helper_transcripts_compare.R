#' Helper: Compare two versions of a transcript
#'
#' Compares the content of two versions of a transcript and reports where they
#' differ: the time regions per tier in which annotations were added, removed or
#' changed, the tiers that were added, removed, renamed or moved, and the media
#' files that were added or removed.
#'
#' The comparison works on the content, not on the file text: for every tier the
#' annotations are compared by start, end and content. Formatting differences of
#' the files (identifiers, dates, number formats) therefore do not count as
#' changes. Times are rounded to \code{digits} decimals before they are compared.
#'
#' A tier that is missing in \code{y} while a new tier in \code{y} holds exactly
#' the same annotations is reported as renamed. Added, removed and renamed tiers
#' are reported in \code{tiers} only; \code{regions} covers the tiers that exist
#' in both versions.
#'
#' @param x Transcript object; the older version.
#' @param y Transcript object; the newer version.
#' @param gap Double; regions of the same tier that are less than or exactly
#'   \code{gap} seconds apart are joined into one region. \code{0} joins only
#'   touching and overlapping regions.
#' @param digits Integer; number of decimals to which times are rounded.
#'
#' @return List with three data frames: \code{regions} (columns \code{tierName},
#'   \code{startsec}, \code{endsec}, \code{change} with the values
#'   \code{"added"}, \code{"removed"}, \code{"changed"}), \code{tiers} (columns
#'   \code{change} with the values \code{"added"}, \code{"removed"},
#'   \code{"renamed"}, \code{"moved"}, \code{tierName}, \code{tierName.new}) and
#'   \code{media} (columns \code{change}, \code{path}).
#'
#' @seealso \link{helper_transcript_patch_make}
#'
#' @export
#'
#' @example inst/examples/helper_transcripts_compare.R
#'
helper_transcripts_compare <- function(x, y, gap = 0, digits = 3) {
	.assert_transcript(x, arg = "x", missing = missing(x))
	.assert_transcript(y, arg = "y", missing = missing(y))

	tiers_x <- .compare_tier_order(x)
	tiers_y <- .compare_tier_order(y)
	sig_x <- .compare_tier_signatures(x@annotations, tiers_x, digits)
	sig_y <- .compare_tier_signatures(y@annotations, tiers_y, digits)

	removed <- setdiff(tiers_x, tiers_y)
	added   <- setdiff(tiers_y, tiers_x)
	renamed_from <- character(0)
	renamed_to   <- character(0)
	for (tier_old in removed) {
		candidates <- setdiff(added, renamed_to)
		hit <- candidates[vapply(candidates, function(tier_new) identical(sig_x[[tier_old]], sig_y[[tier_new]]), logical(1))]
		if (length(hit) == 1L || (length(hit) > 1L && length(sig_x[[tier_old]]) > 0L)) {
			renamed_from <- c(renamed_from, tier_old)
			renamed_to   <- c(renamed_to, hit[1])
		}
	}
	removed <- setdiff(removed, renamed_from)
	added   <- setdiff(added, renamed_to)

	common_x <- tiers_x[tiers_x %in% c(intersect(tiers_x, tiers_y), renamed_from)]
	common_y_names <- ifelse(common_x %in% renamed_from, renamed_to[match(common_x, renamed_from)], common_x)
	order_in_y <- match(common_y_names, tiers_y[tiers_y %in% common_y_names])
	moved <- common_x[!.compare_in_lis(order_in_y)]

	tiers <- data.frame(
		change       = c(rep("added", length(added)), rep("removed", length(removed)),
						 rep("renamed", length(renamed_from)), rep("moved", length(moved))),
		tierName     = c(added, removed, renamed_from, moved),
		tierName.new = c(rep(NA_character_, length(added) + length(removed)), renamed_to,
						 ifelse(moved %in% renamed_from, renamed_to[match(moved, renamed_from)], moved)),
		stringsAsFactors = FALSE)

	regions <- list()
	for (tier in intersect(tiers_x, tiers_y)) {
		occ_x <- .patch_occurrence(sig_x[[tier]])
		occ_y <- .patch_occurrence(sig_y[[tier]])
		rows_x <- .compare_tier_rows(x@annotations, tier, digits)
		rows_y <- .compare_tier_rows(y@annotations, tier, digits)
		only_x <- rows_x[!(occ_x %in% occ_y), , drop = FALSE]
		only_y <- rows_y[!(occ_y %in% occ_x), , drop = FALSE]
		if (!nrow(only_x) && !nrow(only_y)) next
		parts <- rbind(
			if (nrow(only_x)) data.frame(startsec = only_x$startsec, endsec = only_x$endsec, change = "removed", stringsAsFactors = FALSE),
			if (nrow(only_y)) data.frame(startsec = only_y$startsec, endsec = only_y$endsec, change = "added", stringsAsFactors = FALSE))
		joined <- .compare_join_regions(parts, gap)
		joined$tierName <- tier
		regions[[tier]] <- joined[, c("tierName", "startsec", "endsec", "change")]
	}
	regions <- if (length(regions)) do.call(rbind, unname(regions)) else
		data.frame(tierName = character(0), startsec = double(0), endsec = double(0),
				   change = character(0), stringsAsFactors = FALSE)
	rownames(regions) <- NULL

	paths_x <- unique(as.character(x@media$path))
	paths_y <- unique(as.character(y@media$path))
	media <- data.frame(
		change = c(rep("added", length(setdiff(paths_y, paths_x))), rep("removed", length(setdiff(paths_x, paths_y)))),
		path   = c(setdiff(paths_y, paths_x), setdiff(paths_x, paths_y)),
		stringsAsFactors = FALSE)

	list(regions = regions, tiers = tiers, media = media)
}

.compare_tier_order <- function(t) {
	tiers <- t@tiers
	names_table <- if (!is.null(tiers) && nrow(tiers)) {
		if ("position" %in% names(tiers)) as.character(tiers$name[order(tiers$position)]) else as.character(tiers$name)
	} else character(0)
	unique(c(names_table, setdiff(unique(as.character(t@annotations$tierName)), names_table)))
}

.compare_tier_rows <- function(a, tier, digits) {
	rows <- a[a$tierName == tier, c("startsec", "endsec", "content"), drop = FALSE]
	rows$startsec <- round(as.numeric(rows$startsec), digits)
	rows$endsec   <- round(as.numeric(rows$endsec), digits)
	rows$content  <- ifelse(is.na(rows$content), "", as.character(rows$content))
	rows <- rows[order(rows$startsec, rows$endsec, rows$content), , drop = FALSE]
	rownames(rows) <- NULL
	rows
}

.compare_tier_signatures <- function(a, tiers, digits) {
	stats::setNames(lapply(tiers, function(tier) {
		rows <- .compare_tier_rows(a, tier, digits)
		if (!nrow(rows)) return(character(0))
		paste(sprintf("%.*f", digits, rows$startsec), sprintf("%.*f", digits, rows$endsec), rows$content, sep = "␟")
	}), tiers)
}

.compare_join_regions <- function(parts, gap) {
	parts <- parts[order(parts$startsec, parts$endsec), , drop = FALSE]
	out_start <- parts$startsec[1]
	out_end   <- parts$endsec[1]
	out_kind  <- parts$change[1]
	result <- list()
	if (nrow(parts) > 1L) for (i in 2:nrow(parts)) {
		if (parts$startsec[i] - out_end <= gap) {
			out_end <- max(out_end, parts$endsec[i])
			if (!identical(out_kind, parts$change[i])) out_kind <- "changed"
		} else {
			result[[length(result) + 1L]] <- c(out_start, out_end)
			attr(result[[length(result)]], "change") <- out_kind
			out_start <- parts$startsec[i]
			out_end   <- parts$endsec[i]
			out_kind  <- parts$change[i]
		}
	}
	result[[length(result) + 1L]] <- c(out_start, out_end)
	attr(result[[length(result)]], "change") <- out_kind
	data.frame(
		startsec = vapply(result, function(r) r[1], double(1)),
		endsec   = vapply(result, function(r) r[2], double(1)),
		change   = vapply(result, function(r) attr(r, "change"), character(1)),
		stringsAsFactors = FALSE)
}

.compare_in_lis <- function(v) {
	n <- length(v)
	if (n < 2L) return(rep(TRUE, n))
	len  <- rep(1L, n)
	prev <- rep(0L, n)
	for (i in 2:n) for (j in seq_len(i - 1L)) {
		if (v[j] < v[i] && len[j] + 1L > len[i]) {
			len[i]  <- len[j] + 1L
			prev[i] <- j
		}
	}
	keep <- logical(n)
	i <- which.max(len)
	while (i > 0L) {
		keep[i] <- TRUE
		i <- prev[i]
	}
	keep
}
