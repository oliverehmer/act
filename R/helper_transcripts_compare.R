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
#' @param content Logical; if \code{TRUE}, \code{regions} gets the columns
#'   \code{content.old} and \code{content.new}: the text of the removed and of the
#'   added annotations in the region (several joined by \code{" | "}).
#' @param pairs Logical; if \code{TRUE}, the list gets a fourth data frame
#'   \code{annotations}: one row per changed annotation of the tiers that exist in
#'   both versions. An annotation of \code{x} and one of \code{y} form a pair when
#'   they overlap in time, the largest overlap first and each annotation at most
#'   once; what stays without a partner is \code{"added"} or \code{"removed"}.
#'
#' @return List with three data frames: \code{regions} (columns \code{tierName},
#'   \code{startsec}, \code{endsec}, \code{change} with the values
#'   \code{"added"}, \code{"removed"}, \code{"changed"}; with \code{content} also
#'   \code{content.old}, \code{content.new}), \code{tiers} (columns
#'   \code{change} with the values \code{"added"}, \code{"removed"},
#'   \code{"renamed"}, \code{"moved"}, \code{tierName}, \code{tierName.new}) and
#'   \code{media} (columns \code{change}, \code{path}). With \code{pairs} also
#'   \code{annotations} (columns \code{tierName}, \code{startsec.old},
#'   \code{endsec.old}, \code{content.old}, \code{startsec.new}, \code{endsec.new},
#'   \code{content.new}, \code{change} with the values \code{"text"}, \code{"time"},
#'   \code{"text, time"}, \code{"added"}, \code{"removed"}).
#'
#' @seealso \link{helper_transcript_patch_make}
#'
#' @export
#'
#' @example inst/examples/helper_transcripts_compare.R
#'
helper_transcripts_compare <- function(x, y, gap = 0, digits = 3, content = FALSE, pairs = FALSE) {
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
	annotations <- list()
	for (tier in intersect(tiers_x, tiers_y)) {
		occ_x <- .patch_occurrence(sig_x[[tier]])
		occ_y <- .patch_occurrence(sig_y[[tier]])
		rows_x <- .compare_tier_rows(x@annotations, tier, digits)
		rows_y <- .compare_tier_rows(y@annotations, tier, digits)
		only_x <- rows_x[!(occ_x %in% occ_y), , drop = FALSE]
		only_y <- rows_y[!(occ_y %in% occ_x), , drop = FALSE]
		if (!nrow(only_x) && !nrow(only_y)) next
		if (isTRUE(pairs)) annotations[[tier]] <- .compare_pairs(only_x, only_y, tier)
		parts <- rbind(
			if (nrow(only_x)) data.frame(startsec = only_x$startsec, endsec = only_x$endsec, change = "removed", stringsAsFactors = FALSE),
			if (nrow(only_y)) data.frame(startsec = only_y$startsec, endsec = only_y$endsec, change = "added", stringsAsFactors = FALSE))
		joined <- .compare_join_regions(parts, gap)
		joined$tierName <- tier
		if (isTRUE(content)) {
			joined$content.old <- .compare_region_text(only_x, joined)
			joined$content.new <- .compare_region_text(only_y, joined)
		}
		regions[[tier]] <- joined[, c("tierName", "startsec", "endsec", "change", if (isTRUE(content)) c("content.old", "content.new"))]
	}
	regions <- if (length(regions)) do.call(rbind, unname(regions)) else
		data.frame(tierName = character(0), startsec = double(0), endsec = double(0),
				   change = character(0), stringsAsFactors = FALSE)
	if (isTRUE(content) && !nrow(regions)) { regions$content.old <- character(0); regions$content.new <- character(0) }
	rownames(regions) <- NULL

	paths_x <- unique(as.character(x@media$path))
	paths_y <- unique(as.character(y@media$path))
	media <- data.frame(
		change = c(rep("added", length(setdiff(paths_y, paths_x))), rep("removed", length(setdiff(paths_x, paths_y)))),
		path   = c(setdiff(paths_y, paths_x), setdiff(paths_x, paths_y)),
		stringsAsFactors = FALSE)

	out <- list(regions = regions, tiers = tiers, media = media)
	if (isTRUE(pairs)) {
		out$annotations <- if (length(annotations)) do.call(rbind, unname(annotations)) else .compare_pairs_empty()
		rownames(out$annotations) <- NULL
	}
	out
}

# The changed annotations of one tier as pairs (old, new): the largest time overlap
# pairs first, every annotation at most once. A split or a merge keeps the part
# with the largest overlap as the pair; the rest is added or removed.
.compare_pairs <- function(old, new, tier) {
	end_old <- ifelse(is.na(old$endsec), old$startsec, old$endsec)
	end_new <- ifelse(is.na(new$endsec), new$startsec, new$endsec)
	pair_new <- rep(NA_integer_, nrow(old))
	if (nrow(old) && nrow(new)) {
		ov <- outer(seq_len(nrow(old)), seq_len(nrow(new)), function(i, j)
			pmin(end_old[i], end_new[j]) - pmax(old$startsec[i], new$startsec[j]))
		# two points at the same time overlap by 0 and still belong together
		ov[ov < 0] <- NA
		repeat {
			if (all(is.na(ov))) break
			k <- which(ov == max(ov, na.rm = TRUE), arr.ind = TRUE)[1, ]
			pair_new[k[1]] <- k[2]
			ov[k[1], ] <- NA
			ov[, k[2]] <- NA
		}
	}
	used_new <- pair_new[!is.na(pair_new)]
	rows <- list()
	for (i in seq_len(nrow(old))) {
		j <- pair_new[i]
		if (is.na(j)) {
			rows[[length(rows) + 1L]] <- data.frame(tierName = tier, startsec.old = old$startsec[i], endsec.old = old$endsec[i],
				content.old = old$content[i], startsec.new = NA_real_, endsec.new = NA_real_, content.new = NA_character_,
				change = "removed", stringsAsFactors = FALSE)
			next
		}
		same_text <- identical(old$content[i], new$content[j])
		same_time <- identical(old$startsec[i], new$startsec[j]) && identical(old$endsec[i], new$endsec[j])
		rows[[length(rows) + 1L]] <- data.frame(tierName = tier, startsec.old = old$startsec[i], endsec.old = old$endsec[i],
			content.old = old$content[i], startsec.new = new$startsec[j], endsec.new = new$endsec[j], content.new = new$content[j],
			change = if (!same_text && !same_time) "text, time" else if (!same_text) "text" else "time", stringsAsFactors = FALSE)
	}
	for (j in setdiff(seq_len(nrow(new)), used_new)) {
		rows[[length(rows) + 1L]] <- data.frame(tierName = tier, startsec.old = NA_real_, endsec.old = NA_real_, content.old = NA_character_,
			startsec.new = new$startsec[j], endsec.new = new$endsec[j], content.new = new$content[j],
			change = "added", stringsAsFactors = FALSE)
	}
	out <- do.call(rbind, rows)
	at <- ifelse(is.na(out$startsec.new), out$startsec.old, out$startsec.new)
	out[order(at), , drop = FALSE]
}

.compare_pairs_empty <- function() {
	data.frame(tierName = character(0), startsec.old = double(0), endsec.old = double(0), content.old = character(0),
			   startsec.new = double(0), endsec.new = double(0), content.new = character(0), change = character(0),
			   stringsAsFactors = FALSE)
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
		paste(sprintf("%.*f", digits, rows$startsec), sprintf("%.*f", digits, rows$endsec), rows$content, sep = "\u241f")
	}), tiers)
}

# the text of the annotations of one side that lie in each joined region
.compare_region_text <- function(rows, joined) {
	vapply(seq_len(nrow(joined)), function(i) {
		if (!nrow(rows)) return("")
		end <- ifelse(is.na(rows$endsec), rows$startsec, rows$endsec)
		in_region <- rows$startsec >= joined$startsec[i] & end <= joined$endsec[i]
		paste(rows$content[in_region], collapse = " | ")
	}, character(1))
}

.compare_join_regions <- function(parts, gap) {
	parts <- parts[order(parts$startsec, parts$endsec), , drop = FALSE]
	out_start <- parts$startsec[1]
	out_end   <- if (is.na(parts$endsec[1])) parts$startsec[1] else parts$endsec[1]
	out_kind  <- parts$change[1]
	result <- list()
	if (nrow(parts) > 1L) for (i in 2:nrow(parts)) {
		if (isTRUE(parts$startsec[i] - out_end <= gap)) {
			out_end <- max(out_end, parts$endsec[i], na.rm = TRUE)
			if (!identical(out_kind, parts$change[i])) out_kind <- "changed"
		} else {
			result[[length(result) + 1L]] <- c(out_start, out_end)
			attr(result[[length(result)]], "change") <- out_kind
			out_start <- parts$startsec[i]
			out_end   <- if (is.na(parts$endsec[i])) parts$startsec[i] else parts$endsec[i]
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
