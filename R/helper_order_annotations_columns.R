#' Reorder columns of an annotations data.frame
#'
#' Ensures consistent column ordering: standard annotation columns first,
#' then layer columns, then search/concordance columns (if present).
#'
#' @param df Data.frame; annotations or search results data.frame.
#'
#' @return Data.frame with reordered columns.
#'
#' @export
#'
helper_order_annotations_columns <- function(df) {
	if (is.null(df)) 		{ return(NULL) }
	if (nrow(df) == 0 && ncol(df) == 0) { return(df) }

	standardCols <- c("annotationID", "tierName", "startsec", "endsec",
					   "content", "content.norm",
					   "char.orig.bytime.start", "char.orig.bytime.end",
					   "char.norm.bytime.start", "char.norm.bytime.end",
					   "char.orig.bytier.start", "char.orig.bytier.end",
					   "char.norm.bytier.start", "char.norm.bytier.end")

	searchCols <- c("resultID", "transcriptName",
					 "hit", "hit.nr", "hit.length",
					 "hit.pos.fulltext", "hit.pos.content",
					 "searchMode", "hit.span",
					 "stills.values", "stills.folder",
					 "concLeft2", "concLeft1", "concHit", "concRight1", "concRight2",
					 "nrWordsLeft", "nrWordsHitPosition", "nrWordsHit", "nrWordsRight", "nrWordsTotal")

	presentStandard <- intersect(standardCols, colnames(df))
	presentSearch   <- intersect(searchCols, colnames(df))
	layerCols       <- setdiff(colnames(df), c(standardCols, searchCols))

	df[, c(presentStandard, layerCols, presentSearch), drop = FALSE]
}
