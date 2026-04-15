#' Convert a column layer to tiers
#'
#' Converts a column in \code{t@annotations} into proper tiers. For each
#' annotation that has content in the column, a new tier is created
#' with the name \code{{mainTierName}{tierSeparator}{columnName}}.
#'
#' @param t Transcript object.
#' @param columnName Character string; name of the column in \code{t@annotations}
#'   to convert.
#' @param tierSeparator Character string; separator between main tier name and
#'   layer name in the new tier name.
#' @param insertAfter Character string or NULL. Controls where the new tier
#'   is inserted relative to other tiers of the same speaker:
#'   \code{NULL} (default) appends as last layer,
#'   \code{"main"} inserts directly after the main tier,
#'   or a layer name (e.g., \code{"trans-en"}) to insert after that layer.
#' @param mainTierNameCase Character string. Case conversion for the main tier
#'   name part: \code{"lower"} (default), \code{"upper"}, or \code{"none"}.
#'   \code{NULL} and \code{NA} are treated as \code{"none"}.
#'
#' @return The modified transcript object with new tiers and the column removed.
#'
#' @export
layers_convert_column_to_tier <- function(
	t,
	columnName,
	tierSeparator,
	insertAfter      = NULL,
	mainTierNameCase = "lower"
) {

	if (1==2) {
		columnName       <- "mm-joined"
		tierSeparator    <- "#"
		insertAfter      <- NULL
		mainTierNameCase <- "lower"
	}

	if (is.null(mainTierNameCase) || is.na(mainTierNameCase)) {
		mainTierNameCase <- "none"
	}

	if (!columnName %in% colnames(t@annotations)) return(t)

	has_content <- !is.na(t@annotations[[columnName]]) &
		nchar(trimws(as.character(t@annotations[[columnName]]))) > 0
	if (!any(has_content)) {
		t@annotations[[columnName]] <- NULL
		return(t)
	}

	source_rows <- t@annotations[has_content, ]
	speakers <- unique(source_rows$tierName)

	# ===== CREATE TIERS =====
	for (speaker in speakers) {
		main_part <- switch(mainTierNameCase,
			"lower" = tolower(speaker),
			"upper" = toupper(speaker),
			speaker
		)
		new_tier_name <- paste0(main_part, tierSeparator, columnName)
		if (new_tier_name %in% t@tiers$name) next

		speaker_pos <- which(t@tiers$name == speaker)
		if (length(speaker_pos) == 0) next

		if (is.null(insertAfter) || insertAfter != "main") {
			speaker_prefix <- tolower(paste0(speaker, tierSeparator))
			speaker_layers <- which(startsWith(tolower(t@tiers$name), speaker_prefix))

			if (!is.null(insertAfter) && insertAfter != "main") {
				ref_name <- paste0(main_part, tierSeparator, insertAfter)
				ref_pos <- which(t@tiers$name == ref_name)
				insert_pos <- if (length(ref_pos) > 0) ref_pos[1] else max(c(speaker_pos, speaker_layers))
			} else {
				insert_pos <- max(c(speaker_pos, speaker_layers))
			}
		} else {
			insert_pos <- speaker_pos
		}

		new_tier_row <- data.frame(
			name     = new_tier_name,
			type     = "IntervalTier",
			position = 0L,
			stringsAsFactors = FALSE
		)
		rownames(new_tier_row) <- new_tier_name

		if (insert_pos < nrow(t@tiers)) {
			t@tiers <- rbind(
				t@tiers[1:insert_pos, , drop = FALSE],
				new_tier_row,
				t@tiers[(insert_pos + 1):nrow(t@tiers), , drop = FALSE]
			)
		} else {
			t@tiers <- rbind(t@tiers, new_tier_row)
		}
	}

	# ===== UPDATE POSITIONS =====
	t@tiers$position <- seq_len(nrow(t@tiers))

	# ===== CREATE ANNOTATIONS =====
	new_annotations <- source_rows
	new_annotations$content <- as.character(source_rows[[columnName]])
	for (speaker in speakers) {
		main_part <- switch(mainTierNameCase,
			"lower" = tolower(speaker),
			"upper" = toupper(speaker),
			speaker
		)
		mask <- new_annotations$tierName == speaker
		new_annotations$tierName[mask] <- paste0(main_part, tierSeparator, columnName)
	}
	t@annotations <- rbind(t@annotations, new_annotations)

	# ===== REMOVE COLUMN =====
	t@annotations[[columnName]] <- NULL

	return(t)
}
