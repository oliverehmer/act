#' Load base styles matrix for .docx transcripts
#'
#' Loads the base styles matrix from an XLSX file (sheet `base`) or
#' a CSV file (semicolon-delimited). The matrix maps act-internal style names
#' to DOCX template paragraph style names.
#'
#' @param path Character string; path to the styles file (.xlsx or .csv).
#'   If `NULL`, the default styles file shipped with act is used.
#' @param path_docx Character string; Path to a .docx template file. If given,
#'   it will be checked if all styles defined in the matrix are present in the
#'   .docx file. If `NA` nothing will be checked.
#'
#' @return Data.frame with columns `act.style.name` and `docx.template.name`.
#'
#' @seealso \code{vignette("export_docx_styles", package = "act")}
#'
#' @export
#'
#' @example inst/examples/matrix_load.R
#'
export_styles_base_load <- function(path      = NULL,
                                    path_docx = NA) {
	if (1==2) {
		path <- NULL
		path <- '/Users/oliverehmer/Documents/R/iclo/inst/resources/export_styles.xlsx'
	}
	if (is.null(path)) {
		path <- system.file("extdata", "docx", "export_styles.xlsx", package = "act")
		if (!file.exists(path)) {
			cli::cli_abort("Unable to locate the default styles file. Please reinstall the act package.")
		}
	}

	if (!file.exists(path)) {
		cli::cli_abort("Styles file does not exist: {.path {path}}")
	}

	# ===== READ =====
	ext <- tolower(tools::file_ext(path))
	if (ext == "xlsx") {
		temp <- openxlsx::read.xlsx(path, sheet = "base")
	} else {
		temp <- readr::read_delim(path,
		                          delim = ";",
		                          locale = readr::locale(encoding = "UTF-8"),
		                          quote = "",
		                          col_types = readr::cols(.default = readr::col_character()))
	}

	# ===== TRIM =====
	cols_to_trim <- intersect(c("act.style.name", "docx.template.name"), names(temp))
	temp[cols_to_trim] <- lapply(temp[cols_to_trim], stringr::str_trim)

	# ===== CHECK =====
	# ---- obligatory columns ----
	cols_obligatory <- c("act.style.name", "docx.template.name")
	cols_missing <- cols_obligatory[!cols_obligatory %in% names(temp)]
	if (length(cols_missing) > 0) {
		cli::cli_abort("Obligatory columns missing in styles file: {.val {cols_missing}}")
	}

	# ---- obligatory styles ----
	styles_obligatory <- c("header.preface", "header.title", "header.subtitle",
	                        "header.info", "transcript.default")
	styles_missing <- styles_obligatory[!styles_obligatory %in% temp$act.style.name]
	if (length(styles_missing) > 0) {
		cli::cli_abort("Obligatory style definitions missing: {.val {styles_missing}}")
	}

	# ---- check against docx template ----
	if (!is.null(path_docx)) {
		if (!is.na(path_docx)) {
			doc <- officer::read_docx(path_docx)
			all_styles <- officer::styles_info(doc)
			styles_not_in_docx <- which(!temp$docx.template.name %in% all_styles$style_name)
			if (length(styles_not_in_docx) > 0) {
				cli::cli_abort("Styles missing in .docx template: {.val {temp$docx.template.name[styles_not_in_docx]}}")
			}
		}
	}

	attr(temp, 'path') <- path

	if (nrow(temp) == 0) {
		cli::cli_warn("The styles matrix does not contain any rows.")
	}

	return(temp)
}

#' Load user formatting rules matrix for .docx transcripts
#'
#' Loads the user formatting rules matrix from an XLSX file (sheet `user`) or
#' a CSV file (semicolon-delimited). The matrix defines per-tier rendering
#' rules for the DOCX export: visibility, line numbers, speaker acronyms,
#' paragraph style, indentation, and spacing.
#'
#' @param path Character string; path to the styles file (.xlsx or .csv).
#'   If `NULL`, the default styles file shipped with act is used.
#'
#' @details
#' Each row in the matrix defines formatting rules for tiers matching a regex
#' pattern or for a named layer column. The first matching row wins.
#'
#' **Columns:**
#'
#' *Identification:*
#' - `name` -- Character. Human-readable label for the rule (informational only).
#' - `match.regex` -- Character. Regular expression matched against tier names
#'   or internal style identifiers. First matching rule wins. Each row must
#'   have a `match.regex` value.
#'
#'   Tier names: matched against `tierName` in annotations (e.g. `#mm-body`,
#'   `^cut`). Internal identifiers: `^space$` (spacer rows between blocks).
#'
#' *Visibility:*
#' - `show` -- Logical. `TRUE` to render the tier/layer, `FALSE` to hide it.
#'   Default: `TRUE`.
#'
#' *Paragraph style:*
#' - `docx.template.name` -- Character. Name of a paragraph style defined in
#'   the .docx template file. If `NA`, uses the default transcript style.
#'
#' *Line numbers:*
#' - `line.nr.show` -- Logical. `TRUE` to show line numbers, `FALSE` to
#'   replace them with spaces (preserving alignment). Default: `TRUE`.
#'
#' *Speaker acronym:*
#' - `acronym.show` -- Logical. Show (`TRUE`) or hide (`FALSE`) the speaker
#'   acronym. Default: `TRUE`.
#' - `acronym.case` -- Character. Case transformation for the acronym.
#'   Allowed: `"toupper"`, `"tolower"`, `"capitalize"`. Default: no change.
#' - `acronym.search` -- Character. Regex search pattern applied to the
#'   acronym text.
#' - `acronym.replace` -- Character. Replacement string for
#'   `acronym.search` matches.
#' - `acronym.width` -- Numeric. Fixed character width for the acronym
#'   (padded/truncated). `0` = use global layout default. Default: `0`.
#' - `acronym.ending` -- Character. String appended after the acronym
#'   (e.g., `":"` or `" "`). If `NA`, uses the layout default.
#'
#' *Indentation (for layers):*
#' - `content.indent` -- Character. Controls indentation of layer lines.
#'   Allowed values:
#'   - `"none"` -- No indent, content starts at the left margin.
#'   - `"content"` -- Align under the content column (after prefix).
#'   - `"text"` -- Align under the first text character of the main
#'     annotation. Use `content.indent.text.skip` to skip leading symbols.
#'   - `"align"` -- Align symbols in the layer under corresponding symbols
#'     in the main annotation using `content.indent.align.char`.
#'   Default: `"none"`.
#' - `content.indent.text.skip` -- Character. Regex pattern for characters
#'   at the beginning of the main annotation to skip when calculating
#'   indent position for `content.indent = "text"`. For example,
#'   `=`, `[`, breath symbols, or pauses that should not affect
#'   the alignment of layer text.
#' - `content.indent.align.char` -- Character. One or more characters used
#'   as alignment targets for `content.indent = "align"`. Each character
#'   in the string is treated as a separate symbol to align.
#' - `content.indent.align.filler.inside` -- Character. Single character
#'   used as filler inside brackets when adjusting bracket lengths during
#'   alignment. Default: space `" "`. Must be exactly one character.
#' - `content.indent.align.mode` -- Character. Alignment mode for
#'   `content.indent = "align"`. Allowed values:
#'   - `"bracket"` -- Pairwise symbols (e.g. `|...|`, `+...+`).
#'     Filler inside brackets uses `content.indent.align.filler.inside`.
#'   - `"point"` -- Single symbol marking a point in time (e.g. `#01`).
#'     Only the symbol position is aligned, no bracket filling.
#'
#' *Wrapping:*
#' - `content.wrap` -- Logical. `TRUE` to wrap text at the transcript width,
#'   `FALSE` to allow text to extend beyond the column width without
#'   line breaks. Default: `TRUE`. Ignored for `^space$` rows.
#'
#' *Spacing:*
#' - `space.after` -- Character. Controls spacing after this tier's annotations.
#'   Allowed values:
#'   - `"block"` -- Insert an empty paragraph after the end of the block
#'     (after the last visible line that belongs to the same main annotation).
#'     This is the typical setting for transcript tiers.
#'   - `"always"` -- Insert an empty paragraph after every line of this tier,
#'     even if more lines in the same block follow.
#'   - `"no"` -- No spacing after this tier.
#'   Default: `NA` (no spacing).
#'
#' *Other:*
#' - `comment` -- Character. Free-text comment (ignored by the code).
#'
#' @return Data.frame with tier formatting rules.
#'
#' @seealso \code{vignette("export_docx_styles", package = "act")}
#'
#' @export
#'
export_styles_user_load <- function(path = NULL) {
	if (1==2) {
		path <- NULL
	}
	if (is.null(path)) {
		path <- system.file("extdata", "docx", "export_styles.xlsx", package = "act")
		if (!file.exists(path)) {
			cli::cli_abort("Unable to locate the default styles file. Please reinstall the act package.")
		}
	}

	if (!file.exists(path)) {
		cli::cli_abort("Styles file does not exist: {.path {path}}")
	}

	package_path <- system.file("extdata", "docx", "export_styles.xlsx", package = "act")
	if (path != package_path && file.exists(package_path)) {
		if (!.validate_resource(path, package_path)) {
			path <- package_path
		}
	}

	# ===== READ =====
	ext <- tolower(tools::file_ext(path))
	if (ext == "xlsx") {
		sheets <- openxlsx::getSheetNames(path)
		if (!"user" %in% sheets) {
			temp <- data.frame(stringsAsFactors = FALSE)
		} else {
			temp <- openxlsx::read.xlsx(path, sheet = "user")
		}
	} else {
		temp <- readr::read_delim(path,
		                          delim = ";",
		                          locale = readr::locale(encoding = "UTF-8"),
		                          quote = "",
		                          col_types = readr::cols(.default = readr::col_character()))
	}

	if (is.null(temp)) {
		temp <- data.frame(stringsAsFactors = FALSE)
	}

	# ===== BACKWARD COMPATIBILITY =====
	if ("layer.name" %in% names(temp) && !"match.regex" %in% names(temp)) {
		names(temp)[names(temp) == "layer.name"] <- "match.regex"
	}
	if ("column.name" %in% names(temp) && !"match.regex" %in% names(temp)) {
		names(temp)[names(temp) == "column.name"] <- "match.regex"
	}
	if ("tier.regex" %in% names(temp) && !"match.regex" %in% names(temp)) {
		names(temp)[names(temp) == "tier.regex"] <- "match.regex"
	}
	if ("content.indent.skip" %in% names(temp) && !"content.indent.text.skip" %in% names(temp)) {
		names(temp)[names(temp) == "content.indent.skip"] <- "content.indent.text.skip"
	}
	if ("content.align.char" %in% names(temp) && !"content.indent.align.char" %in% names(temp)) {
		names(temp)[names(temp) == "content.align.char"] <- "content.indent.align.char"
	}

	# ===== ENSURE COLUMNS =====
	all_cols <- c("name", "show", "match.regex", "docx.template.name",
	              "line.nr.show", "acronym.show", "acronym.case",
	              "acronym.search", "acronym.replace", "acronym.width",
	              "acronym.ending", "content.indent", "content.indent.text.skip",
	              "content.indent.align.char", "content.indent.align.filler.inside",
	              "content.indent.align.mode", "content.indent.align.arrow",
	              "content.wrap", "space.after", "comment")
	for (col in all_cols) {
		if (!col %in% names(temp)) {
			temp[[col]] <- NA
		}
	}

	if (nrow(temp) == 0) {
		attr(temp, 'path') <- path
		return(temp)
	}

	# ===== CLEAN =====
	char_cols <- intersect(c("name", "match.regex", "docx.template.name",
	                         "acronym.case", "acronym.search", "acronym.replace",
	                         "acronym.ending", "content.indent", "content.indent.text.skip",
	                         "content.indent.align.char", "content.indent.align.filler.inside",
	                         "content.indent.align.mode", "content.indent.align.arrow",
	                         "space.after", "comment"), names(temp))
	temp[char_cols] <- lapply(temp[char_cols], function(x) {
		x <- as.character(x)
		x[!is.na(x) & (x == "" | grepl("^#[A-Z]", x))] <- NA_character_
		x
	})
	other_char_cols <- setdiff(names(temp)[vapply(temp, is.character, logical(1))], char_cols)
	if (length(other_char_cols) > 0) {
		temp[other_char_cols] <- lapply(temp[other_char_cols], function(x) {
			x[!is.na(x) & (x == "" | grepl("^#[A-Z]", x))] <- NA_character_
			x
		})
	}

	# ===== TYPE CONVERSION =====
	temp$show          <- as.logical(temp$show)
	if ("is.main.tier" %in% names(temp)) {
		temp$is.main.tier <- as.logical(temp$is.main.tier)
	} else {
		temp$is.main.tier <- FALSE
	}
	temp$line.nr.show  <- as.logical(temp$line.nr.show)
	temp$acronym.show  <- as.logical(temp$acronym.show)
	temp$acronym.width <- as.numeric(temp$acronym.width)
	if ("content.wrap" %in% names(temp)) {
		temp$content.wrap <- as.logical(temp$content.wrap)
		temp$content.wrap[is.na(temp$content.wrap)] <- TRUE
	} else {
		temp$content.wrap <- TRUE
	}
	if ("content.indent.align.filler.inside" %in% names(temp)) {
		temp$content.indent.align.filler.inside[is.na(temp$content.indent.align.filler.inside)] <- " "
		temp$content.indent.align.filler.inside <- substr(temp$content.indent.align.filler.inside, 1, 1)
	}

	# ===== VALIDATE =====
	# ---- each row needs match.regex ----
	for (i in seq_len(nrow(temp))) {
		if (is.na(temp$match.regex[i])) {
			cli::cli_abort("Row {i} in user matrix has no {.field match.regex} set.")
		}
	}

	# ---- validate content.indent values ----
	valid_indent <- c("none", "content", "text", "align")
	bad_indent <- which(!is.na(temp$content.indent) & !temp$content.indent %in% valid_indent)
	if (length(bad_indent) > 0) {
		cli::cli_abort("Invalid {.field content.indent} values: {.val {temp$content.indent[bad_indent]}}. Allowed: {.val {valid_indent}}")
	}

	# ---- validate acronym.case values ----
	valid_case <- c("toupper", "tolower", "capitalize")
	bad_case <- which(!is.na(temp$acronym.case) & !temp$acronym.case %in% valid_case)
	if (length(bad_case) > 0) {
		cli::cli_abort("Invalid {.field acronym.case} values: {.val {temp$acronym.case[bad_case]}}. Allowed: {.val {valid_case}}")
	}

	# ---- validate content.indent.align.mode values ----
	valid_align_mode <- c("bracket", "point")
	bad_align <- which(!is.na(temp$content.indent.align.mode) & !temp$content.indent.align.mode %in% valid_align_mode)
	if (length(bad_align) > 0) {
		cli::cli_abort("Invalid {.field content.indent.align.mode} values: {.val {temp$content.indent.align.mode[bad_align]}}. Allowed: {.val {valid_align_mode}}")
	}

	# ---- validate space.after values ----
	valid_space_after <- c("always", "block", "no")
	bad_space <- which(!is.na(temp$space.after) & !temp$space.after %in% valid_space_after)
	if (length(bad_space) > 0) {
		cli::cli_abort("Invalid {.field space.after} values: {.val {temp$space.after[bad_space]}}. Allowed: {.val {valid_space_after}}")
	}

	# ---- warn about duplicate match.regex ----
	match_regexes <- temp$match.regex[!is.na(temp$match.regex)]
	dupes <- unique(match_regexes[duplicated(match_regexes)])
	if (length(dupes) > 0) {
		cli::cli_warn("Duplicate {.field match.regex} entries: {.val {dupes}}. First match will be used.")
	}

	attr(temp, 'path') <- path
	return(temp)
}
