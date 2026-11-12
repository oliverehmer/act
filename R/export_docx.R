#' Export print transcript in .docx format
#'
#' Writes a print transcript as a Word file. Everything about its look comes
#' from a transcript profile (parameter \code{style}): mode (gat or score),
#' width, line numbers, speaker acronyms, tier and character styles, the Word
#' template and the advanced engine settings. Profiles are JSON files; the
#' ones shipped with act are listed by \code{helper_style_list()}, and
#' \code{helper_style_read()} reads one for inspection or modification.
#'
#' The Word styles of the profile are looked up in the template of the
#' profile (\code{word$file}); with \code{word$look = "profile"} styles
#' missing there are created from the profile.
#'
#' @param t Transcript object.
#' @param pathOutput Character string; path where to save the transcript.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param insertArrowStartsec Numeric; start time (seconds) of the hit annotation for arrow placement. The annotation is marked with the arrow of the profile; all lines get room for it before the line number. Used with \code{insertArrowEndsec} and \code{insertArrowTierName} to locate the annotation by time and tier. If \code{NA}, no arrow is placed.
#' @param insertArrowEndsec Numeric; end time (seconds) of the hit annotation for arrow placement.
#' @param insertArrowTierName Character string; tier name of the hit annotation for arrow placement.
#' @param headerPreface Character string; text used as preface before title.
#' @param headerTitle Character string; text used as title.
#' @param headerSubtitle Character string; text  used as sub title.
#' @param headerDescription Character string; text used as description after sub title.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.
#' @param layerOrder Vector of character strings; order of the multimodal layers within a block. \code{NULL} keeps the tier order of the annotation file.
#' @param report Logical; if \code{TRUE} and \code{pathOutput} is set, an alignment report is written next to the output file.
#' @param pathReport Character string; explicit path for the alignment report.
#' @param mainTierNames Vector of character strings; exact names of the tiers to treat as main tiers. \code{NULL} derives the main flag from the tier styles of the profile; without any tier style every tier counts as a main tier.
#' @param alignChars Named vector of character strings; anchor characters per layer tier (names = tier names, values = the characters). \code{NULL} derives them from the tier styles of the profile.
#' @param alignModes Named vector of character strings; alignment mode per layer tier (\code{"bracket"} or \code{"point"}). \code{NULL} derives the mode from the tier styles of the profile.
#' @param style Transcript profile: the name of a profile, the path of a profile file or a profile read with \code{helper_style_read}. \code{NULL} uses the profile \code{"act"}.
#' @param headerTable Data frame or \code{NULL}; a small table put right after the preface (e.g. a status table to tick). The column names are the head row, every row of the data frame a table row, all cells as text. The cells take the Word style of the preface, the columns fit their text. \code{NULL} adds no table.
#'
#' @return Officer doc; transcript as object from library officer.
#'
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}, \code{vignette("export_docx_styles", package = "act")}
#'
#' @export
#'
#' @example inst/examples/export_docx.R
#'
export_docx <- function (   t,
							pathOutput                   = NULL,
							filterTierNames              = NULL,
							filterSectionStartsec        = NULL,
							filterSectionEndsec          = NULL,
							insertArrowStartsec          = NA_real_,
							insertArrowEndsec            = NA_real_,
							insertArrowTierName          = NA_character_,
							headerPreface                = NULL,
							headerTitle                  = NULL,
							headerSubtitle               = NULL,
							headerDescription            = NULL,
							headerInsertSource           = TRUE,
							layerOrder                   = NULL,
							report                       = FALSE,
							pathReport                   = NULL,
							mainTierNames                = NULL,
							alignChars                   = NULL,
							alignModes                   = NULL,
							style                        = NULL,
							headerTable                  = NULL
) {
	.assert_transcript(t, missing = missing(t))
	l <- .style_layout(style)
	profile <- .layout_style(l)
	if (!requireNamespace("officer", quietly = TRUE)) {
		cli::cli_abort("Please install the {.pkg officer} package.")
	}
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			cli::cli_abort("Output folder does not exist. Modify parameter {.arg pathOutput}.")
		}
	}
	if (is.na(l[["transcript.width"]]) || l[["transcript.width"]] == -1) {
	} else if (l[["transcript.width"]] < 40) {
		cli::cli_abort("The width of the transcript is to low. Minimum is 40. Check {.field width} of the profile.")
	}
	if (is.na(l[["speaker.width"]]) || l[["speaker.width"]] == -1) {
	} else if (l[["speaker.width"]] == 0 || l[["speaker.width"]] < -1) {
		cli::cli_abort("Length of tier names is to short. Minimum is 1. Check {.field acronym$width} of the profile.")
	} else if (l[["speaker.width"]] > 25) {
		cli::cli_abort("Length of tier names is to long. Maximum is 25. Check {.field acronym$width} of the profile.")
	}

	templates <- .layout_docx_templates_resolve(l)
	template_suffixes <- if (length(templates) <= 1) {
		""
	} else {
		paste0("__", names(templates))
	}

	rendered <- .layout_render(t, l,
		filterTierNames       = filterTierNames,
		filterSectionStartsec = filterSectionStartsec,
		filterSectionEndsec   = filterSectionEndsec,
		layerOrder            = layerOrder,
		mainTierNames         = mainTierNames,
		alignChars            = alignChars,
		alignModes            = alignModes,
		insertArrowStartsec   = insertArrowStartsec,
		insertArrowEndsec     = insertArrowEndsec,
		insertArrowTierName   = insertArrowTierName)
	t <- rendered$transcript
	plan <- rendered$plan
	result <- rendered$result
	mondada <- identical(rendered$layoutMode, "mondada")

	symbol_chars <- .docx_symbol_chars(result)

	results <- list()
	for (template_idx in seq_along(templates)) {
	doc <- officer::read_docx(path = templates[template_idx])
	from_profile <- .docx_profile_styles(doc, profile, symbol_chars)
	doc <- from_profile$doc
	character_rules <- from_profile$rules
	layer_symbols <- .docx_layer_symbol_styles(doc, profile, result, plan, rendered$anchors, character_rules)
	doc <- layer_symbols$doc
	if (length(from_profile$created) > 0 && identical(profile$word$look, "file")) {
		cli::cli_alert_info("Styles missing in the Word file, created from the profile: {.val {from_profile$created}}")
	}
	paragraphs <- .docx_paragraphs_new(doc)
	paragraphs$direct <- layer_symbols$direct
	.docx_add_header(paragraphs, l, t, headerPreface, headerTitle,
	                 headerSubtitle, headerDescription,
	                 headerInsertSource, headerTable)

	if (!is.null(result)) {
		style_default_name <- .layout_style_base_get(l, "transcript.default")$docx.template.name
		space_style_row <- get_style_user(l, name = "space")
		space_style_name <- if (!is.null(space_style_row) &&
		                        !is.na(space_style_row$docx.template.name)) {
			space_style_row$docx.template.name
		} else {
			style_default_name
		}
		previous_main_row <- NA_integer_
		emitted_any <- FALSE
		space_lines <- isTRUE(profile$space.lines)
		line_in_row <- stats::ave(seq_len(nrow(plan)), plan$row, FUN = seq_along)
		for (p in seq_len(nrow(plan))) {
			row_p <- plan$row[p]
			if (mondada) {
				if (space_lines && emitted_any && isTRUE(result$is_main[row_p])) {
					.docx_par_add(paragraphs, "", space_style_name)
				}
			} else if (space_lines && emitted_any && isTRUE(result$is_main[row_p]) &&
			           !identical(row_p, previous_main_row)) {
				.docx_par_add(paragraphs, "", space_style_name)
			}
			if (isTRUE(result$show[row_p])) {
				.docx_add_line_rules(paragraphs, plan$line[p], result$style[row_p],
				                     .docx_rules_of_row(character_rules, layer_symbols$styles, as.character(result$tierName[row_p])),
				                     owners = .docx_symbol_owners(layer_symbols$styles, rendered$anchors, row_p, line_in_row[p], plan$line[p]))
				if (isTRUE(result$is_main[row_p])) previous_main_row <- row_p
				emitted_any <- TRUE
			}
		}
		if (emitted_any) {
			.docx_par_add(paragraphs, "", space_style_name)
		}
	}
	doc <- .docx_paragraphs_insert(doc, paragraphs)

	if (!is.null(pathOutput)) {
		base_path <- tools::file_path_sans_ext(pathOutput)
		suffix    <- template_suffixes[template_idx]
		if (nzchar(suffix)) {
			date_re <- "__\\d{4}-\\d{2}-\\d{2}[a-z]?$"
			m <- regmatches(base_path, regexec(date_re, base_path))[[1]]
			if (length(m) > 0L && nchar(m[1]) > 0L) {
				base_without_date <- substr(base_path, 1L, nchar(base_path) - nchar(m[1]))
				output_path <- paste0(base_without_date, suffix, m[1], ".docx")
			} else {
				output_path <- paste0(base_path, suffix, ".docx")
			}
		} else {
			output_path <- paste0(base_path, ".docx")
		}
		print(x = doc, target = output_path)
	}

	results[[template_idx]] <- doc
	}

	if (is.null(pathReport) && isTRUE(report)) {
		pathReport <- alignment_report_path(pathOutput)
	}
	if (!is.null(pathReport) && !is.null(result)) {
		report_lines <- build_alignment_report(
			result, plan, transcript_name = t@name,
			layout_mode = rendered$layoutMode,
			text_body_width = rendered$engineWidth,
			time_tolerance = profile$advanced$tolerance.gesture)
		con <- file(pathReport, open = "w", encoding = "UTF-8")
		writeLines(report_lines, con = con)
		close(con)
	}
	if (!is.null(result)) {
		report_render_warnings(result, transcript_name = t@name)
	}

	if (length(results) == 1) return(results[[1]])
	return(results)
}

.docx_add_header <- function(paragraphs, l, t, headerPreface, headerTitle,
                             headerSubtitle, headerDescription,
                             headerInsertSource, headerTable = NULL) {
	if (!isTRUE(l[["header.insert"]])) return(invisible(NULL))
	add_block <- function(value, style_name) {
		value <- as.character(value)
		value <- value[!is.na(value)]
		if (length(value) == 0 || !any(nzchar(value))) return(invisible(NULL))
		value <- paste(value, collapse = "\n")
		style <- .layout_style_base_get(l, style_name)$docx.template.name
		for (line in unlist(stringr::str_split(value, "\n"))) {
			.docx_par_add(paragraphs, line, style)
		}
	}
	add_block(headerPreface,     "header.preface")
	if (is.data.frame(headerTable) && ncol(headerTable) > 0) {
		.docx_table_add(paragraphs, headerTable, .layout_style_base_get(l, "header.preface")$docx.template.name)
	}
	add_block(headerTitle,       "header.title")
	add_block(headerSubtitle,    "header.subtitle")
	add_block(headerDescription, "header.info")
	if (isTRUE(headerInsertSource) && nrow(t@annotations) > 0) {
		source_line <- paste0("(", t@name, ", ",
			helper_time_format(min(t@annotations$startsec), format = .layout_time_format(l)), "-",
			helper_time_format(max(t@annotations$endsec), format = .layout_time_format(l)), ")")
		.docx_par_add(paragraphs, source_line, .layout_style_base_get(l, "header.subtitle")$docx.template.name)
	}
	invisible(NULL)
}

# ===== THE PARAGRAPHS OF THE DOCUMENT =====
# officer::body_add_par() looks for its place in the document and copies the list
# of all paragraphs for every paragraph: 333 s for 6288 annotations, 17 s when they
# are collected as text and go in at once. The XML is the same, byte for byte.
.docx_paragraphs_new <- function(doc) {
	paragraphs <- new.env(parent = emptyenv())
	paragraphs$styles <- officer::styles_info(doc, type = "paragraph")
	paragraphs$xml <- character(1024)
	paragraphs$n <- 0L
	paragraphs
}

.docx_paragraph_put <- function(paragraphs, xml) {
	force(xml)
	paragraphs$n <- paragraphs$n + 1L
	if (paragraphs$n > length(paragraphs$xml)) length(paragraphs$xml) <- 2L * length(paragraphs$xml)
	paragraphs$xml[paragraphs$n] <- xml
	invisible(NULL)
}

.docx_style_id <- function(paragraphs, style) {
	styles <- paragraphs$styles
	if (is.null(style)) return(styles$style_id[which(styles$is_default)[1]])
	i <- match(style, styles$style_name)
	if (is.na(i)) {
		cli::cli_abort("Could not match any paragraph style named {.val {style}} in the Word file: {.val {styles$style_name}}.")
	}
	styles$style_id[i]
}

# text of a paragraph, as officer writes it (.docx_xml_escape also takes quotes, for attributes)
.docx_text_escape <- function(text) {
	text <- enc2utf8(as.character(text))
	text <- gsub("&", "&amp;", text, fixed = TRUE, useBytes = TRUE)
	text <- gsub("<", "&lt;", text, fixed = TRUE, useBytes = TRUE)
	text <- gsub(">", "&gt;", text, fixed = TRUE, useBytes = TRUE)
	Encoding(text) <- "UTF-8"
	text
}

.docx_par_add <- function(paragraphs, value, style) {
	.docx_paragraph_put(paragraphs, paste0("<w:p><w:pPr><w:pStyle w:val=\"", .docx_style_id(paragraphs, style),
		"\"/></w:pPr><w:r><w:t xml:space=\"preserve\">", .docx_text_escape(value), "</w:t></w:r></w:p>"))
}

# A small table in the paragraph style of the preface: thin grey lines, no
# font of its own. Word fits the columns to their text (autofit, cell widths
# auto); the grid is a first guess from the longest text of each column for
# programs that do not fit on opening. The last column may wrap, the others not.
.docx_table_add <- function(paragraphs, table, style) {
	style_id <- .docx_style_id(paragraphs, style)
	head <- names(table)
	cells <- rbind(head, as.matrix(table))
	cells[is.na(cells)] <- ""
	n_col <- ncol(cells)
	widths <- round(apply(cells, 2, function(col) max(nchar(col), 3)) * 110 + 200)
	border <- "w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"808080\""
	cell_xml <- function(text, j, bold) paste0(
		"<w:tc><w:tcPr><w:tcW w:w=\"0\" w:type=\"auto\"/>", if (j < n_col) "<w:noWrap/>" else "", "</w:tcPr>",
		"<w:p><w:pPr><w:pStyle w:val=\"", style_id, "\"/><w:spacing w:before=\"0\" w:after=\"0\"/>",
		"<w:ind w:left=\"0\" w:hanging=\"0\"/><w:jc w:val=\"", if (j == 1 || j == n_col) "left" else "center", "\"/></w:pPr>",
		"<w:r>", if (bold) "<w:rPr><w:b/></w:rPr>" else "", "<w:t xml:space=\"preserve\">",
		.docx_text_escape(text), "</w:t></w:r></w:p></w:tc>")
	rows <- vapply(seq_len(nrow(cells)), function(i) paste0("<w:tr>", paste0(vapply(seq_len(n_col), function(j)
		cell_xml(cells[i, j], j, bold = i == 1 && j == 1), character(1)), collapse = ""), "</w:tr>"), character(1))
	.docx_paragraph_put(paragraphs, paste0(
		"<w:tbl><w:tblPr><w:tblW w:w=\"0\" w:type=\"auto\"/><w:tblBorders>",
		paste0("<w:", c("top", "left", "bottom", "right", "insideH", "insideV"), " ", border, "/>", collapse = ""),
		"</w:tblBorders><w:tblLayout w:type=\"autofit\"/><w:tblCellMar><w:left w:w=\"70\" w:type=\"dxa\"/>",
		"<w:right w:w=\"70\" w:type=\"dxa\"/></w:tblCellMar><w:tblLook w:val=\"0000\"/></w:tblPr><w:tblGrid>",
		paste0("<w:gridCol w:w=\"", widths, "\"/>", collapse = ""), "</w:tblGrid>",
		paste0(rows, collapse = ""), "</w:tbl>"))
}

.docx_fpar_add <- function(paragraphs, value, style) {
	.docx_paragraph_put(paragraphs, officer::to_wml(value, add_ns = FALSE, style_id = .docx_style_id(paragraphs, style)))
}

.docx_paragraphs_insert <- function(doc, paragraphs) {
	if (paragraphs$n == 0L) return(doc)
	body <- xml2::xml_find_first(officer::docx_body_xml(doc), "/w:document/w:body")
	parsed <- xml2::read_xml(paste0(
		"<w:body xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"",
		" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\"",
		" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"",
		" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">",
		paste(paragraphs$xml[seq_len(paragraphs$n)], collapse = ""), "</w:body>"))
	section <- xml2::xml_find_first(body, "w:sectPr")
	for (paragraph in xml2::xml_children(parsed)) {
		if (inherits(section, "xml_missing")) xml2::xml_add_child(body, paragraph)
		else xml2::xml_add_sibling(section, paragraph, .where = "before")
	}
	# officer's cursor must know them, or what a caller adds to the returned document
	# lands before the transcript. Reading the file back instead would turn on
	# evenAndOddHeaders in the next print (officer reads w:val="0" as set).
	blocks <- xml2::xml_name(xml2::xml_children(body))
	blocks <- blocks[blocks != "sectPr"]
	doc$officer_cursor$nodes_names <- blocks
	doc$officer_cursor$which <- length(blocks)
	doc
}

#==== FUNCTONS ====
# Row of the base styles table for an act style name, e.g. "transcript.default".
.layout_style_base_get <- function(l, actStyleName) {
	id <- which(l[["docx.styles.base"]]$act.style.name==actStyleName)
	if (length(id)==0) {
		cli::cli_abort("Style {.val {actStyleName}} is not defined in your styles file. Add this style to your base styles.")
	} else {
		return (
			l[["docx.styles.base"]][id[1],]
		)
	}
}

# ===== MULTIMODAL SYMBOLS AS CHARACTER STYLE =====
# The symbols are the anchor characters of the layer rows: what the engine
# aligns is what gets highlighted, so no second symbol list is needed.
.docx_symbol_chars <- function(result) {
	if (is.null(result) || is.null(result$align_chars)) return(character(0))
	chars <- result$align_chars[!is.na(result$align_chars)]
	unique(unlist(lapply(chars, helper_text_graphemes_split)))
}

get_style_user <- function(l, name) {
	user_df <- l[["docx.styles.user"]]

	if (nrow(user_df) > 0 && "match.regex" %in% names(user_df)) {
		match_rows <- which(!is.na(user_df$match.regex))
		# the last matching row wins: general styles above, special ones below
		for (idx in rev(match_rows)) {
			if (stringr::str_detect(name, user_df$match.regex[idx])) {
				return(user_df[idx, , drop = FALSE])
			}
		}
	}
	return(data.frame(
		name              = "default",
		show              = TRUE,
		match.regex       = NA_character_,
		docx.template.name = NA_character_,
		line.nr.show      = NA,
		acronym.show      = TRUE,
		acronym.case      = NA_character_,
		acronym.search    = NA_character_,
		acronym.replace   = NA_character_,
		acronym.width     = 0,
		acronym.ending    = NA_character_,
		content.indent    = NA_character_,
		content.indent.text.skip        = NA_character_,
		content.indent.align.char       = NA_character_,
		content.indent.align.filler.inside = NA_character_,
		content.indent.align.mode       = NA_character_,
		content.wrap      = TRUE,
		content.join      = FALSE,
		content.join.separator = NA_character_,
		space.after       = NA_character_,
		comment           = NA_character_,
		stringsAsFactors  = FALSE
	))
}

# ---- shared prerender: prepare the aligned annotation frame -------------
# Produces the styled + bracket/layer-aligned annotations (fixed
# transcript.width) shared by export_docx() and the transcript viewer.
