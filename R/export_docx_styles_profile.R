# Word styles from a style profile. The styles are written straight into
# word/styles.xml of the document: only the six appearance values of a style
# are touched, so spacing, tabs and everything else a Word file defines for
# a style stays as it is (officer's own style setters replace the whole
# paragraph definition).

.DOCX_RPR_ORDER <- c("rStyle", "rFonts", "b", "bCs", "i", "iCs", "caps", "smallCaps", "strike",
                     "dstrike", "outline", "shadow", "emboss", "imprint", "noProof", "snapToGrid",
                     "vanish", "webHidden", "color", "spacing", "w", "kern", "position", "sz",
                     "szCs", "highlight", "u", "effect", "bdr", "shd", "fitText", "vertAlign",
                     "rtl", "cs", "em", "lang", "eastAsianLayout", "specVanish", "oMath")
.DOCX_PPR_ORDER <- c("pStyle", "keepNext", "keepLines", "pageBreakBefore", "framePr", "widowControl",
                     "numPr", "suppressLineNumbers", "pBdr", "shd", "tabs", "suppressAutoHyphens",
                     "kinsoku", "wordWrap", "overflowPunct", "topLinePunct", "autoSpaceDE",
                     "autoSpaceDN", "bidi", "adjustRightInd", "snapToGrid", "spacing", "ind",
                     "contextualSpacing", "mirrorIndents", "suppressOverlap", "jc", "textDirection",
                     "textAlignment", "textboxTightWrap", "outlineLvl", "divId", "cnfStyle", "rPr",
                     "sectPr", "pPrChange")
.DOCX_W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

# Brings the styles of a profile into the document.
# look "profile": every style of the profile is written (created or its
# appearance replaced). look "file": the Word file rules; only styles it
# does not have are created from the profile.
# Returns the document, the names of the created styles and the character
# rules (pattern + style id) for the lines.
.docx_profile_styles <- function(doc, profile, symbol_chars = character(0)) {
	styles_file <- file.path(doc$package_dir, "word", "styles.xml")
	xml <- xml2::read_xml(styles_file)
	from_profile <- identical(profile$word$look, "profile")
	default_word <- .style_by_role(profile, "default")$word
	created <- character(0)
	written <- character(0)
	rules <- list()
	for (s in profile$styles) {
		if (identical(s$role, "normal") || !nzchar(s$word)) next
		if (!nzchar(s$role) && !isTRUE(s$active)) next
		type <- if (identical(s$type, "character")) "character" else "paragraph"
		node <- .docx_style_node(xml, s$word, type)
		exists <- !inherits(node, "xml_missing")
		if (!exists) {
			based_on <- if (type == "character") NA_character_ else if (!nzchar(s$role) || identical(s$role, "space")) default_word else "Normal"
			node <- .docx_style_create(xml, s$word, type, based_on, keep_next = !identical(s$role, "space"))
			created <- c(created, s$word)
		}
		# two styles of one Word name: the upper one sets the look, as in the viewer
		key <- paste(type, s$word)
		if ((from_profile || !exists) && !key %in% written) .docx_style_appearance(node, s, type)
		written <- c(written, key)
		if (type == "character") {
			pattern <- if (identical(s$applies, "symbols")) .docx_symbol_class(symbol_chars) else s$pattern
			if (!is.na(pattern) && nzchar(pattern)) {
				rules[[length(rules) + 1]] <- list(pattern = pattern, style_id = xml2::xml_attr(node, "styleId"))
			}
		}
	}
	xml2::write_xml(xml, file = styles_file)
	# officer keeps a table of the styles and looks names up there
	read_styles <- utils::getFromNamespace("read_docx_styles", "officer")
	doc$styles <- read_styles(doc$package_dir)
	list(doc = doc, created = unique(created), rules = rules)
}

.docx_symbol_class <- function(symbol_chars) {
	if (length(symbol_chars) == 0) return(NA_character_)
	paste0("[", paste(stringr::str_escape(symbol_chars), collapse = ""), "]+")
}

.docx_style_node <- function(xml, name, type) {
	nodes <- xml2::xml_find_all(xml, sprintf("/w:styles/w:style[@w:type='%s']", type))
	names_found <- xml2::xml_attr(xml2::xml_find_first(nodes, "w:name"), "val")
	hit <- which(names_found == name)
	if (length(hit) == 0) return(xml2::xml_missing())
	nodes[[hit[1]]]
}

.docx_style_create <- function(xml, name, type, based_on, keep_next = TRUE) {
	ids <- xml2::xml_attr(xml2::xml_find_all(xml, "/w:styles/w:style"), "styleId")
	id <- stringr::str_replace_all(name, "[^A-Za-z0-9]", "")
	if (!nzchar(id)) id <- "style"
	base_id <- id
	n <- 1
	while (id %in% ids) {
		n <- n + 1
		id <- paste0(base_id, n)
	}
	based <- ""
	if (!is.na(based_on)) {
		base_node <- .docx_style_node(xml, based_on, "paragraph")
		if (!inherits(base_node, "xml_missing")) {
			based <- sprintf("<w:basedOn w:val=\"%s\"/>", xml2::xml_attr(base_node, "styleId"))
		}
	}
	# A new line style must not take spacing and justification of Normal:
	# the lines of a transcript stand directly under each other, flush left,
	# and stay together on a page (a space line is where a page may break).
	# No spell check on transcript text.
	spacing <- if (type == "paragraph") {
		paste0("<w:pPr><w:keepNext w:val=\"", if (isTRUE(keep_next)) "1" else "0", "\"/>",
		       "<w:spacing w:before=\"0\" w:after=\"0\" w:line=\"240\" w:lineRule=\"auto\"/><w:jc w:val=\"left\"/></w:pPr>",
		       "<w:rPr><w:noProof/></w:rPr>")
	} else {
		""
	}
	code <- paste0(sprintf("<w:style xmlns:w=\"%s\" w:type=\"%s\" w:customStyle=\"1\" w:styleId=\"%s\">", .DOCX_W_NS, type, id),
	               sprintf("<w:name w:val=\"%s\"/>", .docx_xml_escape(name)), based, "<w:qFormat/>", spacing, "</w:style>")
	root <- xml2::xml_find_first(xml, "/w:styles")
	xml2::xml_add_child(root, xml2::read_xml(code))
	.docx_style_node(xml, name, type)
}

.docx_xml_escape <- function(x) {
	x <- stringr::str_replace_all(x, "&", "&amp;")
	x <- stringr::str_replace_all(x, "<", "&lt;")
	x <- stringr::str_replace_all(x, ">", "&gt;")
	stringr::str_replace_all(x, "\"", "&quot;")
}

# Sets font, size, color, background, italic and bold of a style node to
# what the style itself states. A value the style does not state is taken
# out, so Word takes it from the style this one is based on.
.docx_style_appearance <- function(node, s, type) {
	rpr <- .docx_child_ensure(node, "rPr")
	xml2::xml_remove(xml2::xml_find_all(rpr, "w:rFonts|w:b|w:bCs|w:i|w:iCs|w:color|w:sz|w:szCs|w:shd"))
	add <- function(parent, name, attrs) .docx_xml_add(parent, name, attrs, if (identical(xml2::xml_name(parent), "pPr")) .DOCX_PPR_ORDER else .DOCX_RPR_ORDER)
	if (!is.null(s$font)) {
		add(rpr, "rFonts", list(ascii = s$font, hAnsi = s$font, cs = s$font, eastAsia = s$font))
	}
	for (key in c("bold", "italic")) {
		if (is.null(s[[key]])) next
		tag <- if (key == "bold") "b" else "i"
		value <- if (isTRUE(s[[key]])) "1" else "0"
		add(rpr, tag, list(val = value))
		add(rpr, paste0(tag, "Cs"), list(val = value))
	}
	if (!is.null(s$color)) add(rpr, "color", list(val = toupper(sub("^#", "", s$color))))
	if (!is.null(s$size)) {
		half_points <- as.character(round(as.numeric(s$size) * 2))
		add(rpr, "sz", list(val = half_points))
		add(rpr, "szCs", list(val = half_points))
	}
	shade <- if (is.null(s$background)) NULL else list(val = "clear", color = "auto", fill = toupper(sub("^#", "", s$background)))
	if (type == "character") {
		if (!is.null(shade)) add(rpr, "shd", shade)
	} else {
		ppr <- .docx_child_ensure(node, "pPr")
		xml2::xml_remove(xml2::xml_find_all(ppr, "w:shd"))
		if (!is.null(shade)) add(ppr, "shd", shade)
		if (length(xml2::xml_children(ppr)) == 0) xml2::xml_remove(ppr)
	}
	if (length(xml2::xml_children(rpr)) == 0) xml2::xml_remove(rpr)
	invisible(node)
}

# Adds a property element at its place: Word rejects a file whose property
# elements stand in another order than the schema names them.
.docx_xml_add <- function(parent, name, attrs, order_names) {
	children <- xml2::xml_children(parent)
	rank <- match(xml2::xml_name(children), order_names)
	later <- which(!is.na(rank) & rank > match(name, order_names))
	child <- if (length(later) == 0) {
		xml2::xml_add_child(parent, paste0("w:", name))
	} else {
		xml2::xml_add_sibling(children[[later[1]]], paste0("w:", name), .where = "before")
	}
	for (key in names(attrs)) xml2::xml_set_attr(child, paste0("w:", key), attrs[[key]])
	invisible(child)
}

# pPr must stand before rPr inside a style
.docx_child_ensure <- function(node, name) {
	child <- xml2::xml_find_first(node, paste0("w:", name))
	if (!inherits(child, "xml_missing")) return(child)
	if (name == "pPr") {
		rpr <- xml2::xml_find_first(node, "w:rPr")
		if (!inherits(rpr, "xml_missing")) {
			xml2::xml_add_sibling(rpr, "w:pPr", .where = "before")
			return(xml2::xml_find_first(node, "w:pPr"))
		}
	}
	xml2::xml_add_child(node, paste0("w:", name))
	xml2::xml_find_first(node, paste0("w:", name))
}

# One paragraph per line; where a character rule matches, the text is
# written as a run with that character style. The text is the same
# character for character, so the alignment holds. An earlier rule wins.
.docx_add_line_rules <- function(doc, line, style, rules) {
	if (length(rules) == 0 || is.na(line) || !nzchar(line)) {
		return(officer::body_add_par(doc, value = line, style = style))
	}
	chars <- helper_text_graphemes_split(line)
	owner <- rep(NA_character_, length(chars))
	ends <- cumsum(nchar(chars))
	starts <- ends - nchar(chars) + 1
	for (rule in rev(rules)) {
		found <- tryCatch(stringr::str_locate_all(line, rule$pattern)[[1]], error = function(e) NULL)
		if (is.null(found) || nrow(found) == 0) next
		for (k in seq_len(nrow(found))) {
			if (found[k, "end"] < found[k, "start"]) next
			owner[starts >= found[k, "start"] & ends <= found[k, "end"]] <- rule$style_id
		}
	}
	if (all(is.na(owner))) {
		return(officer::body_add_par(doc, value = line, style = style))
	}
	key <- ifelse(is.na(owner), "", owner)
	group <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
	runs <- lapply(split(seq_along(chars), group), function(idx) {
		piece <- paste(chars[idx], collapse = "")
		if (is.na(owner[idx[1]])) officer::ftext(piece) else officer::run_wordtext(piece, style_id = owner[idx[1]])
	})
	officer::body_add_fpar(doc, do.call(officer::fpar, unname(runs)), style = style)
}
