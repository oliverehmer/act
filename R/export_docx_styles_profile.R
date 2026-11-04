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
# does not have are created from the profile, and the values that profiles
# building on the look-"file" profile state (file.override) replace those
# of the Word file.
# Returns the document, the names of the created styles and the character
# rules (pattern + style id) for the lines.
.docx_profile_styles <- function(doc, profile, symbol_chars = character(0)) {
	styles_file <- file.path(doc$package_dir, "word", "styles.xml")
	xml <- xml2::read_xml(styles_file)
	from_profile <- identical(profile$word$look, "profile")
	default_word <- .style_by_role(profile, "default")$word
	created <- character(0)
	rules <- list()
	# two styles of one Word name: the lower one sets the look, as in the viewer
	used <- vapply(profile$styles, function(s) !identical(s$role, "normal") && nzchar(s$word) && (nzchar(s$role) || isTRUE(s$active)), logical(1))
	keys_all <- vapply(profile$styles, function(s) paste(if (identical(s$type, "character")) "character" else "paragraph", s$word), character(1))
	look_last <- used & !duplicated(ifelse(used, keys_all, paste0("\001", seq_along(keys_all))), fromLast = TRUE)
	for (i in seq_along(profile$styles)) {
		s <- profile$styles[[i]]
		if (identical(s$role, "normal") || !nzchar(s$word)) next
		if (!nzchar(s$role) && !isTRUE(s$active)) next
		type <- if (identical(s$type, "character")) "character" else "paragraph"
		node <- .docx_style_node(xml, s$word, type)
		exists <- !inherits(node, "xml_missing")
		if (!exists) {
			based_on <- if (type == "character") NA_character_ else if (!nzchar(s$role) || identical(s$role, "space")) default_word else "Normal"
			# the subtitle stands apart from the transcript below it: 6 pt (120 twips)
			# after, as in the Word templates of the old system
			node <- .docx_style_create(xml, s$word, type, based_on, keep_next = !identical(s$role, "space"),
			                           space_after = if (identical(s$role, "header.subtitle")) 120 else 0)
			created <- c(created, s$word)
		}
		override <- profile$file.override[[s$name]]
		if (look_last[i]) {
			if (from_profile || !exists || s$word %in% created) .docx_style_appearance(node, s, type)
			else if (length(override) > 0) .docx_style_appearance(node, s, type, keys = override)
		}
		if (type == "character") {
			pattern <- if (identical(s$applies, "symbols")) .docx_symbol_class(symbol_chars) else s$pattern
			if (!is.na(pattern) && nzchar(pattern)) {
				rules[[length(rules) + 1]] <- list(pattern = pattern, style_id = xml2::xml_attr(node, "styleId"),
				                                   symbols = identical(s$applies, "symbols"),
				                                   bold = s$bold, italic = s$italic)
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

.docx_style_create <- function(xml, name, type, based_on, keep_next = TRUE, space_after = 0, based_type = "paragraph") {
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
		base_node <- .docx_style_node(xml, based_on, based_type)
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
		       "<w:spacing w:before=\"0\" w:after=\"", space_after, "\" w:line=\"240\" w:lineRule=\"auto\"/><w:jc w:val=\"left\"/></w:pPr>",
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
# out, so Word takes it from the style this one is based on. With keys only
# these values are touched; the others stay as the Word file has them.
.docx_style_appearance <- function(node, s, type, keys = .STYLE_APPEARANCE) {
	s <- s[!names(s) %in% setdiff(.STYLE_APPEARANCE, keys)]
	tags <- c(font = "w:rFonts", bold = "w:b|w:bCs", italic = "w:i|w:iCs", color = "w:color", size = "w:sz|w:szCs", background = "w:shd")
	rpr <- .docx_child_ensure(node, "rPr")
	xml2::xml_remove(xml2::xml_find_all(rpr, paste(tags[keys], collapse = "|")))
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
		if ("background" %in% keys) xml2::xml_remove(xml2::xml_find_all(ppr, "w:shd"))
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
# character for character, so the alignment holds. A later rule wins.
.docx_add_line_rules <- function(paragraphs, line, style, rules, owners = NULL) {
	if ((length(rules) == 0 && is.null(owners)) || is.na(line) || !nzchar(line)) {
		return(.docx_par_add(paragraphs, line, style))
	}
	chars <- helper_text_graphemes_split(line)
	owner <- rep(NA_character_, length(chars))
	ends <- cumsum(nchar(chars))
	starts <- ends - nchar(chars) + 1
	for (rule in rules) {
		found <- tryCatch(stringr::str_locate_all(line, rule$pattern)[[1]], error = function(e) NULL)
		if (is.null(found) || nrow(found) == 0) next
		for (k in seq_len(nrow(found))) {
			if (found[k, "end"] < found[k, "start"]) next
			owner[starts >= found[k, "start"] & ends <= found[k, "end"]] <- rule$style_id
		}
	}
	# the layer line a symbol belongs to (positions count graphemes)
	layer <- rep(NA_character_, length(chars))
	if (!is.null(owners)) {
		ok <- owners$pos >= 1 & owners$pos <= length(chars)
		layer[owners$pos[ok]] <- owners$tier[ok]
	}
	if (all(is.na(owner)) && all(is.na(layer))) {
		return(.docx_par_add(paragraphs, line, style))
	}
	key <- paste(ifelse(is.na(owner), "", owner), ifelse(is.na(layer), "", layer))
	group <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
	style_of_layer <- attr(owners, "styles")
	runs <- lapply(split(seq_along(chars), group), function(idx) {
		piece <- paste(chars[idx], collapse = "")
		i <- idx[1]
		if (is.na(owner[i]) && is.na(layer[i])) return(officer::ftext(piece))
		.docx_run_styled(paragraphs, piece, if (is.na(layer[i])) owner[i] else style_of_layer[[layer[i]]])
	})
	.docx_fpar_add(paragraphs, do.call(officer::fpar, unname(runs)), style)
}

# ===== SYMBOLS IN THE LOOK OF THEIR LAYER LINE =====
# A profile whose character style for the multimodal symbols says "from the
# layer line" gives each symbol, in the verbal line and in its layer line, the
# look of that layer. The look lives in a character style of its own, based on
# the style for the symbols, so that changing it in Word changes every symbol of
# the layer; layers of the same look share one. Returns the document and, per
# layer tier with symbols, the id of its character style (NULL = not asked for),
# and per style what bold and italic must be on the symbols themselves.
.docx_layer_symbol_styles <- function(doc, profile, result, plan, anchors, rules) {
	base <- Filter(function(r) isTRUE(r$symbols), rules)
	stated <- function(bold, italic) Filter(Negate(is.null), list(bold = bold, italic = italic))
	direct <- list()
	if (length(base) > 0) direct[[base[[length(base)]]$style_id]] <- stated(base[[length(base)]]$bold, base[[length(base)]]$italic)
	none <- list(doc = doc, styles = NULL, direct = direct)
	if (is.null(result) || length(base) == 0) return(none)
	symbol_style <- Filter(function(s) identical(s$type, "character") && isTRUE(s$active) && identical(s$applies, "symbols") && isTRUE(s$from.layer), profile$styles)
	if (length(symbol_style) == 0) return(none)
	in_lines <- stringr::str_detect(plan$line, base[[length(base)]]$pattern) %in% TRUE & !result$is_main[plan$row]
	placed <- if (is.null(anchors) || nrow(anchors) == 0) character(0) else as.character(anchors$tierName[anchors$status == "placed"])
	tiers <- intersect(unique(as.character(result$tierName[!result$is_main])), c(as.character(result$tierName[plan$row[in_lines]]), placed))
	if (length(tiers) == 0) return(none)

	styles_file <- file.path(doc$package_dir, "word", "styles.xml")
	xml <- xml2::read_xml(styles_file)
	base_word <- symbol_style[[length(symbol_style)]]$word
	from_profile <- identical(profile$word$look, "profile")
	style_ids <- character(0)
	made <- list()
	for (tier in tiers) {
		layer <- .style_tier(profile, tier)
		a <- .style_appearance(profile, layer)
		look <- list(color = a$color, bold = a$bold, italic = a$italic)
		look <- look[!vapply(look, is.null, logical(1))]
		if (length(look) == 0) {
			style_ids[tier] <- base[[length(base)]]$style_id
			next
		}
		key <- paste(names(look), unlist(look), collapse = "|")
		if (is.null(made[[key]])) {
			name <- paste0(base_word, "_", stringr::str_remove(if (nzchar(layer$word)) layer$word else layer$name, "^transcript_"))
			node <- .docx_style_node(xml, name, "character")
			exists <- !inherits(node, "xml_missing")
			if (!exists) node <- .docx_style_create(xml, name, "character", base_word, based_type = "character")
			if (from_profile || !exists) .docx_style_appearance(node, look, "character", keys = names(look))
			made[[key]] <- xml2::xml_attr(node, "styleId")
			own <- base[[length(base)]]
			direct[[made[[key]]]] <- stated(if (!is.null(look$bold)) look$bold else own$bold,
			                                if (!is.null(look$italic)) look$italic else own$italic)
		}
		style_ids[tier] <- made[[key]]
	}
	xml2::write_xml(xml, file = styles_file)
	read_styles <- utils::getFromNamespace("read_docx_styles", "officer")
	doc$styles <- read_styles(doc$package_dir)
	list(doc = doc, styles = style_ids, direct = direct)
}

# Bold and italic of a symbol stand on the run itself: in a Word style they are
# toggles, "not bold" in a character style does not take the bold of a bold
# paragraph away (and "bold" would take it off). Only direct format is absolute,
# so a symbol stays as the profile says whatever its line is. The colour has no
# such rule and stays in the style.
.docx_run_styled <- function(paragraphs, text, style_id) {
	look <- paragraphs$direct[[style_id]]
	if (length(look) == 0) return(officer::run_wordtext(text, style_id = style_id))
	structure(list(text = text, style_id = style_id, look = look), class = c("act_run_symbol", "run"))
}

#' @exportS3Method officer::to_wml
to_wml.act_run_symbol <- function(x, add_ns = FALSE, ...) {
	props <- sprintf("<w:rStyle w:val=\"%s\"/>", x$style_id)
	if (!is.null(x$look$bold))   props <- c(props, sprintf("<w:b w:val=\"%s\"/>", if (isTRUE(x$look$bold)) "1" else "0"))
	if (!is.null(x$look$italic)) props <- c(props, sprintf("<w:i w:val=\"%s\"/>", if (isTRUE(x$look$italic)) "1" else "0"))
	paste0("<w:r><w:rPr>", paste(props, collapse = ""), "</w:rPr><w:t xml:space=\"preserve\">", .docx_xml_escape(x$text), "</w:t></w:r>")
}

# The rules of one line: in a layer line the symbols take the style of that layer.
# All layers in one style: also a symbol of a verbal line no anchor names takes it.
.docx_rules_of_row <- function(rules, layer_styles, tier) {
	if (is.null(layer_styles)) return(rules)
	style <- if (!is.na(layer_styles[tier])) layer_styles[[tier]] else if (length(unique(layer_styles)) == 1L) layer_styles[[1]] else NA_character_
	if (is.na(style)) return(rules)
	lapply(rules, function(r) {
		if (isTRUE(r$symbols)) r$style_id <- style
		r
	})
}

# For printed line k of row row_p: the symbols a layer line anchors there,
# as character positions (1-based in the line) with the style of that layer.
# The column of an anchor can lie a few places before its symbol (a line that was
# indented after the anchors were placed: "04        |S::EIS- (0.5)|#" named the
# blank and "5)"), so each takes the nearest free place that holds its symbol.
.docx_symbol_owners <- function(layer_styles, anchors, row_p, k, line) {
	if (is.null(layer_styles) || is.null(anchors) || nrow(anchors) == 0 || is.na(line)) return(NULL)
	hit <- which(anchors$source_row == row_p & anchors$target_line == k & anchors$status == "placed" &
	             anchors$tierName %in% names(layer_styles))
	if (length(hit) == 0) return(NULL)
	chars <- helper_text_graphemes_split(line)
	wanted <- anchors$prefix_width[hit] + anchors$target_col[hit]
	places <- rep(NA_integer_, length(hit))
	free <- rep(TRUE, length(chars))
	distance <- function(i) { candidates <- which(chars == anchors$char[hit[i]] & free); if (length(candidates)) min(abs(candidates - wanted[i])) else Inf }
	for (round in seq_along(hit)) {
		open <- which(is.na(places))
		gaps <- vapply(open, distance, numeric(1))
		if (!length(open) || !any(is.finite(gaps))) break
		i <- open[which.min(gaps)]
		candidates <- which(chars == anchors$char[hit[i]] & free)
		places[i] <- candidates[which.min(abs(candidates - wanted[i]))]
		free[places[i]] <- FALSE
	}
	keep <- !is.na(places)
	if (!any(keep)) return(NULL)
	out <- data.frame(pos = places[keep], tier = as.character(anchors$tierName[hit[keep]]), stringsAsFactors = FALSE)
	attr(out, "styles") <- layer_styles
	out
}
