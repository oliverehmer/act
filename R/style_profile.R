# Style profiles: one JSON file holds the whole output format of a print
# transcript (line structure, acronyms, tier styles, character styles, Word
# appearance). A profile is read, checked and turned into a layout object
# that carries the profile as attribute "style"; the alignment engine and
# the exports read the extra values from there.

.STYLE_ROLES <- c("normal", "default", "header.preface", "header.title",
                  "header.subtitle", "header.info", "space")
.STYLE_ROLES_OBLIGATORY <- c("default", "header.preface", "header.title",
                             "header.subtitle", "header.info")
.STYLE_INDENTS <- c("none", "content", "text", "align")
.STYLE_CASES <- c("", "lower", "upper", "capitalize")
.STYLE_APPEARANCE <- c("font", "size", "color", "background", "italic", "bold")

# What the indent "text" skips at the start of the verbal line: latching,
# overlap bracket, comment opener, breathing, pauses. The multimodal symbols
# are added from the anchor characters at render time.
.STYLE_INDENT_TEXT_SKIP <- c("=", "\\[", "<<[^>]*>\\s*", "\u00b0h+\\s*", "h+\u00b0\\s*",
                             "\\([0-9.\\-]*\\)\\s*")

# ===== EXPORTED =====

#' Helper: Read a style profile
#'
#' A style profile is a JSON file that holds the whole format of a print
#' transcript: line structure, acronyms, tier styles, character styles and
#' the appearance in Word. A profile can build on another profile
#' (\code{extends}) and then only states what differs.
#'
#' @param style Character string or list; the name of a profile (a file \code{<name>.json} in one of the profile folders), the path of a profile file, or a profile given as list.
#'
#' @details The profile folders are the folders named in the option \code{act.style.folders} and the folder \code{extdata/styles} of the package. The profile is checked; all problems found are reported together.
#'
#' @return The profile as list of class \code{act_style}, with all values filled in.
#'
#' @export
helper_style_read <- function(style) {
	.style_read(style)
}

#' Helper: Style profiles that can be found
#'
#' @return Named vector of character strings; paths of the profile files, named by the profile names.
#'
#' @export
helper_style_list <- function() {
	.style_list()
}

#' Helper: Layout object of a style profile
#'
#' Turns a style profile into the layout object that the export functions
#' take as \code{l}. The profile rides along, so everything a layout object
#' cannot express (e.g. space lines, acronym rules) still takes effect.
#'
#' @param style Style profile; see \code{helper_style_read}.
#' @param templatePath Character string; path of the Word file the document is created from. \code{NULL} takes the file named in the profile; without any file the template of the package is used.
#'
#' @return Layout object.
#'
#' @export
helper_style_layout <- function(style, templatePath = NULL) {
	.style_layout(style, templatePath = templatePath)
}

#' Helper: Acronyms of tiers by a style profile
#'
#' Builds the acronym of each tier name the way the print transcript does:
#' case, search and replace, extract, width, ending - with the values of the
#' tier style, and where it leaves one open, those of the profile.
#'
#' @param style Style profile; see \code{helper_style_read}.
#' @param tierNames Vector of character strings; tier names.
#' @param ending Logical; if \code{TRUE} the ending of the acronym is added.
#'
#' @return Vector of character strings; one acronym per tier name.
#'
#' @export
helper_style_acronym <- function(style, tierNames, ending = TRUE) {
	profile <- .style_read(style)
	vapply(as.character(tierNames), function(tier_name) {
		.style_acronym(profile, tier_name, ending = ending)
	}, character(1), USE.NAMES = FALSE)
}

#' Helper: Word file of a style profile
#'
#' Writes an empty Word file that carries the styles of a style profile:
#' the file the profile names (or \code{templatePath}, or the template of the
#' package) with the paragraph and character styles of the profile written
#' into it. Use it as a template of your own, or to merge transcripts into.
#'
#' @param style Style profile; see \code{helper_style_read}.
#' @param pathOutput Character string; path of the .docx file to write.
#' @param templatePath Character string; path of the Word file to start from. \code{NULL} takes the file named in the profile.
#' @param symbolChars Vector of character strings; the multimodal symbols, needed only to return the character rules.
#'
#' @return Invisibly a list with the path and the names of the styles that were created (not in the Word file before).
#'
#' @export
helper_style_docx <- function(style, pathOutput, templatePath = NULL, symbolChars = character(0)) {
	profile <- .style_read(style)
	l <- .style_layout(profile, templatePath = templatePath)
	template <- helper_layout_docx_templates_resolve(l)[1]
	doc <- officer::read_docx(path = template)
	written <- .docx_profile_styles(doc, profile, symbolChars)
	print(written$doc, target = pathOutput)
	invisible(list(path = pathOutput, created = written$created))
}

#' Helper: Styles of a style profile with their appearance
#'
#' Lists the styles of a profile with the appearance that finally holds for
#' each: its own values, and for a value it does not set the one of the style
#' it builds on (transcript lines on the default style, the default and the
#' header styles on Normal). Character styles only have their own values.
#'
#' @param style Style profile; see \code{helper_style_read}.
#'
#' @return Data.frame with one row per style: \code{name}, \code{type}, \code{role}, \code{active}, \code{word}, \code{pattern}, \code{applies}, and \code{font}, \code{size}, \code{color}, \code{background}, \code{italic}, \code{bold} (\code{NA} where nothing is set).
#'
#' @export
helper_style_appearance <- function(style) {
	profile <- .style_read(style)
	rows <- lapply(profile$styles, function(s) {
		a <- .style_appearance(profile, s)
		data.frame(
			name = s$name, type = s$type, role = s$role, active = isTRUE(s$active), word = s$word,
			pattern = .style_chr(s$pattern), applies = .style_chr(s$applies), from.layer = isTRUE(s$from.layer),
			font = .style_chr(a$font, NA_character_),
			size = .style_num(a$size, NA_real_),
			color = .style_chr(a$color, NA_character_),
			background = .style_chr(a$background, NA_character_),
			italic = if (is.null(a$italic)) NA else isTRUE(a$italic),
			bold = if (is.null(a$bold)) NA else isTRUE(a$bold),
			stringsAsFactors = FALSE)
	})
	do.call(rbind, rows)
}

# ===== READ =====

# style: a profile name (file <name>.json in one of the folders), a path to
# a JSON file, or an already parsed list. Returns the resolved profile.
.style_read <- function(style, folders = .style_folders()) {
	if (inherits(style, "act_style")) return(style)
	raw <- .style_read_raw(style, folders)
	chain <- character(0)
	parents <- list()
	current <- raw
	while (nzchar(.style_chr(current[["extends"]]))) {
		parent_name <- .style_chr(current[["extends"]])
		if (parent_name %in% chain) {
			cli::cli_abort("Style profiles build on each other in a circle: {.val {c(chain, parent_name)}}")
		}
		chain <- c(chain, parent_name)
		current <- .style_read_raw(parent_name, folders)
		parents[[length(parents) + 1]] <- current
	}
	profile <- .style_defaults()
	for (parent in rev(parents)) profile <- .style_merge(profile, parent)
	profile <- .style_merge(profile, raw)
	profile$extends <- if (length(chain) > 0) chain[1] else NULL
	profile$id <- attr(raw, "id")
	profile$path <- attr(raw, "path")
	own_label <- .style_chr(raw[["label"]])
	profile$label <- if (nzchar(own_label)) own_label else if (is.null(profile$id)) "style" else profile$id
	profile <- .style_normalize(profile)
	problems <- .style_problems(profile)
	if (length(problems) > 0) {
		# the problems quote patterns of the user: braces must not be read as cli markup
		problems <- stringr::str_replace_all(problems, "([{}])", "\\1\\1")
		cli::cli_abort(c("The style profile {.val {profile$label}} is not valid.",
		                 stats::setNames(problems, rep("x", length(problems)))))
	}
	class(profile) <- "act_style"
	profile
}

.style_folders <- function() {
	extra <- getOption("act.style.folders", NULL)
	extra <- as.character(extra[!is.na(extra) & nzchar(extra)])
	c(extra, system.file("extdata", "styles", package = "act"))
}

# Names of the profiles found in the folders (first folder wins on a tie).
.style_list <- function(folders = .style_folders()) {
	out <- character(0)
	for (folder in folders) {
		if (!dir.exists(folder)) next
		files <- list.files(folder, pattern = "[.]json$", full.names = TRUE)
		ids <- tools::file_path_sans_ext(basename(files))
		new <- !ids %in% names(out)
		out <- c(out, stats::setNames(files[new], ids[new]))
	}
	out
}

.style_read_raw <- function(style, folders) {
	if (is.list(style)) {
		raw <- style
		.style_check_shape(raw)
		attr(raw, "id") <- NULL
		return(raw)
	}
	if (!is.character(style) || length(style) != 1 || is.na(style) || !nzchar(style)) {
		cli::cli_abort("{.arg style} must be the name of a style profile, the path of a profile file or a list.")
	}
	path <- NULL
	if (file.exists(style) && !dir.exists(style)) {
		path <- style
	} else {
		known <- .style_list(folders)
		if (style %in% names(known)) path <- known[[style]]
	}
	if (is.null(path)) {
		known <- names(.style_list(folders))
		cli::cli_abort(c("Style profile {.val {style}} not found.",
		                 "i" = "Known profiles: {.val {known}}"))
	}
	raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
	                error = function(e) e)
	if (inherits(raw, "error")) {
		detail <- conditionMessage(raw)
		cli::cli_abort(c("The style profile file cannot be read: {.path {path}}",
		                 "x" = "{detail}"))
	}
	if (!is.list(raw) || is.null(names(raw))) {
		cli::cli_abort("The style profile file does not hold a profile: {.path {path}}")
	}
	# a profile may come wrapped: {"label": ..., "params": {the profile}}
	if (is.list(raw[["params"]])) {
		label <- .style_chr(raw[["label"]])
		raw <- raw[["params"]]
		if (!nzchar(.style_chr(raw[["label"]])) && nzchar(label)) raw[["label"]] <- label
	}
	.style_check_shape(raw, path)
	attr(raw, "id") <- tools::file_path_sans_ext(basename(path))
	attr(raw, "path") <- path
	raw
}

# styles must be a list of styles and the groups must be groups: everything
# after this relies on it.
.style_check_shape <- function(raw, where = "the profile") {
	styles <- raw[["styles"]]
	if (!is.null(styles) && (!is.list(styles) || !is.null(names(styles)) || !all(vapply(styles, is.list, logical(1))))) {
		cli::cli_abort("{.field styles} must be a list of styles in {where}.")
	}
	for (group in c("acronym", "word", "functions", "advanced")) {
		if (!is.null(raw[[group]]) && !is.list(raw[[group]])) {
			cli::cli_abort("{.field {group}} must be a group of settings in {where}.")
		}
	}
	for (style in styles) {
		if (!is.null(style[["acronym"]]) && !is.list(style[["acronym"]])) {
			cli::cli_abort("{.field acronym} of a style must be a group of settings in {where}.")
		}
	}
	invisible(TRUE)
}

# The values a profile has when it says nothing: the standard layout of act.
.style_defaults <- function() {
	list(
		label = "",
		mode = "gat",
		header = TRUE,
		width = 65,
		width.limit = TRUE,
		space.lines = TRUE,
		line.numbers = TRUE,
		time.format = "",
		acronym = list(show = TRUE, suppress.repeated = TRUE, case = "",
		               search = "", replace = "", extract = "", width = 3,
		               ending = ":  "),
		word = list(file = "", look = "file"),
		functions = list(tiers.exclude = "", tiers.keep = "", arrow = TRUE,
		                 arrow.shape = "->"),
		advanced = list(spaces.before = 3, brackets.align = TRUE,
		                symbol.merge = TRUE, tolerance.point = 0.2,
		                tolerance.gesture = 0.5, fill = "-", block.height = 2,
		                min.description = 10, max.span.blocks = 3,
		                fig.replace = TRUE, fig.tier.regex = "^stills(#|$)",
		                multimodal.tier.regex = "#mm[0-9]*$"),
		styles = list()
	)
}

# child over parent: single values replace, the named groups are merged key
# by key, styles are merged by their name; new styles of the child stand
# before the tier styles of the parent (the more special pattern must be
# asked first), unless the child gives the order itself.
.style_merge <- function(parent, child) {
	out <- parent
	groups <- c("acronym", "word", "functions", "advanced")
	for (key in setdiff(names(child), c("styles", "styles.order", "styles.remove", groups))) {
		out[[key]] <- child[[key]]
	}
	for (group in groups) {
		if (is.list(child[[group]])) {
			for (key in names(child[[group]])) out[[group]][[key]] <- child[[group]][[key]]
		}
	}
	parent_styles <- out$styles
	parent_names <- vapply(parent_styles, function(s) .style_chr(s$name), character(1))
	new_styles <- list()
	# [[ ]]: with $ a profile without styles would hand over its styles.remove
	for (style in child[["styles"]]) {
		name <- .style_chr(style$name)
		hit <- match(name, parent_names)
		if (is.na(hit) || !nzchar(name)) {
			new_styles[[length(new_styles) + 1]] <- style
		} else {
			merged <- parent_styles[[hit]]
			for (key in setdiff(names(style), "acronym")) merged[key] <- list(style[[key]])
			if (is.list(style$acronym)) {
				if (!is.list(merged$acronym)) merged$acronym <- list()
				for (key in names(style$acronym)) merged$acronym[key] <- list(style$acronym[[key]])
			}
			parent_styles[[hit]] <- merged
		}
	}
	remove <- unlist(child[["styles.remove"]])
	if (length(remove) > 0) {
		parent_styles <- parent_styles[!parent_names %in% remove]
	}
	styles <- c(new_styles, parent_styles)
	order_wanted <- unlist(child[["styles.order"]])
	if (length(order_wanted) > 0) {
		names_now <- vapply(styles, function(s) .style_chr(s$name), character(1))
		rank <- match(names_now, order_wanted)
		rank[is.na(rank)] <- length(order_wanted) + seq_len(sum(is.na(rank)))
		styles <- styles[order(rank)]
	}
	out$styles <- styles
	out
}

.style_chr <- function(x, default = "") {
	if (is.null(x) || length(x) != 1 || is.na(x)) return(default)
	as.character(x)
}

.style_lgl <- function(x, default) {
	if (is.null(x) || length(x) != 1 || is.na(x)) return(default)
	isTRUE(as.logical(x))
}

.style_num <- function(x, default) {
	if (is.null(x) || length(x) != 1) return(default)
	value <- suppressWarnings(as.numeric(x))
	if (is.na(value)) default else value
}

# Brings every value into its type, so the rest of the code need not test.
.style_normalize <- function(profile) {
	d <- .style_defaults()
	profile$mode <- .style_chr(profile[["mode"]], d$mode)
	profile$header <- .style_lgl(profile[["header"]], d$header)
	profile$width <- .style_num(profile[["width"]], d$width)
	profile$width.limit <- .style_lgl(profile[["width.limit"]], d$width.limit)
	profile$space.lines <- .style_lgl(profile[["space.lines"]], d$space.lines)
	profile$line.numbers <- .style_lgl(profile[["line.numbers"]], d$line.numbers)
	profile$time.format <- .style_chr(profile[["time.format"]])
	a <- profile[["acronym"]]
	profile$acronym <- list(
		show = .style_lgl(a[["show"]], d$acronym$show),
		suppress.repeated = .style_lgl(a[["suppress.repeated"]], d$acronym$suppress.repeated),
		case = .style_chr(a[["case"]]), search = .style_chr(a[["search"]]),
		replace = .style_chr(a[["replace"]]), extract = .style_chr(a[["extract"]]),
		width = .style_num(a[["width"]], 0), ending = .style_chr(a[["ending"]]))
	w <- profile[["word"]]
	profile$word <- list(file = .style_chr(w[["file"]]), look = .style_chr(w[["look"]], d$word$look))
	f <- profile[["functions"]]
	profile$functions <- list(
		tiers.exclude = .style_chr(f[["tiers.exclude"]]), tiers.keep = .style_chr(f[["tiers.keep"]]),
		arrow = .style_lgl(f[["arrow"]], d$functions$arrow),
		arrow.shape = .style_chr(f[["arrow.shape"]], d$functions$arrow.shape))
	v <- profile[["advanced"]]
	profile$advanced <- list(
		spaces.before = .style_num(v[["spaces.before"]], d$advanced$spaces.before),
		brackets.align = .style_lgl(v[["brackets.align"]], d$advanced$brackets.align),
		symbol.merge = .style_lgl(v[["symbol.merge"]], d$advanced$symbol.merge),
		tolerance.point = .style_num(v[["tolerance.point"]], d$advanced$tolerance.point),
		tolerance.gesture = .style_num(v[["tolerance.gesture"]], d$advanced$tolerance.gesture),
		fill = .style_chr(v[["fill"]], d$advanced$fill),
		block.height = .style_num(v[["block.height"]], d$advanced$block.height),
		min.description = .style_num(v[["min.description"]], d$advanced$min.description),
		max.span.blocks = .style_num(v[["max.span.blocks"]], d$advanced$max.span.blocks),
		fig.replace = .style_lgl(v[["fig.replace"]], d$advanced$fig.replace),
		fig.tier.regex = .style_chr(v[["fig.tier.regex"]], d$advanced$fig.tier.regex),
		multimodal.tier.regex = .style_chr(v[["multimodal.tier.regex"]], d$advanced$multimodal.tier.regex))
	profile$styles <- lapply(profile[["styles"]], .style_normalize_style)
	profile$styles.order <- NULL
	profile$styles.remove <- NULL
	profile
}

.style_normalize_style <- function(s) {
	role <- .style_chr(s[["role"]])
	type <- .style_chr(s[["type"]], "tier")
	out <- list(
		name = .style_chr(s[["name"]]),
		type = if (nzchar(role)) "tier" else type,
		role = role,
		active = .style_lgl(s[["active"]], TRUE),
		word = .style_chr(s[["word"]]))
	for (key in .STYLE_APPEARANCE) {
		value <- s[[key]]
		if (is.null(value) || length(value) != 1 || is.na(value)) value <- NULL
		else if (key == "size") value <- suppressWarnings(as.numeric(value))
		else if (key %in% c("italic", "bold")) value <- as.logical(value)
		else value <- as.character(value)
		out[key] <- list(if (length(value) != 1 || is.na(value)) NULL else value)
	}
	# line and character styles take font and size from Transcript default:
	# a layer line is aligned to its main line character by character, which
	# holds only with one font and one size
	if (!nzchar(role)) out[c("font", "size")] <- list(NULL)
	if (identical(out$type, "character")) {
		out$applies <- .style_chr(s[["applies"]], "symbols")
		out$pattern <- .style_chr(s[["pattern"]])
		out$from.layer <- .style_lgl(s[["from.layer"]], FALSE)
		return(out)
	}
	a <- s[["acronym"]]
	out$pattern <- .style_chr(s[["pattern"]])
	out$example <- .style_chr(s[["example"]])
	out$main <- if (is.null(s[["main"]]) || length(s[["main"]]) != 1 || is.na(s[["main"]])) NA else isTRUE(as.logical(s[["main"]]))
	out$line.numbers.suppress <- .style_lgl(s[["line.numbers.suppress"]], FALSE)
	out$acronym <- list(
		suppress = .style_lgl(a[["suppress"]], FALSE),
		case = .style_chr(a[["case"]]), search = .style_chr(a[["search"]]),
		replace = if (length(a[["replace"]]) == 0) NULL else .style_chr(a[["replace"]]),
		extract = .style_chr(a[["extract"]]),
		width = .style_num(a[["width"]], 0),
		ending = if (length(a[["ending"]]) == 0) NULL else .style_chr(a[["ending"]]))
	out$indent <- .style_chr(s[["indent"]], "none")
	out$align.chars <- .style_chr(s[["align.chars"]])
	out$align.mode <- .style_chr(s[["align.mode"]])
	out
}

# ===== CHECK =====

# extract patterns run in stringr (ICU), the others in base R (PCRE): a
# pattern must be valid where it is used.
.style_regex_ok <- function(pattern, icu = FALSE) {
	if (!nzchar(pattern)) return(TRUE)
	isTRUE(tryCatch({
		if (icu) stringr::str_detect("", pattern) else grepl(pattern, "", perl = TRUE)
		TRUE
	}, error = function(e) FALSE, warning = function(w) FALSE))
}

# Returns the problems of a normalized profile as sentences; empty = valid.
.style_problems <- function(profile) {
	p <- character(0)
	if (!profile$mode %in% c("gat", "mondada")) {
		p <- c(p, paste0("mode is '", profile$mode, "'; allowed: gat, mondada."))
	}
	if (isTRUE(profile$width.limit) && (profile$width < 40 || profile$width > 500)) {
		p <- c(p, paste0("width is ", profile$width, "; allowed: 40 to 500."))
	}
	v <- profile$advanced
	if (v$spaces.before < 0 || v$spaces.before > 20) p <- c(p, paste0("spaces before is ", v$spaces.before, "; allowed: 0 to 20."))
	if (v$tolerance.point < 0 || v$tolerance.gesture < 0) p <- c(p, "a time tolerance is negative.")
	if (v$block.height < 0) p <- c(p, paste0("block height is ", v$block.height, "; the minimum is 0."))
	if (v$min.description < 0) p <- c(p, paste0("minimum room is ", v$min.description, "; the minimum is 0."))
	if (v$max.span.blocks < 1) p <- c(p, paste0("span is ", v$max.span.blocks, "; the minimum is 1."))
	if (nchar(v$fill) != 1) p <- c(p, "the fill character must be exactly one character.")
	aw <- profile$acronym$width
	if (aw < 0 || aw > 25) {
		p <- c(p, paste0("acronym width is ", aw, "; allowed: 1 to 25, or 0 for the full name."))
	}
	if (!profile$acronym$case %in% .STYLE_CASES) {
		p <- c(p, paste0("acronym case is '", profile$acronym$case, "'; allowed: lower, upper, capitalize or empty."))
	}
	if (nzchar(profile$time.format) && !profile$time.format %in% helper_time_formats_list()) {
		p <- c(p, paste0("time format is '", profile$time.format, "'; allowed: ", paste(helper_time_formats_list(), collapse = ", "), " or empty."))
	}
	if (!profile$word$look %in% c("file", "profile")) {
		p <- c(p, paste0("word look is '", profile$word$look, "'; allowed: file, profile."))
	}
	for (key in c("search", "extract")) {
		if (!.style_regex_ok(profile$acronym[[key]], icu = key == "extract")) {
			p <- c(p, paste0("acronym ", key, " is not a valid regular expression: ", profile$acronym[[key]]))
		}
	}
	for (key in c("tiers.exclude", "tiers.keep")) {
		if (!.style_regex_ok(profile$functions[[key]])) {
			p <- c(p, paste0(key, " is not a valid regular expression: ", profile$functions[[key]]))
		}
	}
	names_all <- vapply(profile$styles, function(s) s$name, character(1))
	if (any(!nzchar(names_all))) p <- c(p, "a style has no name.")
	doubled <- unique(names_all[duplicated(names_all) & nzchar(names_all)])
	if (length(doubled) > 0) {
		p <- c(p, paste0("style names occur twice: ", paste(doubled, collapse = ", "), "."))
	}
	roles <- vapply(profile$styles, function(s) s$role, character(1))
	unknown <- setdiff(roles[nzchar(roles)], .STYLE_ROLES)
	if (length(unknown) > 0) {
		p <- c(p, paste0("unknown fixed style: ", paste(unknown, collapse = ", "), "."))
	}
	doubled_roles <- unique(roles[duplicated(roles) & nzchar(roles)])
	if (length(doubled_roles) > 0) {
		p <- c(p, paste0("fixed styles occur twice: ", paste(doubled_roles, collapse = ", "), "."))
	}
	missing_roles <- setdiff(.STYLE_ROLES_OBLIGATORY, roles)
	if (length(missing_roles) > 0) {
		p <- c(p, paste0("fixed styles missing: ", paste(missing_roles, collapse = ", "), "."))
	}
	for (s in profile$styles) {
		label <- paste0("style '", s$name, "': ")
		if (!s$type %in% c("tier", "character")) {
			p <- c(p, paste0(label, "type is '", s$type, "'; allowed: tier, character."))
			next
		}
		if (!nzchar(s$word) && (nzchar(s$role) || identical(s$type, "character"))) {
			p <- c(p, paste0(label, "the Word style name is empty."))
		}
		if (!is.null(s$size) && (!is.numeric(s$size) || s$size < 4 || s$size > 72)) {
			p <- c(p, paste0(label, "size must be a number between 4 and 72."))
		}
		for (key in c("color", "background")) {
			value <- s[[key]]
			if (!is.null(value) && !grepl("^#[0-9A-Fa-f]{6}$", as.character(value))) {
				p <- c(p, paste0(label, key, " must look like #1a2b3c."))
			}
		}
		if (identical(s$type, "character")) {
			if (!s$applies %in% c("symbols", "pattern")) {
				p <- c(p, paste0(label, "applies is '", s$applies, "'; allowed: symbols, pattern."))
			}
			if (identical(s$applies, "pattern") && (!nzchar(s$pattern) || !.style_regex_ok(s$pattern, icu = TRUE))) {
				p <- c(p, paste0(label, "the pattern is empty or not a valid regular expression."))
			}
			next
		}
		if (nzchar(s$role)) next
		if (!nzchar(s$pattern)) {
			p <- c(p, paste0(label, "the tier pattern is empty."))
		} else if (!.style_regex_ok(s$pattern)) {
			p <- c(p, paste0(label, "the tier pattern is not a valid regular expression: ", s$pattern))
		}
		if (!s$indent %in% .STYLE_INDENTS) {
			p <- c(p, paste0(label, "indent is '", s$indent, "'; allowed: ", paste(.STYLE_INDENTS, collapse = ", "), "."))
		}
		if (!s$acronym$case %in% .STYLE_CASES) {
			p <- c(p, paste0(label, "acronym case is '", s$acronym$case, "'; allowed: lower, upper, capitalize or empty."))
		}
		if (s$acronym$width < 0 || s$acronym$width > 25) {
			p <- c(p, paste0(label, "acronym width is ", s$acronym$width, "; allowed: 1 to 25, or 0 for the value of the profile."))
		}
		for (key in c("search", "extract")) {
			if (!.style_regex_ok(s$acronym[[key]], icu = key == "extract")) {
				p <- c(p, paste0(label, "acronym ", key, " is not a valid regular expression: ", s$acronym[[key]]))
			}
		}
		if (nzchar(s$align.mode) && !s$align.mode %in% c("bracket", "point")) {
			p <- c(p, paste0(label, "align mode is '", s$align.mode, "'; allowed: bracket, point."))
		}
	}
	p
}

# ===== LOOK UP =====

.style_by_role <- function(profile, role) {
	for (s in profile$styles) if (identical(s$role, role)) return(s)
	NULL
}

# The tier style of a tier: the first active tier style whose pattern
# matches; without a match the fixed style "default".
.style_tier <- function(profile, tierName) {
	for (s in profile$styles) {
		if (!identical(s$type, "tier") || nzchar(s$role) || !isTRUE(s$active)) next
		if (grepl(s$pattern, tierName, perl = TRUE)) return(s)
	}
	.style_by_role(profile, "default")
}

# The appearance that finally holds for a style: its own values, else those
# of the style it builds on (header styles and the default on Normal, the
# other lines on the default).
.style_appearance <- function(profile, style) {
	chain <- list(style)
	if (identical(style$type, "tier")) {
		if (!identical(style$role, "normal")) {
			if (!nzchar(style$role) || identical(style$role, "space")) {
				chain[[length(chain) + 1]] <- .style_by_role(profile, "default")
			}
			chain[[length(chain) + 1]] <- .style_by_role(profile, "normal")
		}
	}
	out <- stats::setNames(vector("list", length(.STYLE_APPEARANCE)), .STYLE_APPEARANCE)
	for (key in .STYLE_APPEARANCE) {
		for (s in chain) {
			if (!is.null(s) && !is.null(s[[key]])) {
				out[key] <- list(s[[key]])
				break
			}
		}
	}
	out
}

# ===== ACRONYMS =====

# The one place an acronym is built. The steps run in this order: case,
# search and replace, extract, width, ending.
.style_acronym_text <- function(text, case = "", search = "", replace = "",
                                extract = "", width = 0, ending = "") {
	if (identical(case, "lower")) {
		text <- tolower(text)
	} else if (identical(case, "upper")) {
		text <- toupper(text)
	} else if (identical(case, "capitalize")) {
		text <- paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
	}
	if (nzchar(search)) text <- sub(search, replace, text, perl = TRUE)
	if (nzchar(extract)) {
		found <- stringr::str_extract(text, extract)
		if (!is.na(found)) text <- found
	}
	if (width > 0) text <- substr(text, 1, width)
	paste0(text, ending)
}

# The acronym settings that hold for a tier: the style of the tier first,
# what it leaves open comes from the profile.
.style_acronym_settings <- function(profile, style = NULL) {
	a <- profile$acronym
	s <- if (is.null(style)) list() else style$acronym
	pick <- function(key) if (!is.null(s[[key]]) && nzchar(s[[key]])) s[[key]] else a[[key]]
	own_search <- !is.null(s$search) && nzchar(s$search)
	list(
		case = pick("case"),
		search = pick("search"),
		replace = if (own_search) .style_chr(s$replace) else a$replace,
		extract = pick("extract"),
		width = if (!is.null(s$width) && s$width > 0) s$width else a$width,
		ending = if (!is.null(s$ending)) s$ending else a$ending)
}

.style_acronym <- function(profile, tierName, ending = TRUE) {
	settings <- .style_acronym_settings(profile, .style_tier(profile, tierName))
	.style_acronym_text(tierName, case = settings$case, search = settings$search,
	                    replace = settings$replace, extract = settings$extract,
	                    width = settings$width,
	                    ending = if (isTRUE(ending)) settings$ending else "")
}

# ===== PROFILE -> LAYOUT OBJECT =====

.style_case_to_table <- function(case) {
	switch(case, lower = "tolower", upper = "toupper", capitalize = "capitalize", NA_character_)
}

.style_na <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else x

# Builds the layout object the engine and the exports work with. The
# profile rides along as attribute "style".
.style_layout <- function(profile, templatePath = NULL) {
	profile <- .style_read(profile)
	l <- methods::new("layout")
	l@name <- profile$label
	l@filter.tier.includeRegEx <- profile$functions$tiers.keep
	l@filter.tier.excludeRegEx <- profile$functions$tiers.exclude
	l@transcript.width <- if (isTRUE(profile$width.limit)) profile$width else -1
	l@speaker.regex <- .style_na(profile$acronym$extract)
	l@speaker.width <- if (profile$acronym$width > 0) profile$acronym$width else -1
	l@speaker.ending <- profile$acronym$ending
	l@speaker.repeat <- !isTRUE(profile$acronym$suppress.repeated)
	l@line.nr.show <- isTRUE(profile$line.numbers)
	l@spacesbefore <- profile$advanced$spaces.before
	l@layout.mode <- profile$mode
	l@symbol.merge <- isTRUE(profile$advanced$symbol.merge)
	l@brackets.align <- isTRUE(profile$advanced$brackets.align)
	l@header.insert <- isTRUE(profile$header)
	l@arrow.insert <- isTRUE(profile$functions$arrow)
	l@arrow.shape <- profile$functions$arrow.shape
	l@docx.template.path <- if (is.null(templatePath)) profile$word$file else templatePath

	base_roles <- c("header.preface", "header.title", "header.subtitle", "header.info", "default")
	base <- data.frame(
		act.style.name = c("header.preface", "header.title", "header.subtitle", "header.info", "transcript.default"),
		docx.template.name = vapply(base_roles, function(r) .style_by_role(profile, r)$word, character(1)),
		stringsAsFactors = FALSE, row.names = NULL)
	for (s in profile$styles) {
		if (identical(s$type, "character") && isTRUE(s$active) && identical(s$applies, "symbols")) {
			base <- rbind(base, data.frame(act.style.name = "transcript.symbols",
			                               docx.template.name = s$word, stringsAsFactors = FALSE))
			break
		}
	}
	l@docx.styles.base <- base

	rows <- list()
	for (s in profile$styles) {
		if (!identical(s$type, "tier") || !isTRUE(s$active)) next
		if (nzchar(s$role) && !identical(s$role, "space")) next
		is_space <- identical(s$role, "space")
		align <- identical(s$indent, "align")
		rows[[length(rows) + 1]] <- data.frame(
			name = s$name,
			is.main.tier = if (is_space) FALSE else s$main,
			show = TRUE,
			match.regex = if (is_space) "^space$" else s$pattern,
			docx.template.name = .style_na(s$word),
			line.nr.show = if (is_space || isTRUE(s$line.numbers.suppress)) FALSE else NA,
			acronym.show = !(is_space || isTRUE(s$acronym$suppress)),
			acronym.case = if (is_space) NA_character_ else .style_case_to_table(s$acronym$case),
			acronym.search = if (is_space) NA_character_ else .style_na(s$acronym$search),
			acronym.replace = if (is_space || is.null(s$acronym$replace)) NA_character_ else s$acronym$replace,
			acronym.extract = if (is_space) NA_character_ else .style_na(s$acronym$extract),
			acronym.width = if (is_space) 0 else s$acronym$width,
			acronym.ending = if (is_space || is.null(s$acronym$ending)) NA_character_ else s$acronym$ending,
			content.indent = if (is_space) "content" else s$indent,
			content.indent.text.skip = NA_character_,
			content.indent.align.char = if (is_space) NA_character_ else .style_na(s$align.chars),
			content.indent.align.filler.inside = if (align && !identical(s$align.mode, "point")) substr(profile$advanced$fill, 1, 1) else " ",
			content.indent.align.mode = if (is_space) NA_character_ else .style_na(s$align.mode),
			content.indent.align.arrow = NA_character_,
			content.wrap = !is_space,
			space.after = NA_character_,
			comment = NA_character_,
			stringsAsFactors = FALSE)
	}
	# the space row first: the exports ask for the row that matches "space",
	# and no tier pattern standing before it may answer in its place
	if (length(rows) > 0) {
		is_space_row <- vapply(rows, function(r) identical(r$match.regex, "^space$"), logical(1))
		rows <- c(rows[is_space_row], rows[!is_space_row])
	}
	user <- if (length(rows) > 0) do.call(rbind, rows) else export_styles_user_load()[0, ]
	l@docx.styles.user <- user
	attr(l, "style") <- profile
	l
}

# The profile a layout object was built from, or NULL for a plain layout.
.layout_style <- function(l) {
	style <- attr(l, "style")
	if (inherits(style, "act_style")) style else NULL
}

# Time format of the transcript header: the profile's, else the act option.
.layout_time_format <- function(l) {
	profile <- .layout_style(l)
	if (!is.null(profile) && nzchar(profile$time.format)) return(profile$time.format)
	getOption("act.time.format.transcript", "h:mm:ss.s")
}

# ===== LAYOUT OBJECT -> PROFILE =====

# Describes a plain layout object as a profile (a list as it would stand in
# a profile file). What a profile cannot say is left out: rows that are not
# shown, rows that do not wrap, own skip patterns of the indent "text".
.style_from_layout <- function(l, label = l@name) {
	na_chr <- function(x) if (length(x) != 1 || is.na(x)) "" else as.character(x)
	width_limited <- length(l@transcript.width) == 1 && !is.na(l@transcript.width) && l@transcript.width != -1
	acronym_width <- if (length(l@speaker.width) != 1 || is.na(l@speaker.width) || l@speaker.width < 0) 0 else l@speaker.width
	base <- l@docx.styles.base
	base_name <- function(act_name, fallback) {
		hit <- which(base$act.style.name == act_name)
		if (length(hit) == 0 || is.na(base$docx.template.name[hit[1]])) fallback else base$docx.template.name[hit[1]]
	}
	styles <- list(
		list(name = "Normal", role = "normal", word = "Normal"),
		list(name = "Transcript default", role = "default", word = base_name("transcript.default", "transcript_default")),
		list(name = "Header preface", role = "header.preface", word = base_name("header.preface", "transcript_header_preface")),
		list(name = "Header title", role = "header.title", word = base_name("header.title", "transcript_header_title")),
		list(name = "Header subtitle", role = "header.subtitle", word = base_name("header.subtitle", "transcript_header_subtitle")),
		list(name = "Header info", role = "header.info", word = base_name("header.info", "transcript_header_info")))
	user <- l@docx.styles.user
	fill <- "-"
	names_used <- vapply(styles, function(s) s$name, character(1))
	for (i in seq_len(nrow(user))) {
		row <- user[i, ]
		if (is.na(row$match.regex)) next
		if (identical(row$match.regex, "^space$")) {
			styles[[length(styles) + 1]] <- list(name = "Space", role = "space", word = na_chr(row$docx.template.name))
			next
		}
		if (identical(row$show, FALSE) || identical(row$content.wrap, FALSE)) next
		name <- na_chr(row$name)
		if (!nzchar(name) || name %in% names_used) name <- paste0(if (nzchar(name)) name else "Style", " ", i)
		names_used <- c(names_used, name)
		acronym <- list(suppress = identical(row$acronym.show, FALSE),
		                case = switch(na_chr(row$acronym.case), tolower = "lower", toupper = "upper", capitalize = "capitalize", ""),
		                search = na_chr(row$acronym.search),
		                width = if (is.na(row$acronym.width)) 0 else row$acronym.width)
		if (nzchar(acronym$search)) acronym$replace <- na_chr(row$acronym.replace)
		if (!is.na(row$acronym.ending)) acronym$ending <- row$acronym.ending
		filler <- na_chr(row$content.indent.align.filler.inside)
		if (identical(na_chr(row$content.indent.align.mode), "bracket") && nzchar(filler)) fill <- filler
		style <- list(name = name, type = "tier", pattern = row$match.regex,
		              word = na_chr(row$docx.template.name),
		              line.numbers.suppress = identical(row$line.nr.show, FALSE),
		              acronym = acronym,
		              indent = if (is.na(row$content.indent)) "none" else row$content.indent,
		              align.chars = na_chr(row$content.indent.align.char),
		              align.mode = na_chr(row$content.indent.align.mode))
		if (!is.null(row$is.main.tier) && !is.na(row$is.main.tier)) style$main <- isTRUE(row$is.main.tier)
		styles[[length(styles) + 1]] <- style
	}
	list(
		label = label,
		mode = if (identical(l@layout.mode, "mondada")) "mondada" else "gat",
		header = isTRUE(l@header.insert),
		width = if (width_limited) l@transcript.width else 65,
		width.limit = width_limited,
		space.lines = TRUE,
		line.numbers = isTRUE(l@line.nr.show),
		acronym = list(show = TRUE, suppress.repeated = !isTRUE(l@speaker.repeat), case = "",
		               search = "", replace = "", extract = na_chr(l@speaker.regex),
		               width = acronym_width, ending = na_chr(l@speaker.ending)),
		word = list(file = "", look = "file"),
		functions = list(tiers.exclude = na_chr(l@filter.tier.excludeRegEx),
		                 tiers.keep = na_chr(l@filter.tier.includeRegEx),
		                 arrow = isTRUE(l@arrow.insert), arrow.shape = na_chr(l@arrow.shape)),
		advanced = list(spaces.before = l@spacesbefore, brackets.align = isTRUE(l@brackets.align),
		                symbol.merge = isTRUE(l@symbol.merge), fill = fill),
		styles = styles)
}
