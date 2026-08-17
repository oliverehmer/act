#' Export print transcript in .docx format
#' 
#' LAYOUT
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#' Using the layout object you may
#' - Adjust with, abbreviation of speakers, etc.
#' - set filters to include/exclude tiers matching regular expressions.
#' - assign template files for .docx formatting using format templates
#'
#' FORMATING
# To format the transcript you can 
#' - adjust the the defaults format templates in the default .docx template.
#' - define further templates and add them to a styles matrix.
#' The paths to both files need to be set in your l layout object. Please check the slots l@docx.template.path and l@docx.styles.base.
#' You can see the structure of the default styles matrix in each new layout object in l@docx.styles.base. Use l@docx.styles.base <- act::export_styles_base_load(...) to assign a custom styles matrix.
#' The default format templates are
#' * Header: 
#' - header.preface (formats: s@results$header.description) 
#' - header.title (formats: s@results$header.description) 
#' - header.subtitle (formats: s@results$header.description) 
#' - header.description (formats: s@results$header.description) 
#' * Transcript body
#' - transcript.default (formats: any annotation in "t@annotations"
#' 
#'
#' @param t Transcript object.
#' @param l Layout object.
#' @param pathOutput Character string; path where to save the transcript.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param insertArrowStartsec Numeric; start time (seconds) of the hit annotation for arrow placement. Currently without effect: the alignment engine does not support hit arrows yet. Used with \code{insertArrowEndsec} and \code{insertArrowTierName} to locate the annotation by time and tier. If \code{NA}, no arrow is placed.
#' @param insertArrowEndsec Numeric; end time (seconds) of the hit annotation for arrow placement.
#' @param insertArrowTierName Character string; tier name of the hit annotation for arrow placement.
#' @param headerPreface Character string; text used as preface before title.
#' @param headerTitle Character string; text used as title.
#' @param headerSubtitle Character string; text  used as sub title.
#' @param headerDescription Character string; text used as description after sub title.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.
#' @param timeTolerancePoint Numeric; up to this distance in seconds two point marks (stills) count as the same moment.
#' @param timeToleranceGesture Numeric; up to this distance in seconds two span marks (gestures) count as the same moment.
#' @param layerOrder Vector of character strings; order of the multimodal layers within a block. \code{NULL} keeps the tier order of the annotation file.
#' @param minDescription Integer; minimum room in characters a description needs before the verbal line breaks early.
#' @param maxSpanBlocks Integer; maximum number of blocks a description may span before it is cut with a resume arrow.
#' @param figReplace Logical; if \code{TRUE} the content of picture tiers is replaced by a number mark.
#' @param figTierRegex Character string; regular expression identifying picture tiers.
#' @param report Logical; if \code{TRUE} and \code{pathOutput} is set, an alignment report is written next to the output file.
#' @param pathReport Character string; explicit path for the alignment report.

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
							l                            = NULL,
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
							timeTolerancePoint           = 0.2,
							timeToleranceGesture         = 0.5,
							layerOrder                   = NULL,
							minDescription               = 10L,
							maxSpanBlocks                = 3L,
							figReplace                   = TRUE,
							figTierRegex                 = "^stills(#|$)",
							report                       = FALSE,
							pathReport                   = NULL
) {
	.assert_transcript(t, missing = missing(t))
	if (missing(l) || is.null(l)) {
		l <- methods::new("layout")
		l@docx.template.path <- ""
	}
	if (!requireNamespace("officer", quietly = TRUE)) {
		cli::cli_abort("Please install the {.pkg officer} package.")
	}
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			cli::cli_abort("Output folder does not exist. Modify parameter {.arg pathOutput}.")
		}
	}
	if (is.na(l@transcript.width) || l@transcript.width == -1) {
	} else if (l@transcript.width < 40) {
		cli::cli_abort("The width of the transcript is to low. Minimum is 40. Check option {.code l@transcript.width}")
	}
	if (is.na(l@speaker.width) || l@speaker.width == -1) {
	} else if (l@speaker.width == 0 || l@speaker.width < -1) {
		cli::cli_abort("Length of tier names is to short. Minimum is 1. Check option {.code l@speaker.width}.")
	} else if (l@speaker.width > 25) {
		cli::cli_abort("Length of tier names is to long. Maximum is 25. Check option {.code l@speaker.width}.")
	}

	templates <- .resolve_docx_templates(l)
	template_suffixes <- if (length(templates) <= 1) {
		""
	} else {
		paste0("__", names(templates))
	}

	rendered <- helper_layout_render(t, l,
		filterTierNames       = filterTierNames,
		filterSectionStartsec = filterSectionStartsec,
		filterSectionEndsec   = filterSectionEndsec,
		timeTolerancePoint    = timeTolerancePoint,
		timeToleranceGesture  = timeToleranceGesture,
		layerOrder            = layerOrder,
		minDescription        = minDescription,
		maxSpanBlocks         = maxSpanBlocks,
		figReplace            = figReplace,
		figTierRegex          = figTierRegex)
	t <- rendered$transcript
	plan <- rendered$plan
	result <- rendered$result
	mondada <- identical(rendered$layoutMode, "mondada")

	results <- list()
	for (template_idx in seq_along(templates)) {
	doc <- officer::read_docx(path = templates[template_idx])
	doc <- .docx_add_header(doc, l, t, headerPreface, headerTitle,
	                        headerSubtitle, headerDescription,
	                        headerInsertSource)

	if (!is.null(result)) {
		style_default_name <- get_style_base(l, "transcript.default")$docx.template.name
		space_style_row <- get_style_user(l, name = "space")
		space_style_name <- if (!is.null(space_style_row) &&
		                        !is.na(space_style_row$docx.template.name)) {
			space_style_row$docx.template.name
		} else {
			style_default_name
		}
		previous_main_row <- NA_integer_
		emitted_any <- FALSE
		for (p in seq_len(nrow(plan))) {
			row_p <- plan$row[p]
			if (mondada) {
				if (emitted_any && isTRUE(result$is_main[row_p]) &&
				    isTRUE(result$number_lines[row_p])) {
					doc <- officer::body_add_par(doc, "", style = space_style_name)
				}
			} else if (emitted_any && isTRUE(result$is_main[row_p]) &&
			           !identical(row_p, previous_main_row)) {
				doc <- officer::body_add_par(doc, "", style = space_style_name)
			}
			if (isTRUE(result$show[row_p])) {
				doc <- officer::body_add_par(doc, value = plan$line[p],
				                             style = result$style[row_p])
				if (isTRUE(result$is_main[row_p])) previous_main_row <- row_p
				emitted_any <- TRUE
			}
		}
		if (emitted_any) {
			doc <- officer::body_add_par(doc, "", style = space_style_name)
		}
	}

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
			time_tolerance = timeToleranceGesture)
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

.docx_add_header <- function(doc, l, t, headerPreface, headerTitle,
                             headerSubtitle, headerDescription,
                             headerInsertSource) {
	if (!isTRUE(l@header.insert)) return(doc)
	add_block <- function(doc, value, style_name) {
		if (is.null(value) || is.na(value)) return(doc)
		style <- get_style_base(l, style_name)$docx.template.name
		for (line in unlist(stringr::str_split(value, "\n"))) {
			doc <- officer::body_add_par(doc, value = line, style = style)
		}
		doc
	}
	doc <- add_block(doc, headerPreface,     "header.preface")
	doc <- add_block(doc, headerTitle,       "header.title")
	doc <- add_block(doc, headerSubtitle,    "header.subtitle")
	doc <- add_block(doc, headerDescription, "header.description")
	if (isTRUE(headerInsertSource) && nrow(t@annotations) > 0) {
		source_line <- paste0("(", t@name, ", ",
			helper_format_time(min(t@annotations$startsec)), "-",
			helper_format_time(max(t@annotations$endsec)), ")")
		doc <- officer::body_add_par(doc, value = source_line,
			style = get_style_base(l, "header.subtitle")$docx.template.name)
	}
	doc
}

#==== FUNCTONS ====
get_style_base <- function(l, actStyleName) {
	id <- which(l@docx.styles.base$act.style.name==actStyleName)
	if (length(id)==0) {
		cli::cli_abort("Style {.val {actStyleName}} is not defined in your styles file. Add this style to your base styles.")
	} else {
		return (
			l@docx.styles.base[id[1],]
		)
	}
}

get_style_user <- function(l, name) {
	user_df <- l@docx.styles.user

	if (nrow(user_df) > 0 && "match.regex" %in% names(user_df)) {
		match_rows <- which(!is.na(user_df$match.regex))
		for (idx in match_rows) {
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
		line.nr.show      = TRUE,
		acronym.show      = TRUE,
		acronym.case      = NA_character_,
		acronym.search    = NA_character_,
		acronym.replace   = NA_character_,
		acronym.width     = 0,
		acronym.ending    = NA_character_,
		content.indent    = NA_character_,
		content.indent.text.skip        = NA_character_,
		content.indent.align.char       = NA_character_,
		content.indent.align.filler.inside = " ",
		content.indent.align.mode       = NA_character_,
		content.wrap      = TRUE,
		space.after       = NA_character_,
		comment           = NA_character_,
		stringsAsFactors  = FALSE
	))
}

export_docx_make_label <- function(transcript_name, headerTitle, startSec, endSec) {
	parts <- c()
	if (!is.null(headerTitle) && !is.na(headerTitle)) {
		parts <- c(parts, headerTitle)
	}
	if (!is.null(startSec) && !is.null(endSec)) {
		parts <- c(parts, paste0("[", round(startSec, 1), "s-", round(endSec, 1), "s]"))
	}
	if (length(parts) > 0) {
		paste0(paste(parts, collapse = " "), " / ", transcript_name)
	} else {
		transcript_name
	}
}

# ---- shared prerender: prepare the aligned annotation frame -------------
# Produces the styled + bracket/layer-aligned annotations (fixed
# transcript.width) shared by export_docx() and the transcript viewer.
