#' Export print transcript in .txt format
#' 
#' Writes a print transcript as plain text. Its layout comes from a transcript
#' profile (parameter \code{style}); see \link{export_docx}.
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
#' @param collapse Logical; if \code{FALSE} a vector will be created, each element corresponding to one annotation. if \code{TRUE} a single string will be created, collapsed by linebreaks \\n.
#' 
#' @return Character string; transcript as text.
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}, \link{export_docx} 
#' 
#' @export
#'
#' @example inst/examples/export_txt.R
#'  
#'
export_txt <- function (t,
						pathOutput              = NULL,
						filterTierNames         = NULL,
						filterSectionStartsec   = NULL,
						filterSectionEndsec     = NULL,
						insertArrowStartsec     = NA_real_,
						insertArrowEndsec       = NA_real_,
						insertArrowTierName     = NA_character_,
						headerPreface           = NULL,
						headerTitle             = NULL,
						headerSubtitle          = NULL,
						headerDescription       = NULL,
						headerInsertSource      = TRUE,
						collapse                = TRUE,
						layerOrder              = NULL,
						report                  = FALSE,
						pathReport              = NULL,
						mainTierNames           = NULL,
						alignChars              = NULL,
						alignModes              = NULL,
						style                   = NULL) {

	.assert_transcript(t, missing = missing(t))
	l <- .style_layout(style)
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

	if (is.null(rendered$result)) {
		return("[no content]")
	}

	output <- rendered$lines
	time_format <- .layout_time_format(l)

	if (isTRUE(l[["header.insert"]])) {
		header <- ''
		if (!is.null(headerPreface) && !is.na(headerPreface)) {
			header <- paste0(header, headerPreface, "\n")
		}
		if (!is.null(headerTitle) && !is.na(headerTitle)) {
			header <- paste0(header, headerTitle, "\n")
		}
		if (!is.null(headerSubtitle) && !is.na(headerSubtitle)) {
			header <- paste0(header, headerSubtitle, "\n")
		}
		if (!is.null(headerDescription) && !is.na(headerDescription)) {
			header <- paste0(header, headerDescription, "\n")
		}
		if (isTRUE(headerInsertSource)) {
			standardsource <- paste0("(", t@name, ", ",
				helper_time_format(min(t@annotations$startsec), format = time_format), "-",
				helper_time_format(max(t@annotations$endsec), format = time_format), ")")
			header <- paste0(header, standardsource, "\n")
		}
		if (nchar(header) > 0) {
			output <- c(header, output)
		}
	}

	if (is.null(pathReport) && isTRUE(report)) {
		pathReport <- alignment_report_path(pathOutput)
	}
	if (!is.null(pathReport)) {
		report_lines <- build_alignment_report(
			rendered$result, rendered$plan, transcript_name = t@name,
			layout_mode = rendered$layoutMode,
			text_body_width = rendered$engineWidth,
			time_tolerance = .layout_style(l)$advanced$tolerance.gesture)
		con <- file(pathReport, open = "w", encoding = "UTF-8")
		writeLines(report_lines, con = con)
		close(con)
	}
	report_render_warnings(rendered$result, transcript_name = t@name)

	if (collapse) {
		output <- stringr::str_c(output, sep='\n', collapse = '\n')
		output <- stringr::str_c(c(output, '\n'), sep='', collapse = '')
	}

	if (!is.null(pathOutput)) {
		fileConn <- file(pathOutput, open="wb")
		writeLines(enc2utf8(output), fileConn, sep="\n", useBytes=TRUE)
		close(fileConn)
	}

	return(output)
}


#' @rdname export_txt
#' @param ... Arguments passed to `export_txt()`.
#'
#' @seealso \code{\link{export_txt}}
#'
#' @export
export_printtranscript <- function(...) {
	export_txt(...)
}
