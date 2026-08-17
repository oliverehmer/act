#' Export print transcript in .txt format
#' 
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#' In the layout object you may also set additional filters to include/exclude tiers matching regular expressions.
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
						l                       = NULL,
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
						timeTolerancePoint      = 0.2,
						timeToleranceGesture    = 0.5,
						layerOrder              = NULL,
						minDescription          = 10L,
						maxSpanBlocks           = 3L,
						figReplace              = TRUE,
						figTierRegex            = "^stills(#|$)",
						report                  = FALSE,
						pathReport              = NULL) {

	.assert_transcript(t, missing = missing(t))
	if (missing(l) || is.null(l)) {
		l <- methods::new("layout")
		l@docx.template.path <- ""
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

	if (is.null(rendered$result)) {
		return("[no content]")
	}

	output <- rendered$lines

	if (isTRUE(l@header.insert)) {
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
				helper_format_time(min(t@annotations$startsec)), "-",
				helper_format_time(max(t@annotations$endsec)), ")")
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
			time_tolerance = timeToleranceGesture)
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
