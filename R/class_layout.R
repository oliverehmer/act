#' Layout object, defining the layout of print transcripts
#'
#' You can create an new layout object with \code{methods::new("layout")}.
#' This will give you a new layout object with the default settings uses by act.
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the values in the \code{@slots} and pass it as argument \code{l} to the respective functions.
#'
#' @slot name Character string; Name of the layout.
#' @slot filter.tier.includeRegEx Character string; as regular expression, tiers matching the expression will be included in the print transcript.
#' @slot filter.tier.excludeRegEx Character string; as regular expression, tiers matching the expression will be excluded from the print transcript.
#' @slot transcript.width Integer; width of transcript. Use \code{-1} or \code{NA} to disable line wrapping.
#' @slot speaker.regex Character string; Regular expression to extract speaker abbreviation from tier name
#' @slot speaker.width Integer; maximum width of speaker abbreviation. Use \code{-1} or \code{NA} for full name without shortening.
#' @slot speaker.ending Character string; string that is added at the end of the speaker name.
#' @slot speaker.repeat Logical; if \code{TRUE} the speaker acronym is repeated in every line; if \code{FALSE} (default) the acronym is suppressed in consecutive lines of the same speaker.
#' @slot line.nr.show Logical; if \code{TRUE} (default) line numbers are shown in print transcripts (TXT and DOCX). In DOCX export, this is the global default; the per-tier setting \code{line.nr.show} in the styles matrix (slot \code{docx.styles.user}) overrides the layout default when not \code{NA}. Behaviour matrix:\cr
#' \tabular{lll}{
#' \strong{layout@line.nr.show} \tab \strong{tier line.nr.show (matrix)} \tab \strong{result} \cr
#' TRUE  \tab NA    \tab TRUE  \cr
#' TRUE  \tab FALSE \tab FALSE (tier wins) \cr
#' TRUE  \tab TRUE  \tab TRUE  \cr
#' FALSE \tab NA    \tab FALSE \cr
#' FALSE \tab FALSE \tab FALSE \cr
#' FALSE \tab TRUE  \tab TRUE  (tier wins) \cr
#' }
#' The TXT export has no per-tier styles, so the layout slot is the only control point.
#' @slot spacesbefore Integer; number of spaces inserted before line number.
#' @slot brackets.align Logical; if \code{TRUE} act will try to align brackets [] for parallel speaking (Attention: experimental function; results may not satisfy).
#' @slot header.insert Logical; if \code{TRUE} a transcript header is inserted.
#' @slot arrow.insert Logical; is only used when transcripts are made based on a search results; if \code{TRUE} an arrow will be inserted, highlighting the transcript line containing the search hit.
#' @slot layout.mode Character string; rendering mode of the alignment engine: 'gat' (one line per annotation) or 'mondada' (score layout, rows of one tier joined).
#' @slot symbol.merge Logical; if \code{TRUE}, adjacent identical multimodal marks denoting the same moment are folded into one mark.
#' @slot arrow.shape Character string; shape of the arrow.
#' @slot docx.template.path Character string;
#' @slot docx.styles.base Data.frame; Matrix with mappings of act style names to DOCX template paragraph styles. To change the styles matrix use \code{l@docx.styles.base <- act::export_styles_base_load(path="...")}
#' @slot docx.styles.user Data.frame; Matrix with user-defined tier-specific formatting rules (regex-based). Empty data.frame means all tiers use layout defaults.
#'
#' @seealso \link{matrix_load}
#'
#' @export
#'

methods::setClass("layout",
				  representation(
				  	name                            = "character",
				  	filter.tier.includeRegEx        = "character",
				  	filter.tier.excludeRegEx        = "character",
				  	transcript.width 				= "numeric",
				  	speaker.regex    				= "character",
				  	speaker.width    				= "numeric",
				  	speaker.ending 					= "character",
				  	speaker.repeat 					= "logical",
				  	line.nr.show 					= "logical",
				  	spacesbefore 					= "numeric",
				  	layout.mode 					= "character",
				  	symbol.merge 					= "logical",
				  	brackets.align 		         	= "logical",
				  	header.insert 					= "logical",
				  	arrow.insert 					= "logical",
				  	arrow.shape  					= "character",
				  	docx.template.path              = "character",
				  	docx.styles.base                = "data.frame",
				  	docx.styles.user                = "data.frame"
				  ), prototype = list(
				  	name                            = "StandardLayout",
				  	filter.tier.includeRegEx        = NA_character_,
				  	filter.tier.excludeRegEx        = NA_character_,
				  	transcript.width 				= 65,
				  	speaker.regex                   = NA_character_,
				  	speaker.width    				= 3,
				  	speaker.ending 					= ":  ",
				  	speaker.repeat 					= FALSE,
				  	line.nr.show 					= TRUE,
				  	spacesbefore 					= 3,
				  	layout.mode 					= "gat",
				  	symbol.merge 					= TRUE,
				  	brackets.align 		         	= TRUE,
				  	header.insert 					= TRUE,
				  	arrow.insert 					= TRUE,
				  	arrow.shape  					= "->",
				  	docx.template.path              = '',
				  	docx.styles.base                = data.frame(act.style.name=character(), docx.template.name=character(), stringsAsFactors = FALSE),
				  	docx.styles.user                = data.frame(stringsAsFactors = FALSE)
				  )
)

layout_show <- function (object) {
	w <- 24
	.show_title(paste0("layout object: ", object@name))

	.show_head("Filters")
	.show_dl(c(
		"filter.tier.includeRegEx" = object@filter.tier.includeRegEx,
		"filter.tier.excludeRegEx" = object@filter.tier.excludeRegEx
	), width = w)

	.show_sep()
	.show_head("Transcript")
	.show_dl(c(
		"transcript.width" = as.character(object@transcript.width),
		"speaker.regex"    = object@speaker.regex,
		"speaker.width"    = as.character(object@speaker.width),
		"speaker.ending"   = object@speaker.ending,
		"speaker.repeat"   = as.character(object@speaker.repeat),
		"line.nr.show"     = as.character(object@line.nr.show),
		"spacesbefore"     = as.character(object@spacesbefore),
		"layout.mode"      = object@layout.mode,
		"symbol.merge"     = as.character(object@symbol.merge),
		"brackets.align"   = as.character(object@brackets.align)
	), width = w)

	.show_sep()
	.show_head("Header & Arrow")
	.show_dl(c(
		"header.insert" = as.character(object@header.insert),
		"arrow.insert"  = as.character(object@arrow.insert),
		"arrow.shape"   = object@arrow.shape
	), width = w)

	.show_sep()
	.show_head("DOCX")
	if (length(object@docx.template.path) <= 1) {
		template_display <- object@docx.template.path
	} else {
		tpl_names <- names(object@docx.template.path)
		if (!is.null(tpl_names)) {
			template_display <- paste0(length(object@docx.template.path), " template(s): ", paste(tpl_names, collapse = ", "))
		} else {
			template_display <- paste0(length(object@docx.template.path), " template(s)")
		}
	}
	.show_dl(c(
		"docx.template.path" = template_display,
		"docx.styles.base"   = paste(nrow(object@docx.styles.base), "row(s)"),
		"docx.styles.user"   = paste(nrow(object@docx.styles.user), "row(s)")
	), width = w)
}
methods::setMethod("show", signature = "layout", definition = layout_show)

methods::setMethod(
	"initialize",
	"layout",
	function(.Object, ...) {

		# call the default initializer first
		.Object <- methods::callNextMethod()

		# custom behavior
		#message("A new layout object has been created.")
		#load the default styles matrix to new layout objects
		.Object@docx.styles.base <- export_styles_base_load()
		.Object@docx.styles.user <- export_styles_user_load()

		.Object
	}
)
