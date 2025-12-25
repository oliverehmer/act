#' Layout object, defining the layout of print transcripts
#'
#' You can create an new layout object with \code{methods::new("layout")}.
#' This will give you a new layout object with the default settings uses by act.
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the values in the \code{@slots} and pass it as argument \code{l} to the respective functions.
#'
#' @slot name Character string; Name of the layout.
#' @slot filter.tier.includeRegEx Character string; as regular expression, tiers matching the expression will be included in the print transcript.
#' @slot filter.tier.excludeRegEx Character string; as regular expression, tiers matching the expression will be excluded from the print transcript.
#' @slot transcript.width Integer; width of transcript, -1 for no line wrapping.
#' @slot speaker.regex Character string; Regular expression to extract speaker abbreviation from tier name
#' @slot speaker.width Integer; maximum width of speaker abbreviation, -1 for full name without shortening.
#' @slot speaker.ending Character string; string that is added at the end of the speaker name.
#' @slot spacesbefore Integer; number of spaces inserted before line number.
#' @slot brackets.align Logical; if \code{TRUE} act will try to align brackets [] for parallel speaking (Attention: experimental function; results may not satisfy).
#' @slot header.insert Logical; if \code{TRUE} a transcript header is inserted.
#' @slot arrow.insert Logical; is only used when transcripts are made based on a search results; if \code{TRUE} an arrow will be inserted, highlighting the transcript line containing the search hit.
#' @slot arrow.shape Character string; shape of the arrow.
#' @slot docx.template.path Character string; 
#' @slot docx.styles Data.frame; Matrix with mappings of act variables and the format templates in the .docx template files. To change the styles matrix use \code{l@docx.styles <- act::export_docx_styles_load(path="...")}
#'
#' @seealso \link{act::matrix_load}
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
				  	spacesbefore 					= "numeric",
				  	brackets.align 		         	= "logical",
				  	header.insert 					= "logical",
				  	arrow.insert 					= "logical",
				  	arrow.shape  					= "character",
				  	docx.template.path              = "character",
				  	docx.styles                     = "data.frame"
				  ), prototype = list(
				  	name                            = "StandardLayout",
				  	filter.tier.includeRegEx        = NA_character_,
				  	filter.tier.excludeRegEx        = NA_character_,
				  	transcript.width 				= 65,
				  	speaker.regex                   = NA_character_,
				  	speaker.width    				= 3,
				  	speaker.ending 					= ":  ",
				  	spacesbefore 					= 3,
				  	brackets.align 		         	= TRUE,
				  	header.insert 					= TRUE,
				  	arrow.insert 					= TRUE,
				  	arrow.shape  					= "->",
				  	docx.template.path              = '',
				  	docx.styles                     = data.frame(act.style.name=character(),	act.style.type=character(), docx.template.type=character(), docx.template.name=character(), stringsAsFactors		= FALSE)
				  )
)

layout_show <- function (object) {
	cat("layout object", fill=TRUE)
	cat("  name                    : ", paste("'", object@name, "'",sep="", collapse=""),fill=TRUE)
	cat("\n")
	cat("  filter.tier.includeRegEx: ", paste("'", object@filter.tier.includeRegEx, "'",sep="", collapse=""),fill=TRUE)
	cat("  filter.tier.excludeRegEx: ", paste("'", object@filter.tier.excludeRegEx, "'",sep="", collapse=""),fill=TRUE)
	cat("\n")
	cat("  transcript.width        : ", object@transcript.width, fill=TRUE)
	cat("\n")
	cat("  speaker.regex           : ", object@speaker.regex, fill=TRUE)
	cat("  speaker.width           : ", object@speaker.width, fill=TRUE)
	cat("  speaker.ending          : ", paste("'", object@speaker.ending, "'",sep="", collapse=""),fill=TRUE)
	cat("  spacesbefore            : ", object@spacesbefore, fill=TRUE)
	cat("\n")
	cat("  brackets.align          : ", object@brackets.align, fill=TRUE)
	cat("\n")
	cat("  header.insert           : ", object@header.insert, fill=TRUE)
	cat("  arrow.insert            : ", object@arrow.insert, fill=TRUE)
	cat("  arrow.shape             : ", paste("'", object@arrow.shape,"'",sep="", collapse=""), fill=TRUE)
	cat("\n")
	cat("  docx.template.path      : ", paste("'", object@docx.template.path,"'",sep="", collapse=""), fill=TRUE)
	cat("  docx.styles             : ", '[check directly]', nrow(object@docx.styles), 'row(s)', fill=TRUE)
}
methods::setMethod("show", signature = "layout", definition = layout_show)




methods::setMethod(
	"initialize",
	"layout",
	function(.Object, ...) {
		
		# call the default initializer first
		.Object <- callNextMethod()
		
		# custom behavior
		#message("A new layout object has been created.")
		#load the default styles matrix to new layout objects
		.Object@docx.styles <- export_docx_styles_load()
		
		.Object
	}
)

