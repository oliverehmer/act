#' Layout object, defining the layout of print transcripts
#'
#' You can check the default layout settings with \code{methods::new("layout")}
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l} to the respective functions.
#'
#' @slot name Character string; Name of the layout.
#' @slot transcript.width Integer; width of transcript, -1 for no line wrapping.
#' @slot header.insert Logical; if \code{TRUE} a transcript header is inserted.
#' @slot header.heading.fromColumnName Character string; from which column the heading is taken (if \code{object$.header.insert==TRUE})
#' @slot header.firstInfo.fromColumnName Character string; from which column the first info is taken (if \code{object$.header.insert==TRUE})
#' @slot speaker.width Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @slot speaker.ending Character string; string that is added at the end of the speaker name.
#' @slot spacesbefore Integer; number of spaces inserted before line number.
#' @slot additionalline1.insert Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{.additionalline1.text}.
#' @slot additionalline1.text Character string; Content of additional dummy line 1.
#' @slot additionalline1.indent Logical; if \code{TRUE} the content of the dummy line 1 will be indented to begin where the content of the annotations start.
#' @slot additionalline2.insert Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{.additionalline2.text}.
#' @slot additionalline2.text Character string; Content of additional dummy line 2.
#' @slot additionalline2.indent Logical; if \code{TRUE} the content of the dummy line 2 will be indented to begin where the content of the annotations start.
#' @slot arrow.insert Logical; if \code{TRUE} an arrow will be inserted, highlighting the transcript line containing the search hit.
#' @slot arrow.shape Character string; shape of the arrow.
#' @slot brackets.tryToAlign Logical; if \code{TRUE} act will try to align brackets [] for parallel speaking (Attention: experimental function; results may not satisfy).
#' @slot pauseTier.regex Character string; regular expression to identify pause tier for auto formatting pauses.
#' 
methods::setClass("layout", 
				  representation(
				  	name                            = "character",
				  	transcript.width 				= "numeric",
				  	header.insert 					= "logical",
				  	header.heading.fromColumnName 	= "character",
				  	header.firstInfo.fromColumnName = "character",
				  	speaker.width    				= "numeric",
				  	speaker.ending 					= "character",
				  	spacesbefore 					= "numeric",
				  	additionalline1.insert          = "logical",
				  	additionalline1.text            = "character",
				  	additionalline1.indent          = "logical",
				  	additionalline2.insert          = "logical",
				  	additionalline2.text            = "character",
				  	additionalline2.indent          = "logical",
				  	arrow.insert 					= "logical",
				  	arrow.shape  					= "character",
				  	brackets.tryToAlign 			= "logical",
				  	pauseTier.regex   				= "character"
				  ), prototype = list (
				  	name                            = "StandardLayout",
				  	transcript.width 				= 65,
				  	header.insert 					= TRUE,
				  	header.heading.fromColumnName 	= "resultID",
				  	header.firstInfo.fromColumnName = "header.firstinfo",
				  	speaker.width    				= 3,
				  	speaker.ending 					= ":  ",
				  	spacesbefore 					= 3,
				  	additionalline1.insert          = FALSE,
				  	additionalline1.text            = "",
				  	additionalline1.indent          = TRUE,
				  	additionalline2.insert          = FALSE,
				  	additionalline2.text            = "",
				  	additionalline2.indent          = FALSE,
				  	arrow.insert 					= TRUE,
				  	arrow.shape  					= "->",
				  	brackets.tryToAlign 			= TRUE,
				  	pauseTier.regex   				= ""
				  )
)

layout_show <- function (object) {
	cat("layout object", fill=TRUE)
	cat("  name                            : ", paste("'", object$name, "'",sep="", collapse=""),fill=TRUE)
	cat("  transcript.width                : ", object$transcript.width, fill=TRUE)
	cat("  header.insert                   : ", object$header.insert, fill=TRUE)
	cat("  header.heading.fromColumnName   : ", paste("'", object$header.heading.fromColumnName, "'",sep="", collapse=""),fill=TRUE)
	cat("  header.firstInfo.fromColumnName : ", paste("'", object$header.firstInfo.fromColumnName,"'",sep="", collapse=""), fill=TRUE)
	cat("  speaker.width                   : ", object$speaker.width, fill=TRUE)
	cat("  speaker.ending                  : ", paste("'", object$speaker.ending, "'",sep="", collapse=""),fill=TRUE)
	cat("  spacesbefore                    : ", object$spacesbefore, fill=TRUE)
	cat("  additionalline1.insert          : ", paste("'", object$additionalline1.insert, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline1.text            : ", paste("'", object$additionalline1.text, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline1.indent          : ", object$additionalline1.indent, fill=TRUE)
	cat("  additionalline2.insert          : ", object$additionalline2.insert, fill=TRUE)
	cat("  additionalline2.text            : ", paste("'", object$additionalline2.text, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline2.indent          : ", object$additionalline2.indent, fill=TRUE)
	cat("  arrow.insert                    : ", object$arrow.insert, fill=TRUE)
	cat("  arrow.shape                     : ", paste("'", object$arrow.shape,"'",sep="", collapse=""), fill=TRUE)
	cat("  brackets.tryToAlign             : ", object$brackets.tryToAlign, fill=TRUE)
	cat("  pauseTier.regex                 : ", paste("'", object$pauseTier.regex, "'",sep="", collapse=""),fill=TRUE)
}

methods::setMethod("show", signature = "layout", definition = layout_show)