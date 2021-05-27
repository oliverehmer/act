#' Layout object, defining the layout of print transcripts
#'
#' You can create an new layout object with \code{methods::new("layout")}.
#' This will give you a new layout object with the default settings uses by act.
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the values in the \code{@slots} and pass it as argument \code{l} to the respective functions.
#'
#' @slot name Character string; Name of the layout.
#' @slot filter.tier.include.regex Character string; as regular expression, tiers matching the expression will be included in the print transcript.
#' @slot filter.tier.exclude.regex Character string; as regular expression, tiers matching the expression will be excluded from the print transcript.
#' @slot transcript.width Integer; width of transcript, -1 for no line wrapping.
#' @slot speaker.width Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @slot speaker.ending Character string; string that is added at the end of the speaker name.
#' @slot spacesbefore Integer; number of spaces inserted before line number.
#' @slot additionalline1.insert Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{.additionalline1.text}.
#' @slot additionalline1.text Character string; Content of additional dummy line 1.
#' @slot additionalline1.indent Logical; if \code{TRUE} the content of the dummy line 1 will be indented to begin where the content of the annotations start.
#' @slot additionalline2.insert Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{.additionalline2.text}.
#' @slot additionalline2.text Character string; Content of additional dummy line 2.
#' @slot additionalline2.indent Logical; if \code{TRUE} the content of the dummy line 2 will be indented to begin where the content of the annotations start.
#' @slot brackets.tryToAlign Logical; if \code{TRUE} act will try to align brackets [] for parallel speaking (Attention: experimental function; results may not satisfy).
#' @slot pauseTier.regex Character string; regular expression to identify pause tier for auto formatting pauses.
#' @slot header.insert Logical; if \code{TRUE} a transcript header is inserted.
#' @slot header.heading.fromColumnName Character string; is only used when transcripts are made based on a search results; defines from which column of a search results table the heading is taken (if \code{object@.header.insert==TRUE})
#' @slot header.firstInfo.fromColumnName Character string; is only used when transcripts are made based on a search results; defines from which column of a search results table the first info is taken (if \code{object@.header.insert==TRUE})
#' @slot arrow.insert Logical; is only used when transcripts are made based on a search results; if \code{TRUE} an arrow will be inserted, highlighting the transcript line containing the search hit.
#' @slot arrow.shape Character string; shape of the arrow.
#' 
methods::setClass("layout", 
				  representation(
				  	name                            = "character",
				  	filter.tier.include.regex       = "character",
				  	filter.tier.exclude.regex       = "character",
				  	transcript.width 				= "numeric",
				  	speaker.width    				= "numeric",
				  	speaker.ending 					= "character",
				  	spacesbefore 					= "numeric",
				  	additionalline1.insert          = "logical",
				  	additionalline1.text            = "character",
				  	additionalline1.indent          = "logical",
				  	additionalline2.insert          = "logical",
				  	additionalline2.text            = "character",
				  	additionalline2.indent          = "logical",
				  	brackets.tryToAlign 			= "logical",
				  	pauseTier.regex   				= "character",
				  	header.insert 					= "logical",
				  	header.heading.fromColumnName 	= "character",
				  	header.firstInfo.fromColumnName = "character",
				  	arrow.insert 					= "logical",
				  	arrow.shape  					= "character"
				  ), prototype = list (
				  	name                            = "StandardLayout",
				  	filter.tier.include.regex       = character(),
				  	filter.tier.exclude.regex       = character(),
				  	transcript.width 				= 65,
				  	speaker.width    				= 3,
				  	speaker.ending 					= ":  ",
				  	spacesbefore 					= 3,
				  	additionalline1.insert          = FALSE,
				  	additionalline1.text            = "",
				  	additionalline1.indent          = TRUE,
				  	additionalline2.insert          = FALSE,
				  	additionalline2.text            = "",
				  	additionalline2.indent          = FALSE,
				  	brackets.tryToAlign 			= TRUE,
				  	pauseTier.regex   				= "",
				  	header.insert 					= TRUE,
				  	header.heading.fromColumnName 	= "resultID",
				  	header.firstInfo.fromColumnName = "header.firstinfo",
				  	arrow.insert 					= TRUE,
				  	arrow.shape  					= "->"

				  )
)

layout_show <- function (object) {
	cat("layout object", fill=TRUE)
	cat("  name                            : ", paste("'", object@name, "'",sep="", collapse=""),fill=TRUE)
	cat("\n")
	cat("  filter.tier.include.regex       : ", paste("'", object@filter.tier.include.regex, "'",sep="", collapse=""),fill=TRUE)
	cat("  filter.tier.exclude.regex       : ", paste("'", object@filter.tier.exclude.regex, "'",sep="", collapse=""),fill=TRUE)
	cat("\n")
	cat("  transcript.width                : ", object@transcript.width, fill=TRUE)
	cat("\n")
	cat("  speaker.width                   : ", object@speaker.width, fill=TRUE)
	cat("  speaker.ending                  : ", paste("'", object@speaker.ending, "'",sep="", collapse=""),fill=TRUE)
	cat("  spacesbefore                    : ", object@spacesbefore, fill=TRUE)
	cat("\n")
	cat("  additionalline1.insert          : ", paste("'", object@additionalline1.insert, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline1.text            : ", paste("'", object@additionalline1.text, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline1.indent          : ", object@additionalline1.indent, fill=TRUE)
	cat("  additionalline2.insert          : ", object@additionalline2.insert, fill=TRUE)
	cat("  additionalline2.text            : ", paste("'", object@additionalline2.text, "'",sep="", collapse=""),fill=TRUE)
	cat("  additionalline2.indent          : ", object@additionalline2.indent, fill=TRUE)
	cat("\n")
	cat("  brackets.tryToAlign             : ", object@brackets.tryToAlign, fill=TRUE)
	cat("  pauseTier.regex                 : ", paste("'", object@pauseTier.regex, "'",sep="", collapse=""),fill=TRUE)
	cat("\n")
	cat("  header.insert                   : ", object@header.insert, fill=TRUE)
	cat("  header.heading.fromColumnName   : ", paste("'", object@header.heading.fromColumnName, "'",sep="", collapse=""),fill=TRUE)
	cat("  header.firstInfo.fromColumnName : ", paste("'", object@header.firstInfo.fromColumnName,"'",sep="", collapse=""), fill=TRUE)
	cat("  arrow.insert                    : ", object@arrow.insert, fill=TRUE)
	cat("  arrow.shape                     : ", paste("'", object@arrow.shape,"'",sep="", collapse=""), fill=TRUE)
}

methods::setMethod("show", signature = "layout", definition = layout_show)