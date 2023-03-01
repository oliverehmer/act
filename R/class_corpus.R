#' Corpus object
#' 
#' This is the main object the act package uses. 
#' It collects the annotations and meta data from loaded annotation files.
#' 
#' Some of the slots are defined by the user.
#' Some slots report results, such as \code{@import.results} and \code{@history} and .
#' Other slots are settings and are used when performing functions on the corpus oibject.
#' To change the normalization matrix use \code{x@normalization.matrix <- act::matrix_load(path="...")}
#'
#' @slot name Character string; Name of the corpus.
#' @slot transcripts List of transcript objects; Each annotation file that has been load is stored in this list as a transcript object.
#' @slot paths.annotation.files Vector of character strings; Path(s) to one or several folders where your annotation files are located. 
#' @slot paths.media.files Vector of character strings; Path(s) to one or several folders where your media files are located. 
#' @slot normalization.matrix Data.frame; Replacement matrix used for normalizing the annotations. 
#' @slot import.skip.double.files Logical; if \code{TRUE} files with the same names will be skipped (only one of them will be loaded), if \code{FALSE} transcripts will be renamed to make the names unique. 
#' @slot import.include Vector of character strings; Only files matching this regular expression will be imported into the corpus.
#' @slot import.exclude Vector of character strings; Files matching this regular expression will be skipped and not imported into the corpus.
#' @slot import.modify.transcript.names List; Options how to modify the names of the transcript objects when they are added to the corpus. These options are useful, for instacne, if your annotation files contain character sequences that you do not want to include into the transcript name in the corpus (e.g. if you regularly add a date to the file name of your annotations files  as 'myFile_2020-09-21.TextGrid'). 
#' @slot import.results Data.frame; information about the import of the annotation files. 
#' @slot history List; History of modifications made by any of the package functions to the corpus.
#'
#' @export
#'
#' @examples
#' library(act)
#' 
#' examplecorpus
#'
methods::setClass("corpus", 
				  representation(
				  	name                                  = "character",
				  	transcripts                           = "list",
				  	paths.annotation.files                = "character",
				  	paths.media.files                     = "character",
				  	normalization.matrix                  = "data.frame",
				  	import.skip.double.files              = "logical",
				  	import.include                        = "character",
				  	import.exclude                        = "character",
				  	import.modify.transcript.names        = "list",
				  	import.results                        = "data.frame",
				  	history                               = "list"
				  ), prototype = list (
				  	name                                  = "NewCorpus",
				  	transcripts                           = list(),
				  	paths.annotation.files                = character(),
				  	paths.media.files                     = character(),
				  	normalization.matrix                  = data.frame(search=character(), replace=character(), description=character(), stringsAsFactors		= FALSE),
				  	import.skip.double.files              = TRUE,
				  	import.include                        = character(),
				  	import.exclude                        = character(),
				  	import.modify.transcript.names        = list(   searchPatterns       = character(),
																	searchReplacements   = character(),
																	toUpperCase          = FALSE,
																	toLowerCase          = FALSE,
																	trim                 = TRUE,
																	defaultForEmptyNames = "no_name"),
				  	import.results                        = data.frame(),
				  	history                               = list()
				  )
)

corpus_show <- function (object) {
	#cat("===================== corpus", fill=TRUE)
	cat("corpus object", fill=TRUE)
	cat("  name                           : ", object@name, fill=TRUE)
	cat("\n")
	cat("  paths.annotation.files         : ", length(object@paths.annotation.files), "folder(s)", fill=TRUE)
	cat("  paths.media.files              : ", length(object@paths.media.files), "folder(s)", fill=TRUE)
	cat("\n")
	cat("  normalization.matrix           : ", '[check directly]', nrow(object@normalization.matrix), 'row(s)', fill=TRUE)
	cat("  import.skip.double.files       : ", object@import.skip.double.files, fill=TRUE)
	cat("  import.include                 : ", object@import.include, fill=TRUE)
	cat("  import.exclude                 : ", object@import.exclude, fill=TRUE)
	cat("  import.modify.transcript.names : ", '[check directly]', fill=TRUE)
	cat("  import.results                 : ", '[check directly]', nrow(object@import.results), 'message(s)', fill=TRUE)
	cat("  history                        : ", '[check directly]', fill=TRUE)
	
	cat("\n")
	cat("Aggregated info from act::info_summarized():", fill=TRUE)
	info <- act::info_summarized(object)
	cat("  transcript.count               : ", info$transcript.count, fill=TRUE)
	cat("  tier.count                     : ", info$tier.count, fill=TRUE)
	cat("  annotations.count              : ", info$annotations.count, fill=TRUE)
	cat("  words.org.count                : ", info$words.org.count, fill=TRUE)
	cat("  words.norm.count               : ", info$words.norm.count, fill=TRUE)
	cat("  length.formatted               : ", info$length.formatted, fill=TRUE)
	cat()
	#cat("  transcript.names              : ", paste("'", info$transcript.names,"'",sep="", collapse=", "), fill=TRUE)
	#cat("  tier.names                    : ", paste("'", info$tier.names,"'",sep="", collapse=", "), fill=TRUE)
	#cat("  length.sec                    : ", info$length.sec, fill=TRUE)
}

methods::setMethod("show", signature = "corpus", definition = corpus_show)