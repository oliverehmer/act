#' Corpus object
#' 
#' This is the main object the act package uses. 
#' It collects the annotations and meta data from loaded annotation files.
#' 
#' Some of the slots are defined by the user.
#' Some slots report results, such as \code{@import.results} and \code{@history} and .
#' Other slots are settings and are used when performing functions on the corpus object.
#' 
#' @slot name Character string; Name of the corpus.
#' @slot transcripts List of transcript objects; Each annotation file that has been load is stored in this list as a transcript object.
#' @slot paths.annotation.files Vector of character strings; Path(s) to one or several folders where your annotation files are located. 
#' @slot paths.media.files Vector of character strings; Path(s) to one or several folders where your media files are located. 
#' @slot normalization.matrix Data.frame; Replacement matrix used for normalizing the annotations. To change the normalization matrix use \code{x@normalization.matrix <- act::matrix_load(path="...")}
#' @slot import.skip.double.files Character string; controls handling of duplicate transcript names. \code{"warn_keep_newest"} keeps the newest file and warns. \code{"warn_keep_first"} keeps the first file and warns. \code{"error"} aborts on duplicates. \code{"allow"} allows duplicates without warning. For backward compatibility, logical values are accepted: \code{TRUE} is treated as \code{"warn_keep_first"}, \code{FALSE} as \code{"allow"}. 
#' @slot import.names.include Vector of character strings; Only files matching this regular expression will be imported into the corpus.
#' @slot import.names.exclude Vector of character strings; Files matching this regular expression will be skipped and not imported into the corpus.
#' @slot import.names.modify List; Options how to modify the names of the transcript objects when they are added to the corpus. These options are useful, for instacne, if your annotation files contain character sequences that you do not want to include into the transcript name in the corpus (e.g. if you regularly add a date to the file name of your annotations files  as 'myFile_2020-09-21.TextGrid'). 
#' @slot import.results Data.frame; information about the import of the annotation files.
#' @slot media.assign.results Data.frame; information about the media assignment.
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
				  	import.skip.double.files              = "character",
				  	import.names.include                  = "character",
				  	import.names.exclude                  = "character",
				  	import.names.modify                   = "list",
				  	import.results                        = "data.frame",
				  	media.assign.results                  = "data.frame",
				  	history                               = "list"
				  ), prototype = list (
				  	name                                  = "NewCorpus",
				  	transcripts                           = list(),
				  	paths.annotation.files                = character(),
				  	paths.media.files                     = character(),
				  	normalization.matrix                  = data.frame(search=character(), replace=character(), description=character(), stringsAsFactors		= FALSE),
				    import.skip.double.files              = "warn_keep_newest",
				  	import.names.include                  = character(),
				  	import.names.exclude                  = character(),
				  	import.names.modify                   = list(  searchPatterns       = character(),
														searchReplacements   = character(),
														toUpper          = FALSE,
														toLower          = FALSE,
														trim                 = TRUE,
														defaultEmpty = "no_name"),
				  	import.results                        = data.frame(),
				  	media.assign.results                  = data.frame(),
				  	history                               = list()
				  )
)

corpus_show <- function (object) {
	w <- 24
	.show_title(paste0("corpus object: ", object@name))

	.show_head("Paths")
	.show_dl(c(
		"annotation.files" = as.character(length(object@paths.annotation.files)),
		"media.files"      = as.character(length(object@paths.media.files))
	), width = w)

	.show_sep()
	.show_head("Import")
	.show_dl(c(
		"normalization.matrix"    = paste(nrow(object@normalization.matrix), "row(s)"),
		"import.skip.double.files" = as.character(object@import.skip.double.files),
		"import.names.include"    = object@import.names.include,
		"import.names.exclude"    = object@import.names.exclude,
		"import.results"          = paste(sum(!is.na(object@import.results$message) & object@import.results$message != ""), "message(s)"),
		"media.assign.results"    = paste(nrow(object@media.assign.results), "entry(s)")
	), width = w)

	.show_sep()
	info <- act::info_summarized(object)
	.show_head("Summary")
	.show_dl(c(
		"transcripts"  = as.character(info$transcript.count),
		"tiers"        = as.character(info$tier.count),
		"annotations"  = as.character(info$annotations.count),
		"words (orig)" = as.character(info$words.org.count),
		"words (norm)" = as.character(info$words.norm.count),
		"length"       = info$length.formatted
	), width = w)
}

methods::setMethod("show", signature = "corpus", definition = corpus_show)