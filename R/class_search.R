#' Search object
#'
#' This object defines the properties of a search in act.
#' It also contains the results of this search in a specific corpus, if the search has already been run. (Note that you can also create a search without running it immediately).
#' A search object can be run on different corpora.
#'  
#' Some of the slots are defined by the user.
#' Other slots are \code{[READ ONLY]}, which means that they can be accessed by the user but 
#' should not be changed. They contain values that are filled when you execute functions 
#' on the object.
#'
#' @slot name Character string; name of the search. Will be used, for example, as name of the sub folder when creating media cuts
#' @slot pattern Character string; search pattern as a regular expression.
#' @slot search.mode Character string; defines if the original contents of the annotations should be searched or if the full texts should be searched. Slot takes the following values: \code{content}, \code{fulltext} (=default, includes both full text modes), \code{fulltext.byTime}, \code{fulltext.byTier}. 
#' @slot search.normalized logical. if \code{TRUE} the normalized annotations will be used for searching.
#' @slot resultid.prefix Character string; search results will be numbered consecutively; This character string will be placed before the consecutive numbers.
#' @slot resultid.start Integer; search results will be numbered consecutively; This is the start number of the identifiers.
#' @slot filter.transcript.names Vector of character strings; names of transcripts to include in the search. If the value is \code{character()} or \code{""} filter will be ignored.
#' @slot filter.transcript.includeRegEx  Character string; Regular expression that defines which transcripts should be INcluded in the search (matching the name of the transcript).
#' @slot filter.transcript.excludeRegEx  Character string; Regular expression that defines which transcripts should be EXcluded in the search (matching the name of the transcript).
#' @slot filter.tier.names Vector of character strings; names of tiers to include in the search. If the value is \code{character()} or \code{""} filter will be ignored.
#' @slot filter.tier.includeRegEx Character string; Regular expression that defines which tiers should be INcluded in the search (matching the name of the tier).
#' @slot filter.tier.excludeRegEx Character string; Regular expression that defines which tiers should be EXcluded in the search (matching the name of the tier).
#' @slot filter.section.startsec Double; Time value in seconds, limiting the search to a certain time span in each transcript, defining the start of the search window.
#' @slot filter.section.endsec Double; Time value in seconds, limiting the search to a certain time span in each transcript, defining the end of the search window.
#' @slot concordance.make Logical; If a concordance should be created when the search is run.
#' @slot concordance.width Integer; number of characters to include in the concordance.
#' @slot cuts.span.beforesec Double; Seconds how much the cuts (media and print transcripts) should start before the start of the search hit.
#' @slot cuts.span.aftersec Double; Seconds how much the cuts (media and print transcripts) should end after the end of the search hit.
#' @slot cuts.column.srt Character string; name of  destination column in the search results data frame where the .srt subtitles will be inserted; column will be created if not present in data frame; set to "" for no insertion.
#' @slot cuts.column.printtranscript Character string; name of  destination column in the search results data frame where the print transcripts will be inserted; column will be created if not present in data frame; set to "" for no insertion.
#' @slot cuts.printtranscripts Character string; \code{[READ ONLY]} All print transcripts for the search results (if generated previously)
#' @slot cuts.cutlist.mac Character string; \code{[READ ONLY]} 'FFmpeg' cut list for use on a Mac, to cut the media files for the search results.
#' @slot cuts.cutlist.win Character string; \code{[READ ONLY]} 'FFmpeg' cut list for use on Windows, to cut the media files for the search results.
#' @slot results Data.frame; Results of the search.1
#' @slot results.nr Integer; \code{[READ ONLY]} Number of search results.
#' @slot results.tiers.nr Integer; \code{[READ ONLY]} Number of tiers over which the search results are distrubuted.
#' @slot results.transcripts.nr Integer; \code{[READ ONLY]} Number of transcripts over which the search results are distrubuted.
#' @slot x.name Character string; \code{[READ ONLY]} name of the corpus object on which the search has been run.
#'
#' @seealso \link{export_docx}
#'
#' @export
#'
#' @example inst/examples/search_new.R
#' 
methods::setClass("search", 
				  representation(
				  	name="character",
				  	pattern="character",
				  	search.mode="character",
				  	search.normalized="logical",
				  	resultid.prefix="character",
				  	resultid.start = "numeric",
				  	filter.transcript.names="character",
				  	filter.transcript.includeRegEx ="character",
				  	filter.transcript.excludeRegEx ="character",
				  	filter.tier.names="character",
				  	filter.tier.includeRegEx="character",
				  	filter.tier.excludeRegEx="character",
				  	filter.section.startsec="numeric",
				  	filter.section.endsec="numeric",
				  	
				  	concordance.make="logical",
				  	concordance.width="numeric",

				  	cuts.span.beforesec ="numeric",
				  	cuts.span.aftersec ="numeric",
				  	cuts.column.srt = "character",
				  	cuts.column.printtranscript = "character",
				  	cuts.printtranscripts="character",
				  	cuts.cutlist.mac="character",
				  	cuts.cutlist.win="character",
				  	
				  	results="data.frame",
				  	results.nr="numeric",
				  	results.tiers.nr="numeric",
				  	results.transcripts.nr="numeric",
				  	x.name="character"
				  	
				  ), prototype = list(
				  	name="mysearch",
				  	pattern="",
				  	search.mode="fulltext",
				  	search.normalized=TRUE,
				  	resultid.prefix="resultID",
				  	resultid.start=1,
				  	
				  	filter.transcript.names=character(),
				  	filter.transcript.includeRegEx ="",
				  	filter.transcript.excludeRegEx ="",
				  	filter.tier.names=character(),
				  	filter.tier.includeRegEx="",
				  	filter.tier.excludeRegEx="",
				  	filter.section.startsec=numeric(),
				  	filter.section.endsec=numeric(),
				  	
				  	concordance.make=TRUE,
				  	concordance.width=120,
				  	
				  	cuts.span.beforesec =0,
				  	cuts.span.aftersec =0,
				  	cuts.column.srt = "srt",
				  	cuts.column.printtranscript = "printtranscript",
				  	cuts.printtranscripts=character(),
				  	cuts.cutlist.mac=character(),
				  	cuts.cutlist.win=character(),
				  	
				  	results=data.frame(),
				  	results.nr=0,
				  	results.tiers.nr=0,
				  	results.transcripts.nr=0,
				  	x.name=character()
				  )
)

search_show <- function (object) {
	w <- 25
	.show_title(paste0("search object: ", object@name))

	.show_head("Search")
	.show_dl(c(
		"pattern"           = object@pattern,
		"search.mode"       = object@search.mode,
		"search.normalized" = as.character(object@search.normalized),
		"resultid.prefix"   = object@resultid.prefix,
		"resultid.start"    = as.character(object@resultid.start)
	), width = w)

	.show_sep()
	.show_head("Filters")
	filter_startsec <- if (length(object@filter.section.startsec)==0 || is.na(object@filter.section.startsec)) "[not set]" else as.character(object@filter.section.startsec)
	filter_endsec   <- if (length(object@filter.section.endsec)==0 || is.na(object@filter.section.endsec)) "[not set]" else as.character(object@filter.section.endsec)
	.show_dl(c(
		"transcript.names"        = paste(object@filter.transcript.names, collapse=", "),
		"transcript.includeRegEx" = object@filter.transcript.includeRegEx,
		"transcript.excludeRegEx" = object@filter.transcript.excludeRegEx,
		"tier.names"              = paste(object@filter.tier.names, collapse=", "),
		"tier.includeRegEx"       = object@filter.tier.includeRegEx,
		"tier.excludeRegEx"       = object@filter.tier.excludeRegEx,
		"section.startsec"        = filter_startsec,
		"section.endsec"          = filter_endsec
	), width = w)

	.show_sep()
	.show_head("Concordance")
	.show_dl(c(
		"make"  = as.character(object@concordance.make),
		"width" = as.character(object@concordance.width)
	), width = w)

	.show_sep()
	.show_head("Cuts")
	.show_dl(c(
		"span.beforesec"         = as.character(object@cuts.span.beforesec),
		"span.aftersec"          = as.character(object@cuts.span.aftersec),
		"column.srt"             = object@cuts.column.srt,
		"column.printtranscript" = object@cuts.column.printtranscript,
		"printtranscripts"       = if(length(object@cuts.printtranscripts)==0) "[not created yet]" else "[check directly]",
		"cutlist.mac"            = if(length(object@cuts.cutlist.mac)==0) "[not created yet]" else "[check directly]",
		"cutlist.win"            = if(length(object@cuts.cutlist.win)==0) "[not created yet]" else "[check directly]"
	), width = w)

	.show_sep()
	search_run <- 'hit' %in% colnames(object@results)
	.show_head("Results")
	.show_dl(c(
		"results"              = if(!search_run) "[search not run yet]" else "[check directly]",
		"results.nr"           = if(!search_run) "[search not run yet]" else as.character(nrow(object@results)),
		"results.tiers.nr"     = if(!'tierName' %in% colnames(object@results)) "[search not run yet]" else as.character(length(unique(object@results$tierName))),
		"results.transcripts.nr" = if(!'transcriptName' %in% colnames(object@results)) "[search not run yet]" else as.character(length(unique(object@results$transcriptName))),
		"x.name"               = object@x.name
	), width = w)
}

methods::setMethod("show", signature = "search", definition = search_show)
