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
#' @slot resultidprefix Character string; search results will be numbered consecutively; This character string will be placed before the consecutive numbers.
#' @slot filter.transcript.names Vector of character strings; names of transcripts to include in the search. If the value is \code{character()} or \code{""} filter will be ignored.
#' @slot filter.transcript.includeRegEx  Character string; Regular expression that defines which transcripts should be INcluded in the search (matching the name of the transcript).
#' @slot filter.transcript.excludeRegEx  Character string; Regular expression that defines which transcripts should be EXcluded in the search (matching the name of the transcript).
#' @slot filter.tier.names Vector of character strings; names of tiers to include in the search. If the value is \code{character()} or \code{""} filter will be ignored.
#' @slot filter.tier.include Character string; Regular expression that defines which tiers should be INcluded in the search (matching the name of the tier).
#' @slot filter.tier.exclude Character string; Regular expression that defines which tiers should be EXcluded in the search (matching the name of the tier).
#' @slot filter.section.startsec Double; Time value in seconds, limiting the search to a certain time span in each transcript, defining the start of the search window.
#' @slot filter.section.endsec Double; Time value in seconds, limiting the search to a certain time span in each transcript, defining the end of the search window.
#' @slot concordance.make Logical; If a concordance should be created when the search is run.
#' @slot concordance.width Integer; number of characters to include in the concordance.
#' @slot cuts.span.beforesec Double; Seconds how much the cuts (media and print transcripts) should start before the start of the search hit.
#' @slot cuts.span.aftersec Double; Seconds how much the cuts (media and print transcripts) should end after the end of the search hit.
#' @slot cuts.column.srt Character string; name of  destination column in the search results data frame where the srt substitles will be inserted; column will be created if not present in data frame; set to "" for no insertion.
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
				  	resultidprefix="character",
				  	
				  	filter.transcript.names="character",
				  	filter.transcript.includeRegEx ="character",
				  	filter.transcript.excludeRegEx ="character",
				  	filter.tier.names="character",
				  	filter.tier.include="character",
				  	filter.tier.exclude="character",
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
				  	
				  ), prototype = list (
				  	name="mysearch",
				  	pattern="",
				  	search.mode="fulltext",
				  	search.normalized=TRUE,
				  	resultidprefix="resultID",
				  	
				  	filter.transcript.names=character(),
				  	filter.transcript.includeRegEx ="",
				  	filter.transcript.excludeRegEx ="",
				  	filter.tier.names=character(),
				  	filter.tier.include="",
				  	filter.tier.exclude="",
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
	#cat("search object", fill=TRUE)
	cat("  name                       : ", paste("'", object@name,"'",sep="", collapse=""), fill=TRUE)
	cat("  pattern                    : ", paste("'", object@pattern,"'",sep="", collapse=""), fill=TRUE)
	cat("  search.mode                : ", object@search.mode, fill=TRUE)
	cat("  search.normalized          : ", object@search.normalized, fill=TRUE)
	cat("  resultidprefix             : ", paste("'", object@resultidprefix,"'",sep="", collapse=""), fill=TRUE)
	cat("\n")
	
	cat("  filter.transcript.names    : ", paste("'", object@filter.transcript.names,"'",sep="", collapse=", "), fill=TRUE)
	cat("  filter.transcript.includeRegEx   : ", paste("'", object@filter.transcript.includeRegEx ,"'",sep="", collapse=""), fill=TRUE)
	cat("  filter.transcript.excludeRegEx   : ", paste("'", object@filter.transcript.excludeRegEx ,"'",sep="", collapse=""), fill=TRUE)
	cat("  filter.tier.names          : ", paste("'", object@filter.tier.names,"'",sep="", collapse=", "), fill=TRUE)
	cat("  filter.tier.include        : ", paste("'", object@filter.tier.include,"'",sep="", collapse=""), fill=TRUE)
	cat("  filter.tier.exclude        : ", paste("'", object@filter.tier.exclude,"'",sep="", collapse=""), fill=TRUE)
	cat("  filter.section.startsec    : ", if (length(object@filter.section.startsec)==0) {"[not set]"} else {if (is.na(object@filter.section.startsec)){"[not set]"} else {object@ffilter.section.startsec}}, fill=TRUE)
	cat("  filter.section.endsec      : ", if (length(object@filter.section.endsec)==0) {"[not set]"} else {if (is.na(object@filter.section.endsec)){"[not set]"} else {object@filter.section.endsec}}, fill=TRUE)
	cat("\n")
	
	cat("  concordance.make           : ", object@concordance.make, fill=TRUE)
	cat("  concordance.width          : ", object@concordance.width , fill=TRUE)
	cat("\n")
	
	cat("  cuts.span.beforesec        : ", object@cuts.span.beforesec , fill=TRUE)
	cat("  cuts.span.aftersec         : ", object@cuts.span.aftersec, fill=TRUE)
	cat("  cuts.column.srt            : ", paste("'", object@cuts.column.srt,"'",sep="", collapse=""), fill=TRUE)
	cat("  cuts.column.printtranscript: ", paste("'", object@cuts.column.printtranscript,"'",sep="", collapse=""), fill=TRUE)
	cat("  cuts.printtranscripts      : ", if(length(object@cuts.printtranscripts)==0) {"[not created yet]"} else {"[check directly]"}, fill=TRUE)
	cat("  cuts.cutlist.mac           : ", if(length(object@cuts.cutlist.mac)==0)      {"[not created yet]"} else {"[check directly]"}, fill=TRUE)
	cat("  cuts.cutlist.win           : ", if(length(object@cuts.cutlist.win)==0)      {"[not created yet]"} else {"[check directly]"}, fill=TRUE)
	cat("\n")
	
	cat("  results                    : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {"[check directly]"}, fill=TRUE)
	cat("  results.nr                 : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {nrow(object@results)}                       , fill=TRUE)
	cat("  results.tiers.nr           : ", if(!'tier.name' %in% colnames(object@results))       {"[search not run yet]"} else {length(unique(object@results$tier.name))}        , fill=TRUE)
	cat("  results.transcripts.nr     : ", if(!'transcript.name' %in% colnames(object@results)) {"[search not run yet]"} else {length(unique(object@results$transcript.name))}  , fill=TRUE)
	cat("  x.name                     : ", paste("'", object@x.name,"'",sep="", collapse=""), fill=TRUE)
	cat()
}

methods::setMethod("show", signature = "search", definition = search_show)
