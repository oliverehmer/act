#' Create a new search
#'
#' Creates a new search object and runs the search in a corpus object.
#' Only 'x' and 'pattern' are obligatory. 
#' The other arguments can be left to their default values.
#'
#' @param x Corpus object; basis in which will be searched.
#' @param pattern Character string; search pattern as regular expression.
#' @param searchMode Character string; takes the following values: \code{content}, \code{fulltext} (=default, includes both full text modes), \code{fulltext.byTime}, \code{fulltext.byTier}.
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#' @param name Character string; name of the search. Will be used, for example, as name of the sub folder when creating media cuts.
#' @param resultid.prefix Character string; search results will be numbered consecutively; This character string will be placed before the consecutive numbers.
#' @param resultid.start Integer; search results will be numbered consecutively; This is the start number of the identifiers.
#' @param filterTranscriptNames Vector of character strings; names of transcripts to be included. 
#' @param filterTranscriptIncludeRegEx Character string; as regular expression, limit search to certain transcripts matching the expression.
#' @param filterTranscriptExcludeRegEx Character string; as regular expression, exclude certain transcripts matching the expression.
#' @param filterTierNames Vector of character strings; names of tiers to be included. 
#' @param filterTierIncludeRegEx Character string; as regular expression, limit search to certain tiers matching the expression.
#' @param filterTierExcludeRegEx Character string; as regular expression, exclude certain tiers matching the expression.
#' @param filterSectionStartsec Double; start time of region for search.
#' @param filterSectionEndsec Double; end time of region for search. 
#' @param concordanceMake Logical; if \code{TRUE} concordance will be added to search results.
#' @param concordanceWidth Integer; number of characters  to the left and right of the search hit in the concordance , the default is \code{120}.
#' @param cutSpanBeforesec Double; Start the media and transcript cut some seconds before the hit to include some context, the default is \code{0}.
#' @param cutSpanAftersec Double; End the media and transcript cut some seconds before the hit to include some context, the default is \code{0}.
#' @param runSearch Logical; if \code{TRUE} search will be run in corpus object, if \code{FALSE} only the search object will be created.
#' 
#' @return Search object.
#' 
#' @seealso \link{search_run}, \link{search_makefilter}, \link{search_sub}
#' 
#' @export
#'
#' @example inst/examples/search_new.R
#' 
search_new <- function(x, 
					   pattern, 
					   searchMode                   = c("content", "fulltext", "fulltext.byTime", "fulltext.byTier"),
					   searchNormalized             = TRUE, 
					   name                         = "mysearch",  
					   resultid.prefix              = "result",
					   resultid.start               = 1,
					   filterTranscriptNames        = NULL,
					   filterTranscriptIncludeRegEx = NULL, 
					   filterTranscriptExcludeRegEx = NULL, 
					   filterTierNames              = NULL,
					   filterTierIncludeRegEx       = NULL, 
					   filterTierExcludeRegEx       = NULL, 
					   filterSectionStartsec        = NULL, 
					   filterSectionEndsec          = NULL,  
					   concordanceMake              = TRUE, 
					   concordanceWidth             = NULL,
					   cutSpanBeforesec             = 0,
					   cutSpanAftersec              = 0,
					   runSearch                    = TRUE) {
	
	#=== capture original x
	captured_x <- substitute(x)
	original_x <- x
	
	start.time <- Sys.time()

	#=== check x object
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(pattern))			{stop("Pattern is missing.") 	}
	if (is.null(x@transcripts)) 	{stop("No transcripts found in corpus object x.")	}

	#=== check arguments
	searchMode <- match.arg(searchMode)

	#=== create search object
	s <- methods::new("search")
	s@name          					<- name
	s@pattern                   		<- pattern
	s@search.mode               		<- searchMode
	s@search.normalized         		<- searchNormalized
	s@resultid.prefix            		<- resultid.prefix
	s@resultid.start            		<- resultid.start
	
	s@filter.transcript.names   		<- if(!is.null(filterTranscriptNames))  		{filterTranscriptNames}     	else {s@filter.transcript.names}
	s@filter.transcript.includeRegEx	<- if(!is.null(filterTranscriptIncludeRegEx))   {filterTranscriptIncludeRegEx}  else {s@filter.transcript.includeRegEx }
	s@filter.transcript.excludeRegEx	<- if(!is.null(filterTranscriptExcludeRegEx))   {filterTranscriptExcludeRegEx}  else {s@filter.transcript.excludeRegEx }
	
	s@filter.tier.names         		<- if(!is.null(filterTierNames))        		{filterTierNames}           	else {s@filter.tier.names}
	s@filter.tier.includeRegEx       	<- if(!is.null(filterTierIncludeRegEx)) 		{filterTierIncludeRegEx}        else {s@filter.tier.includeRegEx}
	s@filter.tier.excludeRegEx       	<- if(!is.null(filterTierExcludeRegEx))         {filterTierExcludeRegEx}        else {s@filter.tier.excludeRegEx}
	
	s@filter.section.startsec   		<- if(!is.null(filterSectionStartsec))  		{filterSectionStartsec}     	else {s@filter.section.startsec}
	s@filter.section.endsec     		<- if(!is.null(filterSectionEndsec))    		{filterSectionEndsec}       	else {s@filter.section.endsec}
	
	s@concordance.make          		<- concordanceMake
	s@concordance.width         		<- if(!is.null(concordanceWidth))   {concordanceWidth}   else {s@concordance.width}
	s@cuts.span.beforesec       		<- as.double(cutSpanBeforesec)
	s@cuts.span.aftersec        		<- as.double(cutSpanAftersec)	
	
	s@x.name                    		<- x@name

	#=== run the search
	if (runSearch) {
		s <- act::search_run(x=x, s=s)
	}

	#if corpus object has changed, assign to original corpus object
	if (getOption("act.updateX", TRUE)) {
		if (!identical(original_x,x)) {
			p <- parent.frame() 
			p[[deparse(captured_x)]] <- x
		}	
	}
	
	#=== return the results
	return(s)
}
	