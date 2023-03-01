#' Run a search
#'
#' Runs a search, based on an existing search object s, in a corpus object x.
#' 
#' @param x Corpus object.
#' @param s Search object.
#'  
#' @return Search object.
#' 
#' @seealso \link{search_new}, \link{search_makefilter}, \link{search_sub} 
#' 
#' @export
#'
#' @example inst/examples/search_run.R
#' 
search_run <- function(x, s) {
	captured_x <- substitute(x)
	original_x <- x
	
	temp <- NULL
	start.time <- Sys.time()
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	
	#get transcripts and tiers to include
	#x <- examplecorpus
	#s <- mysearch
	#   necessary for creation of fulltexts including only some tiers
	#   necessary for searching only in some transcripts
	myfilter <- act::search_makefilter(x,
									 filterTranscriptNames        =s@filter.transcript.names, 
									 filterTranscriptIncludeRegEx =s@filter.transcript.includeRegEx , 
								 	 filterTranscriptExcludeRegEx =s@filter.transcript.excludeRegEx ,
									 filterTierNames              =s@filter.tier.names,	
									 filterTierIncludeRegEx       =s@filter.tier.includeRegEx,
									 filterTierExcludeRegEx       =s@filter.tier.excludeRegEx) 
	#View(myfilter)
	
	#=== normalization needed?
	if (s@search.normalized) {	
		x <- act::transcripts_update_normalization(x)
	}
		
	#===  update full texts if full text search
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime" | s@search.mode=="fulltext.byTier" )  {
		#if the fulltext really needs an update will be checked in the function
		# chekcing if 
		#	(A) transcript contents have been modified
		#	(B) another tier filter has been selected
		x <- act::transcripts_update_fulltexts(x, 
											   tierNames   = myfilter$tier.names) 
	}
	

	#=== Search
	helper_progress_set("Searching", length(myfilter$transcript.names))
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime" | s@search.mode=="fulltext.byTier" ) {
		temp 	  			<-	lapply(x@transcripts[myfilter$transcript.names], search_transcript_fulltext, s=s)
		temp	  			<-	do.call("rbind", temp)
		
	} else if (s@search.mode=="content" ) {
		temp 	  			<-	lapply(x@transcripts[myfilter$transcript.names], search_transcript_content, s=s)
		temp	  			<-	do.call("rbind", temp)
	} else {
		#=== some user error
		stop ("Unknow 'searchMode'. Please select 'fulltext', 'fulltext.byTime', 'fulltext.byTier' or 'content' .")
	}

	if(is.null(temp)) {
		myColNames <- c("transcript.name", "annotationID", "tier.name", "startSec","endSec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr" ,"hit.length", "hit.pos.fulltext", "hit.pos.content", "search.mode", "hit.span")
		temp <- data.frame(matrix(ncol = length(myColNames), nrow = 0), 
						   stringsAsFactors		= FALSE)
		colnames(temp) <- myColNames	
	}
	
	#=== keep only some columns
	keep <- c("transcript.name", "annotationID","tier.name","startSec","endSec","content","content.norm", "hit","hit.nr","hit.length", "hit.pos.content", "hit.pos.fulltext", "search.mode", "hit.span")
	temp <- temp[ , keep]
	s@results <- temp
	
	#=== make adaptations and concordance
	if (nrow(temp)==0) {
		s@results      <- 	cbind(resultID=as.character(), s@results)
	} else	{
		#=== add names for results
		resultID      <- 	helper_makeNamesForSearch(s@results, s@resultidprefix)
		s@results      <- 	cbind(resultID, s@results)
		
		#=== turn factors into strings
		fctr.cols <- sapply(s@results, is.factor)
		s@results[, fctr.cols] <- sapply(s@results[, fctr.cols], as.character)
		
		if (s@concordance.make)	{
			helper_progress_set("Concordance",max(1,nrow(s@results)))
			s	<- act::search_concordance(x, s, searchNormalized=s@search.normalized)
		}
	}
	#if corpus object has changed, assign to original corpus object
	if (getOption("act.updateX", TRUE)) {
		if (!identical(original_x,x)) {
			p <- parent.frame() 
			p[[deparse(captured_x)]] <- x
		}	
	}
	
	s@results.nr             <- nrow(temp)
	s@results.tiers.nr       <- length(unique(temp$tier.name))
	s@results.transcripts.nr <- length(unique(temp$transcript.name))
	s@x.name                 <- x@name
	return(s)
}


