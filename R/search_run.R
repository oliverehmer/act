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
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	
	#==== FILTER ====
	#get transcripts and tiers to include
	# x <- examplecorpus
	# s <- mysearch
	#   necessary for creation of fulltexts including only some tiers
	#   necessary for searching only in some transcripts
	myfilter <- act::search_makefilter(x,
									 filterTranscriptNames        =s@filter.transcript.names, 
									 filterTranscriptIncludeRegex =s@filter.transcript.includeRegEx , 
								 	 filterTranscriptExcludeRegex =s@filter.transcript.excludeRegEx ,
									 filterTierNames              =s@filter.tier.names,	
									 filterTierIncludeRegex       =s@filter.tier.includeRegEx,
									 filterTierExcludeRegex       =s@filter.tier.excludeRegEx)
	#View(myfilter)
	s@filter.tier.names <- myfilter$tierNames

	#==== NORMALIZATION ====
	#if needed
	if (s@search.normalized) {	
		x <- act::transcripts_update_normalization(x)
	}
		
	#==== UPDATE full texts ====
	#if full text search
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime" | s@search.mode=="fulltext.byTier" )  {
		#if the fulltext really needs an update will be checked in the function
		# checking if 
		#	(A) transcript contents have been modified
		#	(B) another tier filter has been selected
		x <- act::transcripts_update_fulltexts(x, 
											   tierNames   = myfilter$tierNames) 
	}
	
	#==== . SEARCH ====
	helper_progress_set("Searching", length(myfilter$transcriptNames))
	if (s@search.mode=="fulltext" | s@search.mode=="fulltext.byTime" | s@search.mode=="fulltext.byTier" ) {
		temp 	  			<-	lapply(x@transcripts[myfilter$transcriptNames], search_transcript_fulltext, s=s)
		temp	  			<-	dplyr::bind_rows(temp)

	} else if (s@search.mode=="content" ) {
		temp 	  			<-	lapply(x@transcripts[myfilter$transcriptNames], search_transcript_content, s=s)
		temp	  			<-	dplyr::bind_rows(temp)
	} else {
		#=== some user error
		cli::cli_abort("Unknown {.arg searchMode}. Please select {.val fulltext}, {.val fulltext.byTime}, {.val fulltext.byTier} or {.val content}.")
	}
	#View(temp)

	#---- . check results
	#Check if there are results
	if(is.null(temp) || nrow(temp) == 0) {
		myColNames <- c("resultID", "transcriptName", "annotationID", "tierName", "startsec", "endsec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr", "hit.length", "hit.pos.fulltext", "hit.pos.content", "searchMode", "hit.span", "stills.values", "stills.folder")
		temp <- as.data.frame(stats::setNames(
			replicate(length(myColNames), logical(0), simplify = FALSE),
			myColNames
		))
		temp$stills.values  <- list()
	} else {
		#add columns for stills
		temp$stills.folder <- rep("stills", nrow(temp))
		temp$stills.values <- vector("list", nrow(temp))

		#reorder columns: standard, layers, search
		temp <- helper_order_annotations_columns(temp)
	}
	
	#---- . set return value
	s@results <- temp
	
	#=== make adaptations and concordance
	if (nrow(temp)==0) {
		s@results      <- 	cbind(resultID=as.character(), s@results)
	} else	{
		#=== add names for results
		resultID  <- 	.make_names_for_search(s@results, s@resultid.prefix, s@resultid.start)
		s@results <- 	cbind(resultID, s@results)

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
	s@results.tiers.nr       <- length(unique(temp$tierName))
	s@results.transcripts.nr <- length(unique(temp$transcriptName))
	s@x.name                 <- x@name
	return(s)
}


