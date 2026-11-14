#' Inserts search results as annotations in tiers
#' 
#' The function will insert the results of a search as annotations into a specified destination tier. 
#' Results from different tiers will all be inserted into the same destination tier.
#' It can be specified how overlapping search results or search results from the same annotation will be handled.
#' Please note that hits with exactly the same start and end time will always be merged (e.g. they are not treated as overlapping).
#' 
#' @param x Corpus object.
#' @param s Search object.
#' @param destTier Character string; name of the tier to which the hit should be copied (if no copying is intended set to NA).
#' @param destTierAddMissing Logical; if \code{TRUE} the destination tier will be added if missing in the transcript object, if \code{FALSE} an error will be raised if the destination tier is missing.
#' @param contentFromColname Character string; names of the search results column from which the content of the new annotation shall be copied. Chose for example "resultID", “hit” or “content” or the name of any column that you have added to the results data frame. 
#' @param interruptOverlap Integer; How to proceed in case of overlapping search results: TRUE=insertion will be interrupted, FALSE=overlapping annotations will be merged.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included from search results. 
#' @param filterTierNames Character string; names of the tiers to be included form search results.
#' @param collapseString Character string; will be used to collapse multiple search results into one string.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/annotations_replace_copy.R
#' 
annotations_insert_from_search_to_tier <- function (x,
													s,
													destTier              = "destinationTier", 
													destTierAddMissing  = TRUE,
													contentFromColname    = "hit",
													interruptOverlap    = FALSE,  
													filterTranscriptNames = NULL, 
													filterTierNames       = NULL, 
													collapseString        = " | ") {
	#	x <- corpus
	#	s <- s
	#	destTier              <- "destinationTier" 
	#	destTierAddMissing  <- TRUE
	#	contentFromColname    <- "hit"
	#	interruptOverlap    <- FALSE 
	#	filterTranscriptNames <- NULL 
	#	filterTierNames       <- NULL 
	#	collapseString        <- " | "
	#View(s@results)
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	
	transcripts_modified_ids      <- c()
	annotations_inserted_nr       <- 0
	annotations_inserted_total_nr <- 0
	recodsets_insertederror_destinationTiermissingintranscript_nr <- 0
	recodsets_insertederror_destinationTiermissingintranscript_ids <- c()
	
	#make sure that the destination tier is a string	
	if (is.null(destTier) || length(destTier)!=1 || is.na(destTier)) {
		cli::cli_abort("{.arg destTier} has to name the tier the annotations are inserted into.")
	}
	destTier <- as.character(destTier)
	if (destTier=="") {
		destTier<-"destinationTier"
	}
	
	#=== get the transcript names
	#if none are given, take all names from filter or transcript object
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	#intersect with transcript names from search results
	filterTranscriptNames <- intersect(unique(s@results$transcriptName), filterTranscriptNames)
	
	if (length(filterTranscriptNames)==0) {
		cli::cli_abort("Wrong {.arg filterTranscriptNames}: Possibly modify parameter {.arg filterTranscriptNames}.")
	}
		
	#=== Add the destination tier if it is missing and should be added
	if (destTierAddMissing) {
		# only in the transcripts that get annotations
		x <- act::tiers_add(x, tierName = destTier, filterTranscriptNames = filterTranscriptNames)
	} else {
		tier_missing <- filterTranscriptNames[!vapply(filterTranscriptNames, function(t) destTier %in% x@transcripts[[t]]@tiers$name, logical(1))]
		if (length(tier_missing)>0) {
			cli::cli_abort("The tier {.val {destTier}} is missing in: {.val {tier_missing}}. Add it, or set {.arg destTierAddMissing} to TRUE.")
		}
	}
	
	#=== prepare results
	r <- s@results
	#filter only selected transcripts
	r <- r[r$transcriptName %in% filterTranscriptNames,]
	#filter only selected tiers
	if (!is.null(filterTierNames)) {
		r <- r[r$tierName %in% filterTierNames,]
	}
	#sort results
	r <- r[order(r$transcriptName ,r$startsec, r$endsec), ]
	#add column to mark which results will be removed
	r$remove <- rep(FALSE, nrow(r))
	#add content column
	r$mytext <- r[,contentFromColname]
	
	#run through all transcripts
	overlap <- FALSE
	filterTranscriptNames <- unique(r$transcriptName)
	
	for (t in filterTranscriptNames) {
		# The results are sorted by start: a result that starts before the end of the one before
		# joins it (one annotation from the first start to the last end). Results that only touch
		# are two annotations, and no overlap.
		current <- NA
		for (j in which(r$transcriptName==t)) {
			if (is.na(current) || r$startsec[j] >= r$endsec[current]) {
				current <- j
				next
			}
			if (!(r$startsec[current]==r$startsec[j] & r$endsec[current]==r$endsec[j])) {
				overlap <- TRUE
			}
			r$mytext[current] <- paste0(r$mytext[current], collapseString, r$mytext[j])
			r$endsec[current] <- max(r$endsec[current], r$endsec[j])
			r$remove[j]       <- TRUE
		}
	}
	#View(r)
	
	#--- Stop if overlap
	if (interruptOverlap & overlap) {
		cli::cli_abort("Overlap in results detected.")
	}
	
	#--- remove results that have been merged
	r<-r[r$remove==FALSE,]
	#View(r)
	
	#--- add results as annotations to transcript
	for (t in filterTranscriptNames) {
		resultids <- which(r$transcriptName==t)
		#resultids <- resultids[1:(length(resultids)-1)]
		resultids <- resultids[1:(length(resultids))]
		
		for (i in resultids)  {
			#create new record set
			if (nrow(x@transcripts[[t]]@annotations)>0) {
				annID <- max(x@transcripts[[t]]@annotations$annotationID)+1
			} else {
				annID <- 1
			}

			myrow <- data.frame(
				annotationID            = as.integer(annID),
				tierName                = as.character(destTier),
				startsec                = as.double(r$startsec[i]),
				endsec                  = as.double(r$endsec[i]),
				content                 = as.character(r$mytext[i]),
				content.norm            = as.character(""),
				char.orig.bytime.start  = NA_integer_,
				char.orig.bytime.end    = NA_integer_,
				char.norm.bytime.start  = NA_integer_,
				char.norm.bytime.end    = NA_integer_,
				char.orig.bytier.start  = NA_integer_,
				char.orig.bytier.end    = NA_integer_,
				char.norm.bytier.start  = NA_integer_,
				char.norm.bytier.end    = NA_integer_,
				stringsAsFactors        = FALSE
			)

			extra_cols <- setdiff(names(x@transcripts[[t]]@annotations), names(myrow))
			for (col in extra_cols) {
				myrow[[col]] <- NA
			}
			myrow <- myrow[, names(x@transcripts[[t]]@annotations), drop = FALSE]

			#add to table
			x@transcripts[[t]]@annotations <- rbind(x@transcripts[[t]]@annotations, myrow)
		}
		x@transcripts[[t]]@annotations <- x@transcripts[[t]]@annotations[
			order(x@transcripts[[t]]@annotations$startsec, x@transcripts[[t]]@annotations$tierName),
			]
	}
	
	#View(x@transcripts[[filterTranscriptNames[1]]]@annotations)

	#update info for transcript
	#	if (anyChanges) {
	#		#HISTORY transcript
	#		x@transcripts[[i]]@modification.systime <- Sys.time()
	#		x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
	#			modification               = "annotations_search_replace_copy",
	#			systime                    = Sys.time(),
	#			annotations.copied.count   = annotations_inserted_nr
	#		)
	#		#increase counters for corpus object
	#		transcripts_modified_ids      <- c(transcripts_modified_ids, i)
	#		annotations_inserted_total_nr   <- annotations_inserted_total_nr + annotations_inserted_nr
	#	}
	
	#	#HISTORY corpus
	#	x@history[[length(x@history)+1]] <- list( modification                     = "annotations_search_replace_copy",
	#											  systime                          = Sys.time(),
	#											  pattern                          = pattern,
	#											  replacement                      = replacement,
	#											  destTier                         = destTier,
	#											  destTierAddMissing             = destTierAddMissing,
	#											  transcripts.modified.count       = length(transcripts_modified_ids),
	#											  transcripts.modified.ids         = transcripts_modified_ids,
	#											  annotations.copied.total.count   = annotations_inserted_total_nr)
	#	if (recodsets_insertederror_destinationTiermissingintranscript_nr>0) {
	#		x@history[[length(x@history)+1]] <-  list( 
	#			modification                                        = "annotations_search_replace_copy",
	#			systime                                             = Sys.time(),
	#			recodsets.copiederror                               = "ERROR: the destination tier for copying was missing in some transcripts. No data copied.",
	#			recodsets.copiederror.tiermissingintranscript.count = recodsets_insertederror_destinationTiermissingintranscript_nr,
	#			recodsets.copiederror.tiermissingintranscript.ids   = recodsets_insertederror_destinationTiermissingintranscript_ids
	#		)
	#		
	#	}
	
	return (x)
}
