#' All tiers in a corpus
#'
#' Merges tiers from all transcripts in a corpus and returns a data frame.
#' 
#' @param x Corpus object.
#' @param compact Logical; if \code{TRUE} a condensed overview will be returned,  if \code{FALSE} a detailed overview will be returned.
#'
#' @return Data frame
#' 
#' @export
#'
#' @examples
#' 
#' library(act)
#' 
#' #Get data frame with all tiers
#' alltiers <- act::tiers_all(examplecorpus)
#' alltiers
#' 
#' #Get data frame with a simplified version
#' alltiers <- act::tiers_all(examplecorpus, compact=TRUE)
#' alltiers
#' 
tiers_all <- function(x, compact=TRUE) {
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	#=== base data
	tiers <- data.frame()
	for (t in x@transcripts) {
		if (nrow(t@tiers)>0) {
			tiers <- rbind(tiers, cbind(transcript.name=rep(t@name,nrow(t@tiers)),  t@tiers))	
		}
	}
	
	#--- Count annotations in tiers
	if (nrow(tiers)>0) {
		#each tier
		i <- 1
		for (i in 1:nrow(tiers)) {
			
			#--- count annotations
			ids <- which(x@transcripts[[tiers$transcript.name[i]]]@annotations$tier.name==tiers$name[i])
			tiers$annotations.count[i]<- length(ids)
			
			#--- get annotations
			annotations <- x@transcripts[[tiers$transcript.name[i]]]@annotations[ids,]
			
			#--- words org
			content.org <- annotations$content
			words.org.count <- lapply(content.org, FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
			tiers$words.org.count[i] <- sum(unlist(words.org.count))
			
			#--- words norm
			content.norm <- annotations$content.norm
			words.norm.count <- lapply(content.norm, FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
			tiers$words.norm.count[i] <- sum(unlist(words.norm.count))
		}
	}
	return(tiers)
}



