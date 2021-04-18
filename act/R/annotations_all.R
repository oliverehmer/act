#' All annotations in a corpus
#'
#' Merges annotations from all transcripts in a corpus and returns a data frame.
#' 
#' @param x Corpus object.
#'
#' @return Data.frame.
#' @export
#'
#' @examples
#' 
#' library(act)
#' 
#' #Get data frame with all annotations
#' allannotations <- act::annotations_all(examplecorpus)
#' 
#' #Have a look at the number of annotations
#' nrow(allannotations)
#' 
annotations_all <- function(x) {
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	temp <- data.frame()
	for (t in x@transcripts) {
		if (nrow(t@annotations)>0) {
			temp <- rbind(temp, cbind(transcript.name=rep(t@name,nrow(t@annotations)),  t@annotations))	
		}
	}
	return(temp)
}

