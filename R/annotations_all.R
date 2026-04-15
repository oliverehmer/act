#' All annotations in a corpus
#'
#' Merges annotations from all transcripts in a corpus and returns a data frame.
#' 
#' @param x Corpus object.
#'
#' @return data frame
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
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	temp <- NULL
	for (t in x@transcripts) {
		if (nrow(t@annotations)>0) {
			ann <- cbind(transcriptName=rep(t@name, nrow(t@annotations)),  t@annotations)
			if (is.null(temp)) {
				temp <- ann
			} else {
				temp <- dplyr::bind_rows(temp, ann)
			}
		}
	}
	if (!is.null(temp)) {
		temp <- helper_order_annotations_columns(temp)
	}
	return(temp)
}
