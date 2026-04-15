#' Delete media files links from transcript objects 
#'
#' @param x Corpus object.
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#'
#' @return Corpus object.
#' @export
#'
#' @seealso \link{media_assign}, \link{media_path_to_existing_file}
#'
#' @examples
#' library(act)
#' 
#' examplecorpus <- act::media_delete(examplecorpus)
#' 
media_delete <- function(x, transcriptNames=NULL) {
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	
	#  if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}
	
	#--- run through all transcripts in the corpus file
	for (i in transcriptNames) {
			x@transcripts[[i]]@media.path <- character()
	}
	return (x)
}


