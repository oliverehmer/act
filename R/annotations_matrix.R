#' Search and replace contents of annotations using a matrix
#'
#' This functions performs a search and replace in the contents of an annotation. 
#' A simple matrix consisting of two columns will be used. 
#' The first column of the matrix needs to contain the search string, the second column  the replacement string.
#' The matrix needs to be in CSV format.
#' 
#' @seealso [matrix_load()] for loading the matrix 
#' and [matrix_save()] for saving the matrix to a CSV file.
#'
#' If only certain transcripts or tiers should be affected set the parameter \code{filterTranscriptNames}.
#' In case that you want to select transcripts  by using regular expressions use the function \code{act::search_makefilter} first.
#'
#'
#' @param x Corpus object.
#' @param pathReplacementMatrix Character string; path to replacement matrix (a CSV file).
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#'
#' @return Corpus object.
#' @export
#'
#'@seealso \link{media_delete}, \link{media_path_to_existing_file}
#'
#' @example inst/examples/annotations_matrix.R
#'  
annotations_matrix <- function(x, 
							   pathReplacementMatrix, 
							   filterTranscriptNames=NULL) {
	
	.assert_corpus(x, missing = missing(x))
	
	#=== get the transcript names
	#if none are given, take all names
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	
	#=== load the matrix
	act_replacementMatrix <- matrix_load(pathReplacementMatrix)
	if (is.null(act_replacementMatrix)) 						{	cli::cli_abort("Normalization matrix not read.")		}

	#=== check 
	if (is.null(x@transcripts)) 	{	cli::cli_abort("No transcripts found in corpus object {.arg x}.")	}
	
	#replace NA by empty string
	act_replacementMatrix$replace[is.na(act_replacementMatrix$replace)] <- ""
	
	#=== create named vector for replacement
	mymatrix 		<- as.character(act_replacementMatrix$replace)
	names(mymatrix) <- act_replacementMatrix$search
	
	#=== check if the matrix works
	out <- tryCatch(
		{
			#This is the 'try' part
			stringr::str_replace_all("test string", mymatrix)
		},
		error=function(cond) {
			#this is the error part
			NULL
		}
	)
	if (is.null(out)) 						{	cli::cli_abort("Replacement matrix seems to be containing invalid regular expressions.")		}
	
	#=== do the replacement
	annotations_modified_nr  <- 0
	transcripts_modified_ids <- c()
	
	if (length(mymatrix)<1) {
		cli::cli_warn("Replacement matrix is empty.")
	} else {
		
		#set progress bar
		helper_progress_set("Processing",length(filterTranscriptNames))

		for (i in filterTranscriptNames) 		{
			#update progress bar
			helper_progress_tick()

			#tolower for content.norm
			x@transcripts[[i]]@annotations$content.norm <- stringr::str_to_lower(x@transcripts[[i]]@annotations$content)

			#replace
			content_before <- x@transcripts[[i]]@annotations$content.norm
			x@transcripts[[i]]@annotations$content.norm <- stringr::str_replace_all(content_before, mymatrix)
			annotations_modified_count <- sum(content_before != x@transcripts[[i]]@annotations$content.norm)
			if (annotations_modified_count > 0) {
				annotations_modified_nr <- annotations_modified_nr + annotations_modified_count

				#HISTORY transcript
				x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list(
					modification               = "annotations_matrix",
					systime                    = Sys.time(),
					annotations.modified.count = annotations_modified_count
				)
				#increase counters for corpus object
				transcripts_modified_ids               <- c(transcripts_modified_ids, i)
			}
		}
	}
	
	#HISTORY corpus
	x@history[[length(x@history)+1]] <- list(
		modification                = "annotations_matrix",
		systime                     = Sys.time(),
		transcripts.modified.count  = length(transcripts_modified_ids),
		transcripts.modified.ids    = transcripts_modified_ids,
		annotations.modified.count  = annotations_modified_nr
	)
	
	return (x)
}


