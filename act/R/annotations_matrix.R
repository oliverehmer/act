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
#' In case that you want to select transcripts  by using regular expressions use the function \code{act::search_meta} first.
#'
#'
#' @param x Corpus object.
#' @param path_replacementMatrixCSV Character string; path to replacement matrix (a CSV file).
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#'
#' @return Corpus object.
#' @export
#'
#'@seealso \link{media_delete}, \link{media_getPathToExistingFile}
#'
#' @example inst/examples/annotations_matrix.R
#'  
annotations_matrix <- function(x, 
							   path_replacementMatrixCSV, 
							   filterTranscriptNames=NULL) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

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
	act_replacementMatrix <- matrix_load(path_replacementMatrixCSV)
	
	#=== check matrix
	if (is.null(act_replacementMatrix)) 						{	stop("Normalization matrix not read.")		}
	if ("search" %in% colnames(act_replacementMatrix)==FALSE)   {	stop("Column 'search' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")}
	if ("replace" %in% colnames(act_replacementMatrix)==FALSE)  {	stop("Column 'replace' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")	}
	
	#=== check 
	if (is.null(x@transcripts)) 	{	stop("No transcripts found in corpus object x.")	}
	
	#replace NA by empty string
	act_replacementMatrix$replace[is.na(act_replacementMatrix$replace)] <- ""
	
	#=== create named vector for replacement
	mymatrix 		<- as.character(act_replacementMatrix$replace)
	names(mymatrix) <- act_replacementMatrix$search
	#as.data.frame(mymatrix)
	
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
	if (is.null(out)) 						{	stop("Replacement matrix seems to be containing invalid regular expressions.")		}
	
	#=== do the replacement
	annotations_modified_nr <-0
	transcripts_modified_nr <-0
	transcripts_modified_ids <-c()
	annotations_modified_nr <- 0
	if (length(mymatrix)<1) {
		warning("Replacement matrix is empty.")
		
	} else {
		
		#set progress bar
		helper_progress_set("Processing",length(filterTranscriptNames))

		for (i in filterTranscriptNames) 		{
			#update progress bar
			helper_progress_tick()
			
			#towower
			x@transcripts[[i]]@annotations$content <- stringr::str_to_lower(x@transcripts[[i]]@annotations$content)   
				
			#replace
			annotations_modified_nr <-annotations_modified_nr+length(which(stringr::str_detect(x@transcripts[[i]]@annotations$content, "update.*_B")))
			if (annotations_modified_nr>0) {
				x@transcripts[[i]]@annotations$content <- stringr::str_replace_all(x@transcripts[[i]]@annotations$content, mymatrix)  
				transcripts_modified_nr <-transcripts_modified_nr+1
				transcripts_modified_ids <-c(transcripts_modified_ids, i)
			}
			x@transcripts[[i]]@modification.systime <- Sys.time()			
		}
	}
	
	x@history[[length(x@history)+1]] <- list(
		modification                ="annotations_matrix",
		systime                     = Sys.time(),
		transcripts.modified.count  =transcripts_modified_nr,
		transcripts.modified.ids    =transcripts_modified_ids,
		annotations.modified.count  =annotations_modified_nr
	)
	
	return (x)
}


