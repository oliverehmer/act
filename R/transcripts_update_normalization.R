#' Normalize transcriptions
#'
#' Normalizes the contents of transcriptions in a corpus object using a normalization matrix.
#' Function returns a corpus object with normalized transcription and updates the original corpus object passed as argument to x.
#'
#' @param x Corpus object.
#' @param path_replacementMatrixCSV Character string; path to replacement matrix in CSV format. If empty, the default replacement matrix that comes with the package will be used.
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#' @param forceUpdate Logical; If \code{TRUE} transcripts will be normalized in any case, if \code{FALSE} transcripts will be only normalized if there was a modification to the transcript since the last normalization.
#'
#' @export
#'
#' @examples
#' library(act)
#' 
#' examplecorpus <- act::transcripts_update_normalization(x=examplecorpus)
#' 
transcripts_update_normalization <- function(x, 
											 path_replacementMatrixCSV="", 
											 transcriptNames=NULL, 
											 forceUpdate=FALSE){
	#=== check data
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (is.null(x@transcripts))     {
		warning("No transcripts found in corpus object x.")	
		return(x)
	}
	if (length(x@transcripts)==0) 	{
		warning("No transcripts found in corpus object x.")	
		return(x)
	}
	
	#=== get the matrix
	if (is.null(x@normalization.matrix)) {	
		stop("The corpus object does not contain a normalization matrix in '@normalization.matrix'. To set the normalization matrix use 'x@normalization.matrix <- act::matrix_load(path=...)'")		
	}
	act_replacementMatrix <- x@normalization.matrix
	
	#=== check matrix
	if ("search" %in% colnames(act_replacementMatrix)==FALSE) {	stop("Column 'search' is missing in normalization matrix. The matrix needs to contain colums 'search' and 'replace'")}
	if ("replace" %in% colnames(act_replacementMatrix)==FALSE){	stop("Column 'replace' is missing in normalization matrix. The matrix needs to contain colums 'search' and 'replace'")	}
	#replace NA by empty strings
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
	if (is.null(out)) 						{	stop("Normalization matrix seems to be containing invalid regular expressions.")		}
	
	#=== if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}
	
	#=== check how many transcripts need an update
	transcriptNames.update <- c()
	for (i in transcriptNames) {
		#=== check if update is necessary
		updateThis <- FALSE
		if (forceUpdate) {
			updateThis <- TRUE			
		} else {
			if (length(x@transcripts[[i]]@normalization.systime)==0) {
				#normalization has never been done
				updateThis <- TRUE
			} else {
				#normalization has been done once
				if (length(x@transcripts[[i]]@modification.systime)==0) {
					#without any modification there is no need to update
				} else {
					#if transcript has been modified since last update
					if (x@transcripts[[i]]@modification.systime > x@transcripts[[i]]@normalization.systime) {
						updateThis <- TRUE
					} else {
						
					}
				}
			}
		}
		if (updateThis) {
			transcriptNames.update <- c(transcriptNames.update, i)
		}
	}
	
	#=== do the replacement
	if (length(mymatrix)<1) {
		warning("Replacement matrix is empty.")
	} else {
		
	}
	if (length(transcriptNames.update)) {
		#set progress bar
		helper_progress_set("Updating normalization", length(transcriptNames.update))
		for (i in transcriptNames.update) {
			#update progress bar
			helper_progress_tick()
			x@transcripts[[i]]@modification.systime   <- Sys.time()
			x@transcripts[[i]]@normalization.systime <- Sys.time()
			
			if (length(mymatrix)<1) {
				#towower
				x@transcripts[[i]]@annotations$content.norm <- stringr::str_to_lower(x@transcripts[[i]]@annotations$content)   
				#trim
				x@transcripts[[i]]@annotations$content.norm <- stringr::str_trim(x@transcripts[[i]]@annotations$content.norm, side="both")
			} else {
				#towower
				x@transcripts[[i]]@annotations$content.norm <- stringr::str_to_lower(x@transcripts[[i]]@annotations$content)  
				#replace
				x@transcripts[[i]]@annotations$content.norm <- stringr::str_replace_all(x@transcripts[[i]]@annotations$content.norm, mymatrix)  
				#trim
				x@transcripts[[i]]@annotations$content.norm <- stringr::str_trim(x@transcripts[[i]]@annotations$content.norm, side="both")
			}
		}
	}
	
	return(x)
}
