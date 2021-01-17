#' Gets the path of a media file for a transcript
#'
#' @param t transcript object; transcript for which you want to get the media path.
#' @param filterFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is video > uncompressed audio > compressed audio.
#' 
#' @return Character string; path to a media file, or \code{NULL} if no existing media file has been found.
#' @export
#' 
#' @seealso \link{media_assign}, \link{media_delete}
#'
#' @example inst/examples/media_getPathToExistingFile.R
#' 
media_getPathToExistingFile <- function(t, 
										filterFile=c('.*\\.(mp4|mov)', '.*\\.(aiff|aif|wav)', '.*\\.mp3')) {
	
	if (missing(t)) 		{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	if (typeof(t)=="list")	{stop("Your transcript is of the wrong type (it is a list). Probably you have used single square brackets to access the transcript in your corpus object (corpus@transcripts[...]). Please use double square brackets (corpus@transcripts[[...]])")}
	for (i in 1:length(filterFile)) {
		hits <- stringr::str_detect(t@media.path, filterFile[i])
		# if any of the media files matches to the filterFile pattern
		if (any(hits==TRUE)) {
			hits_paths <- t@media.path[hits]
			#run through all files and check if it exists
			for (j in 1:length(hits_paths)) {
				if(file.exists(hits_paths[j])) {
					return(hits_paths[j])
				}
			}
		}
	}
	return (NULL)
}
