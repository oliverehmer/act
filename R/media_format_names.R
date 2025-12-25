#' Format media names
#'
#' Format the names attribute of the media paths. 
#' Relevant for media cuts with \link{search_cuts}.
#' Names attribute can be used to differentiate multiple cuts with of the same media type (e.g. mp4 from different cameras).

#'
#' @param x Corpus object.
#' @param recreateNames Logical; if \code{TRUE} the names will be created again from the media file names , if \code{FALSE} the existing values in the names attributes will be used. 
#' @param pattern Character strings; Regular Expression as search pattern.
#' @param replacement Character strings; Replacement pattern.
#' @param conditionalPreserve Logical; if \code{TRUE} the names will be set to an empty string "" if there is only one media file of a kind (audio/video) , if \code{FALSE} all names will be preserved. 
#' @return Corpus object.
#' 
#' @seealso \link{media_assign}, \link{media_delete}, \link{search_cuts}, \
#' 
#' @export
#'
#' @example inst/examples/media_assign.R
#' 

media_format_names <- function(x, 
							   recreateNames = TRUE,
							   pattern, 
							   replacement           = NULL,
							   conditionalPreserve   = FALSE) {

#x<-corpus
#pattern<-'^\\p{L}+_\\p{L}*\\d+_*'	#without v2/a1
#replacement<-''
#recreateNames <- TRUE
#conditionalPreserve <- TRUE
#x@transcripts[[105]]@media.path

	#---- .the checks ----
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (!is.null(pattern)) {
		if (is.null(replacement)) {
			{stop("If 'pattern' is set, 'replacement' may not be missing.") 		}
		}
	}

	#---- .recreate names ----
	if (recreateNames) {
		for (t in x@transcripts) {
			names(t@media.path) <- stringr::str_to_lower(
				tools::file_path_sans_ext(basename(t@media.path))
			)
		}
	}

	#----- .search & replace ----
	if (!is.null(pattern)) {
		for (i in seq_along(x@transcripts)) {
			names(x@transcripts[[i]]@media.path) <- stringr::str_replace_all(    
																string      = names(x@transcripts[[i]]@media.path),
																pattern     = pattern,
																replacement = replacement
															)
		}
	}

	#---- .erase names in case of only one media file of the same type ----
	if (conditionalPreserve) {
		for (i in seq_along(x@transcripts)) {
			#get video files
			ids <- which(
				tools::file_ext(x@transcripts[[i]]@media.path) %in% options()$act.fileformats.video
			)
			#set name to '' if there is only one
			if (length(ids)==1) {
				names(x@transcripts[[i]]@media.path)[ids] <- ''
			}
			
			#get audio files
			ids <- which(
				tools::file_ext(x@transcripts[[i]]@media.path) %in% options()$act.fileformats.audio
			)
			#set name to '' if there is only one
			if (length(ids)==1) {
				names(x@transcripts[[i]]@media.path)[ids] <- ''
			}
		}
	}

	#---- .return corpus object ----
	return (x)
}

