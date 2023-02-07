#' Summarized information about corpus and transcript objects
#'
#' Gives summarized information about the contents of a corpus object or a transcript object that is passed as parameter to the function.
#' In the case that you want to pass a transcript object form a corpus object, make sure that you access the transcript using double [[]] brackets.
#' 
#' To get more detailed information about the tiers in a corpus object use \code{act::info}.
#' 
#' @param ... object; either  a corpus or a transcript object.
#'
#' @return List. 
#' 
#' @seealso \link{info}
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' act::info_summarized(examplecorpus)
#' 
#' act::info_summarized(examplecorpus@transcripts[[1]])
#' 
info_summarized <- function(...) {
	dots <- list(...)                
	if(length(dots) == 0) {stop ("You need to pass a corpus object or a transcript object to this function.")}
	x <- NULL
	t <- NULL
	
	if (methods::is(dots[[1]],"corpus")) {
		x <- dots[[1]]	
	} else if (methods::is(dots[[1]], "transcript" )) {
		t <- dots[[1]]	
	} else {
		stop ("You need to pass a corpus object or a transcript object to this function. ")
	}

	if (!is.null(x)) {
		#--- transcripts names
		transcript.names <- names(x@transcripts)
		transcript.count <- length(transcript.names)
		
		#--- length
		length <- lapply(x@transcripts, "slot", name = "length.sec")
		length.sec <- sum(unlist(length))
		length.formatted <- helper_format_time(length.sec, addTimeInSeconds = TRUE)
		
		#--- tiers
		tiers       <- lapply(x@transcripts, "slot", name = "tiers")
		temp        <- do.call("rbind", tiers)
		tier.names  <- unique(temp$name)
		tier.count  <- sum(unlist(lapply(tiers, nrow)))
		
		#---annotations 
		annotations <- lapply(x@transcripts, "slot", name = "annotations")
		annotations.count <- sum(unlist(lapply(annotations, nrow)))
		
		#--- words org
		content.org <- lapply(annotations,"[[", 5)
		words.org.count <- lapply(content.org, FUN=stringr::str_count, pattern=options()$act.wordCountRegEx)
		words.org.count <- sum(unlist(words.org.count))
		
		#--words norm
		content.norm <- lapply(annotations,"[[", 6)
		words.norm.count <- lapply(content.norm, FUN=stringr::str_count, pattern=options()$act.wordCountRegEx)
		words.norm.count <- sum(unlist(words.norm.count))
		
		info <- list(length.formatted  = length.formatted,
					 length.sec        = length.sec,
					 words.org.count   = words.org.count,
					 words.norm.count  = words.norm.count,
					 annotations.count = annotations.count,
					 tier.count        = tier.count,
					 tier.names        = tier.names,
					 transcript.count  = transcript.count,
					 transcript.names  = transcript.names
		)
	}
	if (!is.null(t)) {
		#--- tiers
		tier.names <- t@tiers$name
		tier.count <- nrow(t@tiers)
		
		#--- annotations
		annotations.count <- sum(nrow(t@annotations))
		
		#--- words org
		words.org.count <- lapply(t@annotations$content, FUN=stringr::str_count, pattern=options()$act.wordCountRegEx)
		words.org.count <- sum(unlist(words.org.count))
		
		#--- words norm
		words.norm.count <- lapply(t@annotations$content.norm, FUN=stringr::str_count, pattern=options()$act.wordCountRegEx)
		words.norm.count <- sum(unlist(words.norm.count))

		info <- list(length.formatted  = helper_format_time(t@length.sec , addTimeInSeconds = TRUE),
					 length.sec        = t@length.sec,
					 words.org.count   = words.org.count,
					 words.norm.count  = words.norm.count,					 
					 annotations.count = annotations.count,
					 tier.count        = tier.count,
					 tier.names        = tier.names
					 )
	}
	return(info)
}
