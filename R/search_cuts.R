#' Create print transcripts, media cutlists and srt subtitles for all search results
#'
#' This function will call the following functions:
#' - \code{act_cuts_printtranscript} to create print transcripts,
#' - \code{act::cuts_media} to create FFmpeg cutlist to make media snippets,
#' - \code{act::search_cuts_srt()} to create .srt subtitles,
#' for all  search results.
#'  
#' For a detailed description including examples please refer to the documentation of the indidival functions.
#' They also offer some more parameters than this functions. If you want to use those, call the functions individually.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param l Layout object.
#' @param outputFolder Character string; if parameter is not set, the print transcripts will only be inserted in \code{s@results}; if the path to a existing folder is given transcripts will be saved in '.txt' format.
#'
#' @return Search object; 
#' @export
#'
#' @example inst/examples/search_cuts.R
#' 
search_cuts <- function(x, 
						s,
						cutSpanBeforesec = NULL,
						cutSpanAftersec = NULL,
						l=NULL, 
						outputFolder=NULL ) {
	#x <- corpus
	#s <- mysearch
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	
	if (is.null(s@results$transcript.name)) 		{ stop("Data frame s@results does not contain column 'transcript.name'") 	}

	s <- act::search_cuts_printtranscript(x=x,
										  s=s,
										  cutSpanBeforesec=cutSpanBeforesec,
										  cutSpanAftersec=cutSpanAftersec,
										  l=l,
										  outputFolder=outputFolder)
	
	s <- act::search_cuts_media(x=x,
								s=s,
								cutSpanBeforesec=cutSpanBeforesec,
								cutSpanAftersec=cutSpanAftersec,
								outputFolder=outputFolder)

	s <- act::search_cuts_srt(x=x,
							  s=s,
							  cutSpanBeforesec=cutSpanBeforesec,
							  cutSpanAftersec=cutSpanAftersec,
							  outputFolder=outputFolder)
	
	#=== give modified results back
	return(s)
}