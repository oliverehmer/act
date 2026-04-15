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
#' @param folderOutput Character string; if parameter is not set, the print transcripts will only be inserted in \code{s@results}; if the path to a existing folder is given transcripts will be saved in '.txt' format.

#'
#' @return Search object; 
#' @export
#'
#' @example inst/examples/search_cuts.R
#' 
search_cuts <- function(x, 
						s,
						cutSpanBeforesec = NULL,
						cutSpanAftersec  = NULL,
						l                = NULL, 
						folderOutput     = NULL) {
	#x <- corpus
	#s <- mysearch
	
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	if (missing(s)) 	{cli::cli_abort("Search object in parameter {.arg s} is missing.") 		}	else { if (!methods::is(s, "search")	)	{cli::cli_abort("Parameter {.arg s} needs to be a {.cls search} object.") 	} }
	
	if (is.null(s@results$transcriptName)) 		{ cli::cli_abort("Data frame s@results does not contain column {.arg transcriptName}") 	}

	s <- act::search_cuts_printtranscript(x=x,
										  s=s,
										  cutSpanBeforesec=cutSpanBeforesec,
										  cutSpanAftersec=cutSpanAftersec,
										  l=l,
										  folderOutput=folderOutput)
	
	s <- act::search_cuts_media(x=x,
								s=s,
								cutSpanBeforesec=cutSpanBeforesec,
								cutSpanAftersec=cutSpanAftersec,
								folderOutput=folderOutput)

	s <- act::search_cuts_srt(x=x,
							  s=s,
							  cutSpanBeforesec=cutSpanBeforesec,
							  cutSpanAftersec=cutSpanAftersec,
							  folderOutput=folderOutput)
	
	#=== give modified results back
	return(s)
}