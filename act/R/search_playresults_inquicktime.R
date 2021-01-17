#' Open all search results in 'Quicktime' and play them
#' 
#' The function remote controls 'Quicktime' by using an Apple Script. 
#' It opens consecutively all search results in 'Quicktime' and plays them.
#' 
#' Note: You need to be on a Mac to use this function.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param bringQuicktimeToFront Logical; if \code{TRUE} the Quicktime player will be activated and placed before all other windows.
#' 
#' @return No return value.
#' @export
#'
#' @example inst/examples/search_playresults_inquicktime.R
#' 
search_playresults_inquicktime <- function(x, 
										  s, 
										  bringQuicktimeToFront=FALSE) {
	
	if (missing(x))                          {stop("Corpus object x is missing.") 	}
	if (missing(s))                          {stop("Search object s is missing.") 	}
	if (Sys.info()["sysname"]!="Darwin")     {stop("You need to be on a Mac to use this function") 	}
	
	i <- 1
	repeat {
		if (i> nrow(s@results)){
			break
		}
		cat("==== Result: ", i, "\n")
		cat("transcript.name: ", s@results[i, ]$transcript.name, "\n")
		cat("tier.name      : ", s@results[i, ]$tier.name, "\n")	
		cat("startSec       : ", s@results[i, ]$startSec, "\n")
		cat("endSec         : ", s@results[i, ]$endSec, "\n")
		cat("content        : ", s@results[i, ]$content, "\n")
		if ("printtranscript" %in% colnames(s@results)) {
			cat("\n")	
			cat(s@results$printtranscript)
			cat("\n")	
		}
		played <- act::search_openresult_inquicktime(x=x, 
													  s=s, 
													  resultNr = i, 
													  play=TRUE, 
													  closeAfterPlaying = TRUE,
													  bringQuicktimeToFront=FALSE)
		if (played) {
			key <- readline(prompt="Press: r=repeat, escape=stop, return=continue: ")
			if (key=="r") {
			} else { 
				if(key=="x") {
					break
				} else { 
					i <- i+1
				}
			}		
		} else {
			i <- i+1
		}
	}
}



