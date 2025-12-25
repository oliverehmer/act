#' Open all search results in 'Quicktime' and play them
#' 
#' The function remote controls 'Quicktime' by using an Apple Script. 
#' It opens consecutively all search results in 'Quicktime' and plays them.
#' 
#' Note: You need to be on a Mac to use this function.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param bringToFront Logical; if \code{TRUE} the Quicktime player will be activated and placed before all other windows.
#' 
#' @return No return value.
#' @export
#'
#' @example inst/examples/search_playresults_inquicktime.R
#' 
search_playresults_inquicktime <- function(x, 
										  s, 
										  bringToFront=FALSE) {
	
	if (missing(x))                          {stop("Corpus object x is missing.") 	}
	if (missing(s))                          {stop("Search object s is missing.") 	}
	if (Sys.info()["sysname"]!="Darwin")     {stop("You need to be on a Mac to use this function") 	}
	
	i <- 1
	repeat {
		if (i> nrow(s@results)){
			break
		}
		cat("==== Result: ", i, "\n")
		cat("transcriptName: ", s@results[i, ]$transcriptName, "\n")
		cat("tierName      : ", s@results[i, ]$tierName, "\n")	
		cat("startsec       : ", s@results[i, ]$startsec, "\n")
		cat("endsec         : ", s@results[i, ]$endsec, "\n")
		cat("content        : ", s@results[i, ]$content, "\n")
		if ("printtranscript" %in% colnames(s@results)) {
			cat("\n")	
			cat(s@results$printtranscript[i])
			cat("\n")	
		}
		played <- act::search_openresult_inquicktime(x=x, 
													  s=s, 
													  resultid = i, 
													  play=TRUE, 
													  close = TRUE,
													  bringToFront=FALSE)
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



