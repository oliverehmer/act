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
	
	if (missing(x))                          {cli::cli_abort("Corpus object x is missing.") 	}
	if (missing(s))                          {cli::cli_abort("Search object s is missing.") 	}
	if (Sys.info()["sysname"]!="Darwin")     {cli::cli_abort("You need to be on a Mac to use this function") 	}
	
	i <- 1
	repeat {
		if (i> nrow(s@results)){
			break
		}
		cli::cli_rule("Result {i}")
		cli::cli_dl(c(
			"transcriptName" = s@results[i, ]$transcriptName,
			"tier.name"      = s@results[i, ]$tier.name,
			"startsec"       = as.character(s@results[i, ]$startsec),
			"endsec"         = as.character(s@results[i, ]$endsec),
			"content"        = s@results[i, ]$content
		))
		if ("printtranscript" %in% colnames(s@results)) {
			cli::cli_text("")
			cli::cli_text(s@results$printtranscript[i])
		}
		played <- act::search_openresult_inquicktime(x=x, 
													  s=s, 
													  resultid = i, 
													  play=TRUE, 
													  close = TRUE,
													  bringToFront=FALSE)
		if (played) {
			if (rstudioapi::isAvailable()) {
				rstudioapi::executeCommand("activateConsole")
			}
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



