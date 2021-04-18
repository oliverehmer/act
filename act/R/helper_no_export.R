# Make names for search results
#
# @param mySearchResults Data frame; data frame containing search results.
# @param resultidprefix Character string; prefix for the name of the consecutively numbered search results.
# 
# @return Vector of character strings; names created for the search results.
# @export
#
# @examples
# library(act)

# # Search 
# myRegEx <- "yo"
# searchresults <- act::search_corpus(examplecorpus, pattern=myRegEx, concordanceMake=FALSE)
# 
# # Make custom names
# mynames <- act::search_names(searchresults, resultidprefix="yo")
# 
# # Replace old names in search by new names
# searchresults$resultID <- mynames
helper_makeNamesForSearch <- function(mySearchResults, 
									  resultidprefix="result") {
	
	myFormat <- paste(resultidprefix, "%0", nchar(toString(nrow(mySearchResults))), "d", sep="")
	myNames <- sprintf(myFormat, 1:nrow(mySearchResults))
	return (myNames)
}


# Gets the path of a .TextGrid for a transcript
#
# Returns either the path to the original .TextGrid file or to a temporary TextGrid created on the fly.
# 
# @param t transcript object; transcript for which you want to get the TextGrid
#
# @return Character string; path to TextGrid file.
# 
# 
# @examples
# print("")
#
helper_getTextGridForTranscript <- function(t) {
	
	if (missing(t)) 	{stop("Transcript object t is missing.") }	
	
	#=== check in corpus object if textgrid is given and exits
	if (!is.na(t@file.path)) {
		if (file.exists(t@file.path)) {
			if (stringr::str_to_lower(tools::file_ext(t@file.path))=="textgrid") {
				return(t@file.path)
			}
		}
	}
	
	#=== create temporary textgrid
	path <- file.path(tempdir(), stringr::str_c(t@name, ".TextGrid", collapse=""))
	act::export_textgrid(t, path)
	warning("Original TextGrid has not been found. A temporary TextGrid has been created")
	return(path)
}

helper_test_read <- function(input_path, 
							 testencoding, 
							 testlinenrs) {
	
	#assign("last.warning", NULL, envir = baseenv())
	input_path <- toString(input_path)
	tryCatch(
		{
			myCon <- file(input_path, encoding = testencoding)
			myLines <- readLines(myCon, n = testlinenrs)
			close(myCon)
			return (myLines)
		},

		error = function(c)
		{
			close(myCon)
			"error"
		},

		warning = function(c)
		{
			close(myCon)
			"error"
			#paste("warning:", warnings())
		},
		message = function(c)
		{
			"error"
			close(myCon)
			#"message"
		}
	)
}

helper_progress_set <- function(title, total) {
	#set progress bar	
	if(getOption("act.showprogress", TRUE)) {
		
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				title <- stringr::str_pad(title, width=24, side="right", pad=" ")
				act.environment$pb <- progress::progress_bar$new(
					format = paste("  ", title, "[:bar] :percent missing: :eta", sep=""),
					total = total, 
					clear = FALSE, 
					show_after = 0,
					width= 70)
			}
		}			
	}
}

helper_progress_tick <- function() {
	#update progress
	if (getOption("act.showprogress", TRUE)) {
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				if (!act.environment$pb$finished) {			
					act.environment$pb$tick()
				}
			}
		}
	}
}


