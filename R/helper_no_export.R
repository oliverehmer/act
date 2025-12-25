# Detect current operating system
#
helper_detect_os <- function(){
	sysinf <- Sys.info()
	if (!is.null(sysinf)){
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "macos"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "macos"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}



# Make names for search results
#
# @param search.results Data frame; data frame containing search results.
# @param resultidPrefix Character string; prefix for the name of the consecutively numbered search results.
# @param resultidStart Integer; start number of results 
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
# mynames <- act::search_names(searchresults, resultidPrefix="yo")
# 
# # Replace old names in search by new names
# searchresults$resultID <- mynames
# @keywords internal
helper_makeNamesForSearch <- function(search.results, 
									  resultidPrefix = "result",
									  resultidStart  = 1) {
	
	myFormat <- paste(resultidPrefix, "%0", nchar(toString(nrow(search.results)-1+resultidStart)), "d", sep="")
	myNames <- sprintf(myFormat, resultidStart:(nrow(search.results)+resultidStart-1))
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
#@keywords internal# 
# 
# @examples
# print("")

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
			
			if(	sum(stringr::str_length(myLines))==0) {
				return("error")
			}
			return (myLines)
		},
		
		error = function(c)
		{
			close(myCon)
			return("error")
		},
		
		warning = function(c)
		{
			close(myCon)
			return("error")
			#paste("warning:", warnings())
		},
		message = function(c)
		{
			return("error")
			close(myCon)
			#"message"
		}
	)
}
helper_test_read_OLDBACKUP <- function(input_path, 
							 testencoding, 
							 testlinenrs) {
	
	#assign("last.warning", NULL, envir = baseenv())
	input_path <- toString(input_path)
	
	if(!file.exists(input_path)) {
		return("error")
	}
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




