#' Search corpus and open first result in Praat
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It first searches your corpus object and uses the first search hit. 
#' The corresponding TextGrid will be opened in the 'Praat' TextGrid Editor and the search hit will be displayed.
#' 
#' To make this function work you need to set the path to the 'sendpraat' executable using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' # You can only use this functions if you have located the 'sendpraat' executable 
#' # properly in the package options.
#' \dontrun{
#' act::search_searchandopen_inpraat(x=examplecorpus, "pero")
#' }
#' 
#' 
#' 
search_searchandopen_inpraat <- function(x,
										 pattern) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

	if (missing(pattern)) 	{stop("Pattern is missing.") }	
	
	s <- act::search_new(x=x, pattern=pattern, concordanceMake=FALSE)
	if (is.null(s@results)) {
		#No results
		#return(NULL)
	} else if (nrow(s@results)==0) {
		#No results.
		#return(NULL)
	} else {
		act::search_openresult_inpraat(x, s, 1)
	}
}

#' Open a search result in 'Praat'
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It opens a search result in the 'Praat' TextGrid Editor.
#' 
#' To make this function work you need to set the path to the 'sendpraat' executable using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param s Search object. 
#' @param resultNr Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param closeAfterPlaying Logical; If \code{TRUE} TextGrid editor will be closed after playing (Currently non functional!)
#' @param filterFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is uncompressed audio > compressed audio.
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' mysearch <- act::search_new(x=examplecorpus, pattern = "pero")
#' 
#' # You can only use this functions if you have located the 'sendpraat' executable properly
#' # in the package options.
#' \dontrun{
#' act::search_openresult_inpraat(x=examplecorpus, s=mysearch, resultNr=1, TRUE, TRUE)
#' }
search_openresult_inpraat  <- function(x, 
									   s, 
									   resultNr, 
									   play=TRUE, 
									   closeAfterPlaying=FALSE, 
									   filterFile=c('.*\\.(aiff|aif|wav)', '.*\\.mp3') ) {
	
	# result <- mysearch@results[1,]
	# x <-examplecorpus
	# search_openresult_inpraat(x, searchresults[1,])
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	if (missing(resultNr)) {stop("Number of the search result 'resultNr' is missing.") 	}
	

	#--- check for sendpraat
	if (file.exists(options()$act.path.sendpraat)==FALSE)	{
		stop("Sendpraat not found. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
	}

	#--- get  corresponding transcript
	t <- x@transcripts[[s@results[resultNr, ]$transcript.name]]
	if (is.null(t))	{
		stop("Transcript not found in corpus object'.")
	}
	
	#--- get path of textgrid
	path_textgrid <- helper_getTextGridForTranscript(t)
	name_textgrid <- tools::file_path_sans_ext(basename(path_textgrid))
	#replace blanks by underscores, as praat does
	name_textgrid	<- stringr::str_replace_all(string = t@name, pattern=" ", replacement="_")
	
	#---get path of sound
	path_longsound <- media_getPathToExistingFile(t, filterFile=".*\\.(wav|mp3|aif|aiff)") 
	if (is.null(path_longsound))	{
		name_longsound <-""
		path_longsound <-""
		warning("No media file(s) found.")
	} else {
		name_longsound <- path_longsound
		if (nchar(path_longsound)>=0) {
			name_longsound      <- sub("[.][^.]*$", "", basename(path_longsound))
		}
		#replace blanks by underscores, as praat does
		name_longsound  <- stringr::str_replace_all(string = name_longsound, pattern=" ", replacement="_")
	}
	
	#--- get path to praat script
	praatScriptPath	<-	file.path(system.file("extdata", "praat", package="act"), "OpenSelectionInPraat.praat")

	#read script
	tx <- readLines(con= praatScriptPath, n=-1, warn=FALSE)
	
	#set values of variables
	
	tx  <- stringr::str_replace_all(string = tx, pattern = "PATHTEXTGRID",  replacement = path_textgrid)
	tx  <- stringr::str_replace_all(string = tx, pattern = "PATHLONGSOUND", replacement = path_longsound)
	tx  <- stringr::str_replace_all(string = tx, pattern = "SELSTARTSEC",   replacement = as.character(s@results[resultNr, ]$startSec))
	tx  <- stringr::str_replace_all(string = tx, pattern = "SELENDSEC",     replacement = as.character(s@results[resultNr, ]$endSec))
	tx  <- stringr::str_replace_all(string = tx, pattern = "PLAYSELECTION",     replacement = if(play) {as.character(1)} else {as.character(0)})
	tx  <- stringr::str_replace_all(string = tx, pattern = "CLOSEAFTERPLAYING",     replacement = if (closeAfterPlaying) {as.character(1)} else {as.character(0)})
	
	#write temporary script
	tempScriptPath <- file.path(tempdir(), "temp.praat")
	writeLines(tx, con=tempScriptPath)
	
	#send script
	cmd <- sprintf("%s praat \"runScript: \\\"%s\\\" \"", options()$act.path.sendpraat, tempScriptPath)
	rslt=system(cmd, intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
	
	
	# if execution of sendpraat resulted in an error, try to start praat
	if (!is.null(attributes(rslt)) ) {
		if (file.exists(options()$act.path.praat)==FALSE)	{
			stop("Praat is not running. Please start Praat first. To start Praat automatically indicate its location 'options(act.path.praat = ...)'.")
		}
		rslt = system(paste ("open" , options()$act.path.praat), intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
		rslt = system(cmd, intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
	}

	#delete temporary script
	if (file.exists(tempScriptPath))  {file.remove(tempScriptPath)}
	
}
