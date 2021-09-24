#' Open a search result in 'Praat'
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It opens a search result in the 'Praat' TextGrid Editor.
#' 
#' To make this function work you need to do two things first:
#' - Install 'sendpraat' on your computer. To do so  follow the  instructions in the vignette 'installation-sendpraat'. Show the vignette with \code{vignette("installation-sendpraat")}.
#' - Set the path to the 'sendpraat' executable correctly by using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param s Search object. 
#' @param resultNr Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param closeAfterPlaying Logical; If \code{TRUE} TextGrid editor will be closed after playing (Currently non functional!)
#' @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is uncompressed audio > compressed audio.
#' @param delayBeforeOpen Double; Time in seconds before the section will be opened in Praat. This is useful if Praat opens but the section does not. In that case increase the delay. 
#' @export
#'
#' @examples
#' library(act)
#' 
#' mysearch <- act::search_new(x=examplecorpus, pattern = "pero")
#' 
#' # You can only use this functions if you have installed and 
#' # located the 'sendpraat' executable properly in the package options.
#' \dontrun{
#' act::search_openresult_inpraat(x=examplecorpus, s=mysearch, resultNr=1, TRUE, TRUE)
#' }
search_openresult_inpraat  <- function(x, 
									   s, 
									   resultNr, 
									   play=TRUE, 
									   closeAfterPlaying=FALSE, 
									   filterMediaFile=c('.*\\.(aiff|aif|wav)', '.*\\.mp3'), 
									   delayBeforeOpen=0.5) {
	
	# result <- mysearch@results[1,]
	# x <- examplecorpus
	# search_openresult_inpraat(x, searchresults[1,])
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	if (missing(resultNr)) {stop("Number of the search result 'resultNr' is missing.") 	}
	
	
	#--- check for sendpraat
	if (is.null(options()$act.path.praat)) {
		stop("Path to sendpraat is not set found. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
	} else {
		if (file.exists(options()$act.path.sendpraat)==FALSE)	{
			stop("Sendpraat not found. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
		}
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
	path_longsound <- media_getPathToExistingFile(t, filterMediaFile=".*\\.(wav|mp3|aif|aiff)") 
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
	#tempScriptPath <- file.path(tempdir(), "temp.praat")
	tempScriptPath <- tempfile(pattern = "openresult", tmpdir = tempdir(), fileext = ".praat")
	writeLines(tx, con=tempScriptPath)
	
	#wait until temporary script exists
	for (i in 1:10) {
		if(file.exists(tempScriptPath)) {
			break	
		}
		Sys.sleep(0.02)
	}
	
	if(file.exists(tempScriptPath)) {
		
		#but produce a delay
		Sys.sleep(delayBeforeOpen)
		
		#run script via sendpraat 
		cmd  <- sprintf("%s praat \"runScript: \\\"%s\\\" \"", shQuote(options()$act.path.sendpraat), tempScriptPath)
		rslt <- system(cmd, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
		
		# if execution of sendpraat resulted in an error, try to start praat
		#if intern =FALSE the values will be
		#success rslt=0
		#fail    rslt=1
		if (rslt==1){
			if (is.null(options()$act.path.praat)) {
				stop("Praat is not running. And the path to the your Praat executable is not set. Please start Praat first or indicate its location with 'options(act.path.praat = ...)'.")
			} else {
				if (file.exists(options()$act.path.praat)==FALSE)	{
					stop("Praat is not running. Please start Praat first. To start Praat automatically indicate its location 'options(act.path.praat = ...)'.")
				}
			}
		
			#start praat 
			if (helper_detect_os()=="windows") {
				#start praat WITHOUT waiting for it to finish
				cmd2 <- sprintf("%s", shQuote(options()$act.path.praat))
				rslt <- system(cmd2, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=FALSE)
				
			} else {
				#start praat WITH waiting
				cmd2 <- sprintf("open %s", shQuote(options()$act.path.praat))
				rslt <- system(cmd2, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
			}
			
			#but produce a delay
			Sys.sleep(delayBeforeOpen)
			
			#run script via sendpraat 
			cmd  <- sprintf("%s praat \"runScript: \\\"%s\\\" \"", shQuote(options()$act.path.sendpraat), tempScriptPath)
			rslt <- system(cmd, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
			
		}
		#delete temporary script
		file.remove(tempScriptPath)
	}
}
