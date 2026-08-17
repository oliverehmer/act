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
#' @param resultid Integer; Number of the search result (row in the data frame \code{s@results}) to be played. 
#' @param play Logical; If \code{TRUE} selection will be played.
#' @param close Logical; If \code{TRUE} TextGrid editor will be closed after playing (Currently non functional!)
#' @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is uncompressed audio > compressed audio.
#' @param delay Double; Time in seconds before the section will be opened in Praat. This is useful if Praat opens but the section does not. In that case increase the delay. 
#' @seealso \code{vignette("install_sendpraat", package = "act")}
#'
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
#' act::search_openresult_inpraat(x=examplecorpus, s=mysearch, resultid=1, TRUE, TRUE)
#' }
search_openresult_inpraat  <- function(x, 
									   s, 
									   resultid, 
									   play           =TRUE, 
									   close          =FALSE, 
									   filterMediaFile=c('(?i).*\\.(aiff|aif|wav)', '(?i).*\\.mp3'),
									   delay          =0.5) {
	
	# result <- mysearch@results[1,]
	# x <- examplecorpus
	# search_openresult_inpraat(x, searchresults[1,])
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	
	
	if (missing(resultid)) {cli::cli_abort("Number of the search result {.arg resultid} is missing.") 	}
	
	
	#--- check for sendpraat
	if (is.null(options()$act.path.sendpraat)) {
		cli::cli_abort("Path to sendpraat is not set. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
	} else {
		if (file.exists(options()$act.path.sendpraat)==FALSE)	{
			cli::cli_abort("Sendpraat not found. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
		}
	}
	
	#--- get  corresponding transcript
	t <- x@transcripts[[s@results[resultid, ]$transcriptName]]
	if (is.null(t))	{
		cli::cli_abort("Transcript not found in corpus object'.")
	}
	
	#--- get path of textgrid
	path_textgrid <- .get_textgrid_for_transcript(t)
	name_textgrid <- tools::file_path_sans_ext(basename(path_textgrid))
	#replace blanks by underscores, as praat does
	name_textgrid	<- stringr::str_replace_all(string = t@name, pattern=" ", replacement="_")
	
	#---get path of sound
	path_longsound <- media_path_to_existing_file(t, filterMediaFile="(?i).*\\.(wav|mp3|aif|aiff)")
	if (is.null(path_longsound))	{
		name_longsound <-""
		path_longsound <-""
		cli::cli_warn("No media file(s) found.")
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
	tx <- readLines(con= praatScriptPath, n=-1, warn=FALSE, encoding="UTF-8")

	#set values of variables
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "PATHTEXTGRID",  replacement = path_textgrid)
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "PATHLONGSOUND", replacement = path_longsound)
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "SELSTARTSEC",   replacement = as.character(s@results[resultid, ]$startsec))
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "SELENDSEC",     replacement = as.character(s@results[resultid, ]$endsec))
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "PLAYSELECTION",     replacement = if(play) {as.character(1)} else {as.character(0)})
	tx  <- stringi::stri_replace_all_fixed(str = tx, pattern = "close",     replacement = if (close) {as.character(1)} else {as.character(0)})

	#write temporary script
	#tempScriptPath <- file.path(tempdir(), "temp.praat")
	tempScriptPath <- tempfile(pattern = "openresult", tmpdir = tempdir(), fileext = ".praat")
	tempScriptCon <- file(tempScriptPath, open="wb")
	writeLines(enc2utf8(tx), con=tempScriptCon, sep="\n", useBytes=TRUE)
	close(tempScriptCon)
	tempScriptPath <- normalizePath(tempScriptPath, winslash="/", mustWork=FALSE)
	
	#wait until temporary script exists
	for (i in 1:10) {
		if(file.exists(tempScriptPath)) {
			break	
		}
		Sys.sleep(0.02)
	}
	
	if(file.exists(tempScriptPath)) {
		
		#but produce a delay
		Sys.sleep(delay)
		
		#run script via sendpraat 
		cmd  <- sprintf("%s praat \"runScript: \\\"%s\\\"\"", shQuote(options()$act.path.sendpraat), tempScriptPath)
		rslt <- system(cmd, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
		
		# if execution of sendpraat resulted in an error, try to start praat
		#if intern =FALSE the values will be
		#success rslt=0
		#fail    rslt=1
		if (rslt!=0){
			if (is.null(options()$act.path.praat)) {
				cli::cli_abort("Praat is not running. And the path to the your Praat executable is not set. Please start Praat first or indicate its location with 'options(act.path.praat = ...)'.")
			} else {
				if (file.exists(options()$act.path.praat)==FALSE)	{
					cli::cli_abort("Praat is not running. Please start Praat first. To start Praat automatically indicate its location 'options(act.path.praat = ...)'.")
				}
			}
		
			#start praat
			if (.detect_os()=="macos") {
				#start praat WITH waiting
				cmd2 <- sprintf("open %s", shQuote(options()$act.path.praat))
				rslt <- system(cmd2, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
			} else {
				#start praat WITHOUT waiting for it to finish
				cmd2 <- sprintf("%s", shQuote(options()$act.path.praat))
				rslt <- system(cmd2, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=FALSE)
			}
			
			#but produce a delay
			Sys.sleep(delay)
			
			#run script via sendpraat 
			cmd  <- sprintf("%s praat \"runScript: \\\"%s\\\"\"", shQuote(options()$act.path.sendpraat), tempScriptPath)
			rslt <- system(cmd, intern=FALSE, ignore.stderr = TRUE, ignore.stdout=TRUE, wait=TRUE)
			
		}
		#delete temporary script
		file.remove(tempScriptPath)
	}
}
