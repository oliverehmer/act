#' Open a search result in 'ELAN'
#'
#' The function creates an temporary .eaf file and a .psfx file that locates the search hit.
#' These files will then be opened in ELAN.
#' To make this function work you need to have 'ELAN' installed on your computer and tell the act package where ELAN is located.
#' Therefore you need to set the path to the ELAN executable in the option 'act.path.elan' using \code{options(act.path.elan='PATHTOYOURELANEXECUTABLE')}.
#' 
#' WARNING: This function will overwrite existing .psfx files. 
#' 
#' Credits: Thanks to Han Sloetjes for feedback on the structure of the temporary .pfsx files. He actually made the code work. 
#' 
#' @param x Corpus object.
#' @param s Search object. 
#' @param resultNr Integer; Number of the search result (row in the data frame \code{s@results}) to be opened.
#' @param openOriginalEafFileIfAvailable Logical; if \code{TRUE} the function will check if the original annotation file was an .eaf file and if it still exists in the original location. If so, the function will not create a temporary .eaf file but open the original file. Warning: The original .pfsx file (if it exists) will be overwritten.
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' mysearch <- act::search_new(x=examplecorpus, pattern = "yo")
#' 
#' # You can only use this function if you have installed ELAN on our computer.
#' \dontrun{
#' options(act.path.elan='PATHTOYOURELANEXECUTABLE')
#' act::search_openresult_inelan(x=examplecorpus, s=mysearch, resultNr=1, TRUE)
#' }
#' 

search_openresult_inelan  <- function(x, 
									  s, 
									  resultNr, 
									  openOriginalEafFileIfAvailable=FALSE) {
	
	#NOT IMPLEMENTED YET  @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first matches with existing media files will set as linked media in the eaf file. If the aprameter is left open, media files assigned to the transcript object will be set as links in the .eaf file.
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	if (missing(resultNr)) {stop("Number of the search result 'resultNr' is missing.") 	}
	
	
	#--- check if ELAN exists
	path.elan<- getOption("act.path.elan", default="")
	if(path.elan=="") {
		stop("ELAN not found. Please set the path to the ELAN executable in the option 'act.path.elan' using options(act.path.elan='PATHTOYOURELANEXECUTABLE')")
	} else {
		if(!file.exists(path.elan)) {
			stop("ELAN not found. Please set the path to the ELAN executable in the option 'act.path.elan' using options(act.path.elan='PATHTOYOURELANEXECUTABLE')")
		}	
	}
	
	#--- get corresponding transcript
	t <- x@transcripts[[s@results$transcript.name[resultNr]]]
	if (is.null(t))	{
		stop("Transcript not found in corpus object'.")
	}
	
	#--- set paths to ""
	file.path.eaf <- ""
	file.path.pfsx <- ""
	
	#--- check for original elan file
	if(openOriginalEafFileIfAvailable) {
		if(t@file.type=="eaf") {
			if(file.exists(t@file.path)) {
				file.path.eaf <- t@file.path
			}		
		}
	}
	
	#--- create temporary eaf if original not found
	if (file.path.eaf == ""	) {
		file.path.eaf <- file.path(tempdir(), stringr::str_c(t@name, ".eaf", collapse=""))
		act::export_eaf(t, file.path.eaf)
		if(openOriginalEafFileIfAvailable) {
			warning("Original .eaf file has not been found. A temporary .eaf file has been created")
		}
	}
	
	#--- create pfsx file
	#check if pfsx file already exists - make a backup
	#	file.path.eaf<-'/Users/oliverehmer/Desktop/Quiz.eaf'
	#	pattern<- stringr::str_replace(basename(file.path.eaf), pattern='eaf',replacement="*pfsx$") 
	#	filenames <- list.files(dirname(file.path.eaf), pattern=pattern)
	#	filenames <- tools::file_path_sans_ext(filenames)
	#	destination.name <- tools::file_path_sans_ext(basename(file.path.eaf))
	#   check if destinatino name already exists
	#	if(destination.name %in% filenames) {
	#		uniquename<- make.unique(filenames, destination.name)
	#}
	
	pfsx<-	   '<?xml version="1.0" encoding="UTF-8"?>
				<preferences version="1.1"
				    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://www.mpi.nl/tools/elan/Prefs_v1.1.xsd">
				        <pref key="SelectionBeginTime">
				            <Long>%s</Long>
				        </pref>
				        <pref key="SelectionEndTime">
				            <Long>%s</Long>
				        </pref>
				        <pref key="TimeScaleBeginTime">
				            <Long>%s</Long>
				        </pref>
				        <pref key="MediaTime">
				            <Long>%s</Long>
				    </pref>
				</preferences>'
	startMiliSec <- round(s@results$startSec[resultNr]*1000, 0) 
	endMiliSec <- round(s@results$endSec[resultNr]*1000, 0) 
	pfsx.1 <- sprintf(pfsx, startMiliSec, endMiliSec, max(0, startMiliSec-1000),  startMiliSec)
	#cat(pfsx.1)
	
	#write to file
	file.path.pfsx<- stringr::str_replace(file.path.eaf, pattern='\\.eaf', replacement=".pfsx")
	fileConn <- file(file.path.pfsx, open="wb")
	writeBin(charToRaw(pfsx.1), fileConn, endian="little")
	close(fileConn)
	
	#wait until pfsx exists
	for (i in 1:10) {
		if(file.exists(file.path.pfsx)) {
			break	
		}
		Sys.sleep(0.02)
	}
	
	if(file.exists(file.path.pfsx)) {
		#--- open eaf file
		if (helper_detect_os()=="windows" ){
			cmd <- sprintf("%s %s",   shQuote(path.elan), shQuote(file.path.eaf))
		} else {
			cmd <- sprintf("open %s -a %s",  shQuote(file.path.eaf), shQuote(path.elan))
		}
		#--- open file
		rslt <- system(cmd, wait=FALSE)
	}
}
