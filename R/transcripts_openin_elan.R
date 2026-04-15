#' Open transcript in 'ELAN'
#'
#' The function opens a transcript in ELAN-
#' It can open the an original .eaf file (if it exists) or create an .eaf file.
#' A .psfx file will be created id startSec and endSec are defined.
#'
#' To make this function work you need to have 'ELAN' installed on your computer and tell the act package where ELAN is located.
#' Therefore you need to set the path to the ELAN executable in the option 'act.path.elan' using \code{options(act.path.elan='PATHTOYOURELANEXECUTABLE')}.
#' 
#' WARNING: This function will overwrite existing .psfx files. 
#' 
#' Credits: Thanks to Han Sloetjes for feedback on the structure of the temporary .pfsx files. He actually made the code work. 
#' 
#' @param t Transcript object.
#' @param openOriginal Logical; if \code{TRUE} the function will check if the original annotation file was an .eaf file and if it still exists in the original location. If so, the function will not create a temporary .eaf file but open the original file. Warning: The original .pfsx file (if it exists) will be overwritten.
#' @param filePathOut Character string, optional. If original .eaf is not found, a .eaf file will be created at this location. If \code{NULL} a temporary .eaf file will be created. 
#' @param startSec Double, Optional, start of selection en ELAN
#' @param endSec Double, Optional, end of selection en ELAN
#' @param tierName Character string, Optional, Name of tier to select in ELAN grid view.
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
#' act::search_openresult_inelan(x=examplecorpus, s=mysearch, resultid=1, TRUE)
#' }
#' 

transcripts_openin_elan  <- function(t,
									 openOriginal=FALSE,
									 filePathOut    = NULL,
									 startSec    = NULL,
									 endSec      = NULL,
									 tierName    = NULL
									 ) {

	
	if (missing(t)) 	{cli::cli_abort("Transcript object in parameter {.arg t} is missing.") 		}	else { if (!methods::is(t,"transcript")   )	{cli::cli_abort("Parameter {.arg t} needs to be a {.cls corpus} object.") } }
	
	#--- check if ELAN exists
	path.elan<- getOption("act.path.elan", default="")
	if(path.elan=="") {
		cli::cli_abort("ELAN not found. Please set the path to the ELAN executable in the option {.arg act.path.elan} using options(act.path.elan={.arg PATHTOYOURELANEXECUTABLE})")
	} else {
		if(!file.exists(path.elan)) {
			cli::cli_abort("ELAN not found. Please set the path to the ELAN executable in the option {.arg act.path.elan} using options(act.path.elan={.arg PATHTOYOURELANEXECUTABLE})")
		}	
	}
	
	#--- set paths to ""
	filePath.eaf  <- ""
	filePath.pfsx <- ""
	
	#--- check for original elan file
	if(openOriginal) {
		if(t@file.type=="eaf") {
			if(file.exists(t@file.path)) {
				filePath.eaf <- t@file.path
			}		
		}
	}
	
	#--- create temporary eaf if original not found
	if (filePath.eaf == ""	) {
		if (!is.null(filePathOut)) {
			if (!dir.exists(dirname(filePathOut))) {
				cli::cli_abort("Destination folder does not exist: {.path {dirname(filePathOut)}}")
			} else {
				filePath.eaf <- filePathOut
			}
		} else {
			filePath.eaf <- file.path(tempdir(), stringr::str_c(t@name, ".eaf", collapse=""))
		}		
		act::export_eaf(t, filePath.eaf)
		if(openOriginal) {
			cli::cli_inform("Original .eaf file has not been found. A temporary .eaf file has been created.")
		}
	}
	
	#--- create pfsx if startSec & endSec are set
	if (!is.null(startSec) && !is.null(endSec)) {
		if (!is.na(startSec) && !is.na(endSec)) {
			export_create_pfsx(path.out.eaf  = filePath.eaf, 
							   startSec      = startSec, 
							   endSec        = endSec, 
							   tierName      = tierName)
		}
	}
	
	#--- open eaf file
	if (.detect_os()=="windows" ){
		cmd <- sprintf("%s %s",   shQuote(path.elan), shQuote(filePath.eaf))
	} else {
		cmd <- sprintf("open %s -a %s",  shQuote(filePath.eaf), shQuote(path.elan))
	}
	#--- open file
	rslt <- system(cmd, wait=FALSE)
}



export_create_pfsx <- function(path.out.eaf, 
							   startSec, 
							   endSec, 
							   tierName) {
	#--- create pfsx file
	#check if pfsx file already exists - make a backup
	#	path.out.eaf<-'/Users/oliverehmer/Desktop/Quiz.eaf'
	#	pattern<- stringr::str_replace(basename(path.out.eaf), pattern='eaf',replacement="*pfsx$") 
	#	filenames <- list.files(dirname(path.out.eaf), pattern=pattern)
	#	filenames <- tools::file_path_sans_ext(filenames)
	#	destination.name <- tools::file_path_sans_ext(basename(path.out.eaf))
	#   check if destinatino name already exists
	#	if(destination.name %in% filenames) {
	#		uniquename<- make.unique(filenames, destination.name)
	#}
	if (is.null(tierName)) {
		tierName <- ''
	}
	if (is.na(tierName)) {
		tierName <- ''
	}
	startMiliSec <- round(startSec*1000, 0) 
	endMiliSec   <- round(endSec*1000, 0) 
	
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
				        <pref key="LayoutManager.SelectedTabIndex">
                    <Int>0</Int>
                </pref>
				        <pref key="GridViewer.TierName">
                    <String>%s</String>
                </pref>
				</preferences>'
	
	
	pfsx.1 <- sprintf(pfsx, startMiliSec, endMiliSec, max(0, startMiliSec-1000),  startMiliSec, tierName)
	#cat(pfsx.1)
	
	#write to file
	filePath.pfsx<- stringr::str_replace(path.out.eaf, pattern='\\.eaf', replacement=".pfsx")
	fileConn <- file(filePath.pfsx, open="wb")
	writeBin(charToRaw(pfsx.1), fileConn, endian="little")
	close(fileConn)
	
	#wait until pfsx exists
	for (i in 1:10) {
		if(file.exists(filePath.pfsx)) {
			break	
		}
		Sys.sleep(0.02)
	}
}