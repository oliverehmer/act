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
#' @param resultid Integer; Number of the search result (row in the data frame \code{s@results}) to be opened.
#' @param openOriginal Logical; if \code{TRUE} the function will check if the original annotation file was an .eaf file and if it still exists in the original location. If so, the function will not create a temporary .eaf file but open the original file. Warning: The original .pfsx file (if it exists) will be overwritten.
#' @param overwrite Logical; if \code{TRUE} (default) an existing .pfsx file will be overwritten. If \code{FALSE} an existing .pfsx file is kept.
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

search_openresult_inelan  <- function(x,
									  s,
									  resultid,
									  openOriginal=FALSE,
									  overwrite=TRUE) {
	
	#NOT IMPLEMENTED YET  @param filterMediaFile Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first matches with existing media files will set as linked media in the eaf file. If the aprameter is left open, media files assigned to the transcript object will be set as links in the .eaf file.
	
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	if (missing(resultid)) {cli::cli_abort("Number of the search result {.arg resultid} is missing.") 	}
	
	
	#--- check if ELAN exists
	path.elan<- getOption("act.path.elan", default="")
	if(path.elan=="") {
		cli::cli_abort("ELAN not found. Please set the path to the ELAN executable in the option {.arg act.path.elan} using options(act.path.elan={.arg PATHTOYOURELANEXECUTABLE})")
	} else {
		if(!file.exists(path.elan)) {
			cli::cli_abort("ELAN not found. Please set the path to the ELAN executable in the option {.arg act.path.elan} using options(act.path.elan={.arg PATHTOYOURELANEXECUTABLE})")
		}	
	}
	
	#--- get corresponding transcript
	t <- x@transcripts[[s@results$transcriptName[resultid]]]
	if (is.null(t))	{
		cli::cli_abort("Transcript not found in corpus object'.")
	}
	
	#--- set paths to ""
	filePath.eaf <- ""
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
		filePath.eaf <- file.path(tempdir(), stringr::str_c(t@name, ".eaf", collapse=""))
		act::export_eaf(t, filePath.eaf)
		if(openOriginal) {
			cli::cli_warn("Original .eaf file has not been found. A temporary .eaf file has been created")
		}
	}
	
	#--- create pfsx file
	filePath.pfsx<- stringr::str_replace(filePath.eaf, pattern=stringr::regex('\\.eaf$', ignore_case=TRUE), replacement=".pfsx")
	export_create_pfsx(path.out.eaf  = filePath.eaf,
					   startSec      = s@results$startsec[resultid],
					   endSec        = s@results$endsec[resultid],
					   tierName      = NULL,
					   overwrite     = overwrite)

	if(file.exists(filePath.pfsx)) {
		#--- open eaf file
		if (.detect_os()=="macos"){
			cmd <- sprintf("open %s -a %s",  shQuote(filePath.eaf), shQuote(path.elan))
		} else {
			cmd <- sprintf("%s %s",   shQuote(path.elan), shQuote(filePath.eaf))
		}
		#--- open file
		rslt <- system(cmd, wait=FALSE)
	}
}
