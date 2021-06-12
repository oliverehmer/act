#' Import a single annotation file
#'
#' Advice: In most situations it is more convenient to use \code{act::corpus_new}, \code{act::corpus_import} for importing annotation files.
#' 
#' Imports the contents of an annotation file and returns a transcript object.
#' 
#' The input to this function in the parameter '...' may either be 
#' (1) the path to an annotation file (Currently 'ELAN' .eaf, 'EXMARaLDA .exb and 'Praat' .TextGrid files),
#' (2) the contents of an annotation file obtained from the \code{@file.content} or by reading the contents of the files directly with \code{read.lines()} or
#' (3) a \code{rPraat} TextGrid object.
#' 
#' Only the first input to '...' will be processed
#' 
#' @param ... file path, contents of an annotation file or rPraat object; see description above.
#' @param transcriptName Character string; name of the transcript, if this parameter is set, the default name of the transcript will be changed.
#' 
#' @return Transcript object.
#' 
#' @seealso \code{corpus_import}, \code{corpus_new}, \code{import_eaf}, \code{import_exb}, \code{import_rpraat}, \code{import_textgrid}
#' 
#' @export
#'
#' @example inst/examples/import.R
#' 
import <- function(..., transcriptName=NULL) {
	
	arguments <- list(...)
	
	if (length(arguments)==0) {
		warning("The parameter '...' may not be empty.")
	}
	argument <- arguments[[1]]
	# if argument is a list
	if (typeof(argument)=="list") {
			#check it if it is a rpraat object
			if (length(setdiff(c("tmin", "tmax", "type", "name"), names(class(argument))))==0) {
				test <- act::import_rpraat(fileContent=argument)
			} else {
				warning("Unable to identify the input format.")
				return(NULL)
			}	
	
		if (!is.null(transcriptName)) {
			test@name <- transcriptName
		}
	} else {
		#-- check if this is a file path
		#take the first line
		filePath <- argument[1]
		type <- stringr::str_to_lower(tools::file_ext(filePath[1]))
		if (type=="eaf") { 
			test <- act::import_eaf(filePath=filePath)
		} else if (type=="exb") {
			test <- act::import_exb(filePath=filePath)
		} else if (type=="textgrid") {
			test <- act::import_textgrid(filePath=filePath)
		} else {
			#--- check if it is the content of an annotation file
			#eaf
			if (any(stringr::str_detect(argument, pattern="http://www.mpi.nl/tools/elan/EAFv3.0.xsd" ))) {
				if (is.null(transcriptName)) {transcriptName <- "imported eaf"}
				test <- act::import_eaf(fileContent=argument, transcriptName=transcriptName)
			#exb
			} else if (any(stringr::str_detect(argument, pattern="<basic-transcription>"))) {
				#write to temporary file and read
				tempPath <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = "exb")
				fileConn <- file(tempPath, encoding="UTF-8")
				writeLines(argument, fileConn)
				close(fileConn)
				#import
				if (is.null(transcriptName)) {transcriptName <- "imported textgrid"}
				test <- act::import_exb(filePath=tempPath, transcriptName=transcriptName)
				#set path to ""
				test@file.path <-""
				
			#textgrid
			} else if (any(stringr::str_detect(argument, pattern="ooTextFile"))) {
				if (is.null(transcriptName)) {transcriptName <- "imported textgrid"}
				test <- act::import_textgrid(fileContent=argument, transcriptName=transcriptName)
				
			} else {
				warning("Unable to identify the input format.")
				return(NULL)
			}
		}
	}
	return(test)
}




