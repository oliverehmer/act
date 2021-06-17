#' Import search results 
#' 
#' Search results will be imported from an Excel '.xlsx' file or a comma separated values '.csv' file into a search object.
#'
#' @param path Character string; path to file from where data will be loaded.
#' @param revertReplacements Logical, when exporting search results from act, '=' at the beginning of lines are replaced by '.=", and in numbers the decimal separator '.' is replaced by a ",". If \code{TRUE}, this replacement will be reverted when importing search results.
#' @param sheetNameXLSX Character string, set the name of the excel sheet containing the data.
#' @param encodingCSV Character string; text encoding in the case of CVS files.
#' @param separatorCSV Character; single character that is used to separate the columns in CSV files.
#'
#' @return Search object.
#' 
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_import <- function(path, 
								  revertReplacements=TRUE,
								  sheetNameXLSX="data", 
								  encodingCSV="UTF-8", 
								  separatorCSV=",") {
	
	filetype <- tools::file_ext(path)
	if (filetype=="csv") {
		temp <- utils::read.table(path, header = TRUE, sep = separatorCSV, fileEncoding = encodingCSV, encoding=encodingCSV )
		if (is.null(temp$resultID)==FALSE){
			rownames(temp)<- temp$resultID
		}
	} else {
		temp <- openxlsx::read.xlsx(xlsxFile=path, sheet=sheetNameXLSX)
	}
	
	#check colnames
	necessarycolnames <- c("resultID", "transcript.name", "annotationID",  "tier.name", "startSec", "endSec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "search.mode", "hit.span")
	mycolnames <- colnames(temp)
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames>0)) {
		stop(	stringr::str_c(c("Some necessary columns are missing in your input file'. Missing columns: ", missingcolnames), sep="", collapse=" "))
	}
	
	if(revertReplacements) {
		#replace .  by , in numbers
		temp$startSec	<-	gsub(",", "\\.", temp$startSec)
		temp$endSec		<-	gsub(",", "\\.", temp$endSec) 
		
		#replace = at he beginning of cells
		searchString <-"^\\.="
		replacementString <- "="
		temp$content		<-	stringr::str_replace_all(temp$content, searchString, replacementString )
		temp$content.norm	<-	stringr::str_replace_all(temp$content.norm,searchString, replacementString)	
		temp$hit			<-	stringr::str_replace_all(temp$hit, searchString, replacementString )
		temp$concLeft1		<-	stringr::str_replace_all(temp$concLeft1, searchString, replacementString )
		temp$concLeft2		<-	stringr::str_replace_all(temp$concLeft2, searchString, replacementString )
		temp$concHit		<-	stringr::str_replace_all(temp$concHit, searchString, replacementString )
		temp$concRight1		<-	stringr::str_replace_all(temp$concRight1, searchString, replacementString )
		temp$concRight2		<-	stringr::str_replace_all(temp$concRight2, searchString, replacementString )
		temp$transcript		<-	stringr::str_replace_all(temp$transcript, searchString, replacementString )
	}
	
	#turn factors into vectors
	fctr.cols 					<- sapply(temp, is.factor)
	temp[, fctr.cols] 			<- sapply(temp[, fctr.cols], as.character)
	temp[is.na(temp)]			<- " "
	
	temp$startSec				<- as.double(temp$startSec)
	temp$endSec					<- as.double(temp$endSec)
	
	temp$annotationID					<- as.integer(temp$annotationID)
	temp$hit.length				<- as.integer(temp$hit.length)
	temp$hit.nr					<- as.integer(temp$hit.nr)
	temp$hit.pos.content		<- as.integer(temp$hit.pos.content)
	temp$hit.pos.fulltext		<- as.integer(temp$hit.pos.fulltext)
	temp$nrWordsLeft			<- as.integer(temp$nrWordsLeft)
	temp$nrWordsRight			<- as.integer(temp$nrWordsRight)
	temp$nrWordsHitPosition		<- as.integer(temp$nrWordsHitPosition)
	temp$nrWordsTotal			<- as.integer(temp$nrWordsTotal)
	
	#Create a search object
	s <- methods::new("search")
	#s@pattern                   <- pattern
	#s@search.mode                <- searchMode
	#s@search.normalized          <- searchNormalized
	#s@filter.section.startsec                  <- if(!is.na(startSec)) {if(!is.null(startSec)) {startSec}} else {s@filter.section.startsec}
	#s@filter.section.endsec                    <- if(!is.na(endSec))   {if(!is.null(endSec))   {endSec}}   else {s@filter.section.endsec}
	#s@filter.tier.includeRegEx      <- ""
	#s@filter.tier.excludeRegEx      <- ""
	#s@filter.transcript.includeRegEx  <- if(!is.na(filterTranscriptIncludeRegEx))   {if(!is.null(filterTranscriptIncludeRegEx))   {filterTranscriptIncludeRegEx}}   else {s@filter.transcript.includeRegEx }
	#s@filter.transcript.excludeRegEx  <- if(!is.na(filterTranscriptExcludeRegEx))   {if(!is.null(filterTranscriptExcludeRegEx))   {filterTranscriptExcludeRegEx}}   else {s@filter.transcript.excludeRegEx }
	s@results <- temp
	
	return(s)
}
