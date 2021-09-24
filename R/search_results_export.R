#' Exports search results
#' 
#' Search results from a search object will be saved to a Excel-XLSX or a CSV (comma separated values) file.
#' By default a XLSX file will be saved. If you want to save a CSV file, use \code{saveAsCSV=TRUE}.
#' Please note: 
#' - The function will '=' signs at the beginning of annotation by ".=". This is because the content would be interpreted as the beginning of a formula (leading to an error).
#' - In the case of writing to an excel file, line breaks will be replaced by "|". This is because line breaks will lead to an error.
#' 
#' @param s Search object. Search object containing the results you wish to export.
#' @param path Character string; path where file will be saved. Please add the suffix '.csv' or '.xlsx' to the file name.
#' @param sheetNameXLSX Character string, set the name of the excel sheet.
#' @param saveAsCSV Logical; if \code{TRUE} results will be saved as CSV file; Logical; if \code{FALSE} a XLS file will be saved.
#' @param encodingCSV Character string; text encoding for CSV files.
#' @param separatorCSV Character; single character that is used to separate the columns.
#' @param overwrite Logical; if \code{TRUE} existing files will be overwritten

#'
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_export <- function(s, path, sheetNameXLSX="data", saveAsCSV=FALSE, encodingCSV="UTF-8", separatorCSV=",", overwrite=TRUE) {
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		} else { if (class(s)[[1]]!="search")		{stop("Parameter 's' needs to be a search object.") 	} }
	
	

	#check colnames
	mycolnames <- colnames(s@results)
	necessarycolnames <- c("resultID", "transcript.name", "annotationID",  "tier.name", "startSec", "endSec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "search.mode", "hit.span")
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames>0)) {
		stop(	stringr::str_c(c("Some necessary columns are missing in the data frame '@results' in your search object. Missing columns: ", missingcolnames), sep="", collapse=" "))
	}
	
	#replace .  by , in numbers
	s@results$startSec		<-	gsub("\\.", ",", s@results$startSec)
	s@results$endSec		<-	gsub("\\.", ",", s@results$endSec) 
	
	#replace = at he beginning of cells
	searchString <-"^="
	replacementString <- ".="
	s@results$content		<-	stringr::str_replace_all(s@results$content, searchString, replacementString )
	s@results$content.norm	<-	stringr::str_replace_all(s@results$content.norm,searchString, replacementString)	
	s@results$hit			<-	stringr::str_replace_all(s@results$hit, searchString, replacementString )
	s@results$concLeft1		<-	stringr::str_replace_all(s@results$concLeft1, searchString, replacementString )
	s@results$concLeft2		<-	stringr::str_replace_all(s@results$concLeft2, searchString, replacementString )
	s@results$concHit		<-	stringr::str_replace_all(s@results$concHit, searchString, replacementString )
	s@results$concRight1	<-	stringr::str_replace_all(s@results$concRight1, searchString, replacementString )
	s@results$concRight2	<-	stringr::str_replace_all(s@results$concRight2, searchString, replacementString )
	if ("printtranscript" %in% mycolnames) {
		s@results$transcript	<-	stringr::str_replace_all(s@results$printtranscript, searchString, replacementString )
	}
	

	#write
	if (saveAsCSV) {
		if (!file.exists(path)) {
			utils::write.table(s@results, file = path, sep = separatorCSV, col.names = colnames(s@results), row.names=FALSE, qmethod = "double", fileEncoding= encodingCSV)
		} else if (file.exists(path) & overwrite) {
			file.remove(path)	
			#wait until file does not exist anymore
			for (i in 1:10) {
				if(!file.exists(path)) {
					break	
				}
				Sys.sleep(0.02)
			}
			if (file.exists(path)) {
				warning("Unable to overwrite existing file. No .csv file written")
			} else {
				utils::write.table(s@results, file = path, sep = separatorCSV, col.names = colnames(s@results), row.names=FALSE, qmethod = "double", fileEncoding= encodingCSV)
			}
		} else {
			warning("Destination file already exists. No .csv file written")
		}
	} else {
		if (!file.exists(path)) {
			openxlsx::write.xlsx(s@results, file=path, sheetName="data", overwrite=TRUE)
		} else if(file.exists(path) & overwrite) {
			file.remove(path)
			#wait until file does not exist anymore
			for (i in 1:10) {
				if(!file.exists(path)) {
					break	
				}
				Sys.sleep(0.02)
			}
			
			if (file.exists(path)) {
				warning("Unable to overwrite existing file. No .xlsx file written")
			} else {
				openxlsx::write.xlsx(s@results, file=path, sheetName="data", overwrite=TRUE)
			}
		} else {
			warning("Destination file already exists. No .xlsx file written")
		}
	}
}
