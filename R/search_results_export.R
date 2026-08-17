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
#' @param sheetName Character string, set the name of the excel sheet.
#' @param saveAsCSV Logical; if \code{TRUE} results will be saved as CSV file; Logical; if \code{FALSE} a XLS file will be saved.
#' @param encoding Character string; text encoding for CSV files.
#' @param separator Character; single character that is used to separate the columns.
#' @param overwrite Logical; if \code{TRUE} existing files will be overwritten

#'
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_export <- function(s, 
								  path, 
								  sheetName="data", 
								  saveAsCSV=FALSE, 
								  encoding="UTF-8", 
								  separator=";", 
								  overwrite=TRUE) {
	
	if (1==2) {
		s<-s 
		path <- '/Users/oliverehmer/Downloads/Kniffel_4x_DIN-A4.csv'
		sheetName<-"data" 
		saveAsCSV<-FALSE 
		encoding<-"UTF-8" 
		separator<-";" 
		overwrite<-TRUE 
	}
	
	.assert_search(s, missing = missing(s))
	
	#check colnames
	mycolnames <- colnames(s@results)
	necessarycolnames <- c("resultID", "transcriptName", "annotationID",  "tierName", "startsec", "endsec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "searchMode", "hit.span")
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames) > 0) {
		cli::cli_abort("Some necessary columns are missing in {.code s@results}. Missing columns: {.val {missingcolnames}}")
	}
	
	#replace .  by , in numbers
	s@results$startsec		<-	gsub("\\.", ",", s@results$startsec)
	s@results$endsec		<-	gsub("\\.", ",", s@results$endsec) 

	#replace = at he beginning of cells
	searchString <-"^="
	replacementString <- "\\'="
	s@results$content		<-	stringr::str_replace_all(s@results$content, searchString, replacementString )
	s@results$content.norm	<-	stringr::str_replace_all(s@results$content.norm,searchString, replacementString)	
	s@results$hit			<-	stringr::str_replace_all(s@results$hit, searchString, replacementString )

	if ("concLeft1" %in% mycolnames) {
		s@results$concLeft1		<-	stringr::str_replace_all(s@results$concLeft1, searchString, replacementString )
	}
	if ("concLeft2" %in% mycolnames) {
		s@results$concLeft2		<-	stringr::str_replace_all(s@results$concLeft2, searchString, replacementString )
	}
	if ("concHit" %in% mycolnames) {
		s@results$concHit		<-	stringr::str_replace_all(s@results$concHit, searchString, replacementString )
	}
	if ("concRight1" %in% mycolnames) {
		s@results$concRight1	<-	stringr::str_replace_all(s@results$concRight1, searchString, replacementString )
	}
	if ("concRight2" %in% mycolnames) {
		s@results$concRight2	<-	stringr::str_replace_all(s@results$concRight2, searchString, replacementString )
	}
	if ("printtranscript" %in% mycolnames) {
		s@results$printtranscript <- stringr::str_replace_all(s@results$printtranscript, searchString, replacementString )
	}
	if ("stills.values" %in% mycolnames) {
		s@results$stills.values <- stringr::str_flatten(unlist(s@results$stills.values))
	}

	#rename columns to snake_case for output
	col_rename <- c(
		"resultID"               = "result_id",
		"transcriptName"         = "transcript_name",
		"annotationID"           = "annotation_id",
		"tierName"               = "tier_name",
		"searchMode"             = "search_mode",
		"content.norm"           = "content_norm",
		"hit.nr"                 = "hit_nr",
		"hit.length"             = "hit_length",
		"hit.pos.content"        = "hit_pos_content",
		"hit.pos.fulltext"       = "hit_pos_fulltext",
		"hit.span"               = "hit_span",
		"stills.values"          = "stills_values",
		"stills.folder"          = "stills_folder",
		"char.orig.bytime.start" = "char_orig_bytime_start",
		"char.orig.bytime.end"   = "char_orig_bytime_end",
		"char.norm.bytime.start" = "char_norm_bytime_start",
		"char.norm.bytime.end"   = "char_norm_bytime_end",
		"char.orig.bytier.start" = "char_orig_bytier_start",
		"char.orig.bytier.end"   = "char_orig_bytier_end",
		"char.norm.bytier.start" = "char_norm_bytier_start",
		"char.norm.bytier.end"   = "char_norm_bytier_end",
		"concLeft1"              = "conc_left_1",
		"concLeft2"              = "conc_left_2",
		"concHit"                = "conc_hit",
		"concRight1"             = "conc_right_1",
		"concRight2"             = "conc_right_2",
		"nrWordsLeft"            = "nr_words_left",
		"nrWordsHitPosition"     = "nr_words_hit_position",
		"nrWordsHit"             = "nr_words_hit",
		"nrWordsRight"           = "nr_words_right",
		"nrWordsTotal"           = "nr_words_total"
	)
	for (old in names(col_rename)) {
		idx <- which(colnames(s@results) == old)
		if (length(idx) > 0L) colnames(s@results)[idx] <- col_rename[[old]]
	}

	#write
	if (saveAsCSV) {
		if (!file.exists(path)) {
			utils::write.table(s@results, file = path, sep = separator, col.names = colnames(s@results), row.names=FALSE, qmethod = "double", fileEncoding= encoding)
			
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
				cli::cli_warn("Unable to overwrite existing file. No .csv file written")
			} else {
				utils::write.table(s@results, 
								   file = path, 
								   sep = separator, 
								   col.names = colnames(s@results), 
								   row.names=FALSE, 
								   qmethod = "double", 
								   fileEncoding= encoding)
			}
		} else {
			cli::cli_warn("Destination file already exists. No .csv file written")
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
				cli::cli_warn("Unable to overwrite existing file. No .xlsx file written")
			} else {
				openxlsx::write.xlsx(s@results, file=path, sheetName="data", overwrite=TRUE)
			}
		} else {
			cli::cli_warn("Destination file already exists. No .xlsx file written")
		}
	}
}
