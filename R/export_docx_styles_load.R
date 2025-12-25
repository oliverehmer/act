#' Load styles matrix for .docx transcripts
#'
#' This function is only for checking how the export styles matrix (as .csv) will be loaded internally.
#'
#' @param path Character string; path to the export styles matrix .csv.  If argument is left open, the default export styles data frame of the package will be returned.
#' @param encoding Character string; encoding of the file.
#' @param path_docx Character string; Path to a template file. If given, it will be checked if all styles defined in exportStylesMatrix are present in the .docx file. if \code{NA} nothing will be checked.
#'
#' @return Data.frame
#' 
#' @export
#'
#' @example inst/examples/matrix_load.R
#' 
export_docx_styles_load <- function(path      = NULL, 
									encoding  = "UTF-8",
									path_docx = NA) {
	if (1==2) {
		path<-NULL
		encoding<-"UTF-8"
		path<-'/Users/oliverehmer/Documents/R/iclo/docx_export_styles_matrix_iclo.csv'
	}
	if (is.null(path)) {
		encoding<-"UTF-8"
		path <- system.file("extdata", "docx", "export_styles_matrix.csv", package="act")
		if (!file.exists(path)) {
			stop("The has been a problem locating the default 'export_styles_matrix.csv'. Please install the act package again.")
		}
	}
	
	
	if (file.exists(path)==FALSE) 	{			stop("Path to the exportStylesMatrix files does not exist")		}
	#old read: problems with unicode character
	#temp <- utils::read.table(path, header = TRUE, sep = ";", 
	#						  fileEncoding = encoding, 
	#						  quote        = "")
	#read csv
	library(readr)
	temp <- read_delim(path, 
					   delim = ";",
					   locale = locale(encoding = encoding),
					   quote = "",
					   col_types = cols(.default = col_character()))
	
	
	
	#==== trim ====
	cols <- c("act.style.name",	"act.style.scope",	"act.style.type", "act.regex",	"docx.template.type",	"docx.template.name",	"comment")
	temp[cols] <- lapply(temp[cols], stringr::str_trim)
	
	
	#==== check ====
	#---- columns ----
	#check if necessary columns are present
	cols_obligatory <- c("act.style.name",
						 "act.style.scope",
						 "act.style.type", 
						 "act.regex",
						 "docx.template.type", 	
						 "docx.template.name")
	
	cols_missing_ids <- which(!cols_obligatory %in% names(temp))
	if (length(cols_missing_ids)!=0) 	{
		stop("The following obligatory columns are missing in your exportStylesMatrix .csv file: ", stringr::str_flatten(cols_obligatory[cols_missing_ids], collapse=", "))
	}
	
	#---- default styles -----
	#check if default styles are present	
	styles_obligatory <- c("header.preface",
						   "header.title",
						   "header.subtitle",
						   "header.info",
						   "body.default")
	styles_missing_ids <- which(!styles_obligatory %in% temp$act.style.name)
	if (length(styles_missing_ids)!=0) 	{
		stop("The following obligatory style definitions are missing in your stylesMatrix .csv file: ", stringr::str_flatten(styles_obligatory[styles_missing_ids], collapse=", "))
	}
	
	#---- all styles ----
	#check if all defined styles are in word docx
	if (!is.null(path_docx)) {
		if (!is.na(path_docx)) {
			doc <- officer::read_docx(path_docx)
			all_styles <- officer::styles_info(doc)
			#View(all_styles)
			
			styles.missing <- which(!temp$docx.template.name %in% all_styles$style_name)
			if (length(styles.missing)>0) {
				stop("The following styles from your stylesMatrix .csv are missing in the .docx file: ", stringr::str_flatten(temp$docx.template.name[styles.missing], collapse=", "))
			}
		}
	}
	
	attr(temp, 'path') <- path
	
	if (nrow(temp)==0) {
		warning("The exportStylesMatrix does not contain any rows.")
	}
	
	return(temp)
}
