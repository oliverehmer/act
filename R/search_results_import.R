#' Import search results 
#' 
#' Search results will be imported from an Excel '.xlsx' file or a comma separated values '.csv' file into a search object.
#'
#' @param path Character string; path to file from where data will be loaded.
#' @param revertReplacements Logical, when exporting search results from act, '=' at the beginning of lines are replaced by '.=", and in numbers the decimal separator '.' is replaced by a ",". If \code{TRUE}, this replacement will be reverted when importing search results.
#' @param sheetName Character string, set the name of the excel sheet containing the data.
#' @param encoding Character string; text encoding in the case of CVS files.
#' @param separator Character; single character that is used to separate the columns in CSV files.
#'
#' @return Search object.
#' 
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_import <- function(path, 
								  revertReplacements=TRUE,
								  sheetName="data", 
								  encoding="UTF-8", 
								  separator=";") {
	
	filetype <- tools::file_ext(path)
	if (filetype=="csv") {
		#temp <- utils::read.table(path, header = TRUE, sep = separator, fileEncoding = encoding, encoding=encoding )

		temp <- suppressWarnings(utils::read.csv( path,
												  header = TRUE, 
												  sep = separator, 
												  fileEncoding = encoding, 
												  encoding=encoding ))
		

			
		if (is.null(temp$resultID)==FALSE){
			rownames(temp)<- temp$resultID
		}
	} else {
		temp <- openxlsx::read.xlsx(xlsxFile=path, sheet=sheetName)
	}
	
	# Normalize column names: both new snake_case exports and old legacy formats
	legacy_names <- c(
		# new snake_case export format -> internal
		"result_id"              = "resultID",
		"transcript_name"        = "transcriptName",
		"annotation_id"          = "annotationID",
		"tier_name"              = "tierName",
		"search_mode"            = "searchMode",
		"content_norm"           = "content.norm",
		"hit_nr"                 = "hit.nr",
		"hit_length"             = "hit.length",
		"hit_pos_content"        = "hit.pos.content",
		"hit_pos_fulltext"       = "hit.pos.fulltext",
		"hit_span"               = "hit.span",
		"stills_values"          = "stills.values",
		"stills_folder"          = "stills.folder",
		"char_orig_bytime_start" = "char.orig.bytime.start",
		"char_orig_bytime_end"   = "char.orig.bytime.end",
		"char_norm_bytime_start" = "char.norm.bytime.start",
		"char_norm_bytime_end"   = "char.norm.bytime.end",
		"char_orig_bytier_start" = "char.orig.bytier.start",
		"char_orig_bytier_end"   = "char.orig.bytier.end",
		"char_norm_bytier_start" = "char.norm.bytier.start",
		"char_norm_bytier_end"   = "char.norm.bytier.end",
		"conc_left_1"            = "concLeft1",
		"conc_left_2"            = "concLeft2",
		"conc_hit"               = "concHit",
		"conc_right_1"           = "concRight1",
		"conc_right_2"           = "concRight2",
		"nr_words_left"          = "nrWordsLeft",
		"nr_words_hit_position"  = "nrWordsHitPosition",
		"nr_words_hit"           = "nrWordsHit",
		"nr_words_right"         = "nrWordsRight",
		"nr_words_total"         = "nrWordsTotal",
		# old legacy formats -> internal
		"recording"              = "transcriptName",
		"transcript.name"        = "transcriptName",
		"transcript"             = "printtranscript",
		"tier.name"              = "tierName",
		"startSec"               = "startsec",
		"endSec"                 = "endsec",
		"search.mode"            = "searchMode"
	)
	for (old in names(legacy_names)) {
		idx <- which(colnames(temp) == old)
		if (length(idx) > 0 && !legacy_names[[old]] %in% colnames(temp)) {
			colnames(temp)[idx] <- legacy_names[[old]]
		}
	}

	# Drop redundant 'transcript' column when 'printtranscript' already exists
	# (produced by old export bug that wrote both columns simultaneously)
	if ("transcript" %in% colnames(temp) && "printtranscript" %in% colnames(temp)) {
		temp <- temp[, colnames(temp) != "transcript", drop = FALSE]
	}

	#check colnames
	necessarycolnames <- c("resultID", "transcriptName", "annotationID",  "tierName", "startsec", "endsec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "searchMode", "hit.span")
	mycolnames <- colnames(temp)
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames) > 0) {
		cli::cli_abort("Some necessary columns are missing in your input file. Missing columns: {.val {missingcolnames}}")
	}
	
	if(revertReplacements) {
		#replace .  by , in numbers
		temp$startsec	<-	gsub(",", "\\.", temp$startsec)
		temp$endsec		<-	gsub(",", "\\.", temp$endsec) 
		
		#replace = at he beginning of cells
		searchString <-"^\\'="
		replacementString <- "="
		temp$content		<-	stringr::str_replace_all(temp$content, searchString, replacementString )
		temp$content.norm	<-	stringr::str_replace_all(temp$content.norm,searchString, replacementString)	
		temp$hit			<-	stringr::str_replace_all(temp$hit, searchString, replacementString )
		temp$concLeft1		<-	stringr::str_replace_all(temp$concLeft1, searchString, replacementString )
		temp$concLeft2		<-	stringr::str_replace_all(temp$concLeft2, searchString, replacementString )
		temp$concHit		<-	stringr::str_replace_all(temp$concHit, searchString, replacementString )
		temp$concRight1		<-	stringr::str_replace_all(temp$concRight1, searchString, replacementString )
		temp$concRight2		<-	stringr::str_replace_all(temp$concRight2, searchString, replacementString )
		if ("printtranscript" %in% colnames(temp)) {
			temp$printtranscript <- stringr::str_replace_all(temp$printtranscript, searchString, replacementString)
		}
	}
	
	#turn factors into vectors
	fctr.cols 					<- sapply(temp, is.factor)
	temp[, fctr.cols] 			<- sapply(temp[, fctr.cols], as.character)
	temp[is.na(temp)]			<- " "
	
	temp$startsec				<- as.double(temp$startsec)
	temp$endsec					<- as.double(temp$endsec)
	
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
	#s@filter.section.startsec                  <- if(!is.na(startsec)) {if(!is.null(startsec)) {startsec}} else {s@filter.section.startsec}
	#s@filter.section.endsec                    <- if(!is.na(endsec))   {if(!is.null(endsec))   {endsec}}   else {s@filter.section.endsec}
	#s@filter.tier.includeRegEx      <- ""
	#s@filter.tier.excludeRegEx      <- ""
	#s@filter.transcript.includeRegEx  <- if(!is.na(filterTranscriptIncludeRegex))   {if(!is.null(filterTranscriptIncludeRegex))   {filterTranscriptIncludeRegex}}   else {s@filter.transcript.includeRegEx }
	#s@filter.transcript.excludeRegEx  <- if(!is.na(filterTranscriptExcludeRegex))   {if(!is.null(filterTranscriptExcludeRegex))   {filterTranscriptExcludeRegex}}   else {s@filter.transcript.excludeRegEx }
	s@results <- temp
	
	return(s)
}
