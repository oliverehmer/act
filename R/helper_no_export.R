# Detect current operating system
#
.detect_os <- function(){
	sysinf <- Sys.info()
	if (!is.null(sysinf)){
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "macos"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "macos"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}



# Make names for search results
#
# @param search.results Data frame; data frame containing search results.
# @param resultidPrefix Character string; prefix for the name of the consecutively numbered search results.
# @param resultidStart Integer; start number of results 
# @return Vector of character strings; names created for the search results.
# @export
#
# @examples
# library(act)

# # Search 
# myRegEx <- "yo"
# searchresults <- act::search_corpus(examplecorpus, pattern=myRegEx, concordanceMake=FALSE)
# 
# # Make custom names
# mynames <- act::search_names(searchresults, resultidPrefix="yo")
# 
# # Replace old names in search by new names
# searchresults$resultID <- mynames
# @keywords internal
.make_names_for_search <- function(search.results, 
									  resultidPrefix = "result",
									  resultidStart  = 1) {
	
	myFormat <- paste(resultidPrefix, "%0", nchar(toString(nrow(search.results)-1+resultidStart)), "d", sep="")
	myNames <- sprintf(myFormat, resultidStart:(nrow(search.results)+resultidStart-1))
	return (myNames)
}



# Gets the path of a .TextGrid for a transcript
#
# Returns either the path to the original .TextGrid file or to a temporary TextGrid created on the fly.
# 
# @param t transcript object; transcript for which you want to get the TextGrid
#
# @return Character string; path to TextGrid file.
#
#@keywords internal# 
# 
# @examples
# print("")

.get_textgrid_for_transcript <- function(t) {
	
	if (missing(t)) 	{cli::cli_abort("Transcript object t is missing.") }	
	
	#=== check in corpus object if textgrid is given and exits
	if (!is.na(t@file.path)) {
		if (file.exists(t@file.path)) {
			if (stringr::str_to_lower(tools::file_ext(t@file.path))=="textgrid") {
				return(t@file.path)
			}
		}
	}
	
	#=== create temporary textgrid
	path <- file.path(tempdir(), stringr::str_c(t@name, ".TextGrid", collapse=""))
	act::export_textgrid(t, path)
	cli::cli_warn("Original TextGrid has not been found. A temporary TextGrid has been created")
	return(path)
}

.validate_resource <- function(loaded_path, package_path) {
	if (!file.exists(package_path)) return(TRUE)
	if (!file.exists(loaded_path)) return(TRUE)

	helper_read_cols <- function(path) {
		ext <- tolower(tools::file_ext(path))
		if (ext == "xlsx") {
			wb <- openxlsx2::wb_load(path)
			sheets <- wb$sheet_names
			result <- list()
			for (s in sheets) {
				result[[s]] <- colnames(openxlsx2::read_xlsx(path, sheet = s))
			}
			return(result)
		} else if (ext == "csv") {
			cols <- colnames(utils::read.table(path, header = TRUE, sep = ";", nrows = 1))
			return(list("Sheet1" = cols))
		}
		return(NULL)
	}

	tryCatch({
		pkg_cols <- helper_read_cols(package_path)
		loaded_cols <- helper_read_cols(loaded_path)

		if (is.null(pkg_cols) || is.null(loaded_cols)) return(TRUE)

		issues <- character()
		for (s in names(pkg_cols)) {
			matching_sheet <- if (s %in% names(loaded_cols)) s else if (length(loaded_cols) == 1) names(loaded_cols)[1] else NULL
			if (is.null(matching_sheet)) {
				issues <- c(issues, paste0('Sheet "', s, '": missing entirely'))
				next
			}
			missing <- setdiff(pkg_cols[[s]], loaded_cols[[matching_sheet]])
			if (length(missing) > 0) {
				issues <- c(issues, paste0('Sheet "', s, '": missing column(s): ', paste(missing, collapse = ", ")))
			}
		}

		if (length(issues) > 0) {
			cli::cli_alert_warning("Resource validation failed: {.path {loaded_path}}")
			for (issue in issues) {
				cli::cli_text("  {issue}")
			}
			cli::cli_alert_info("  Falling back to package version.")
			return(FALSE)
		}
		return(TRUE)
	}, error = function(e) {
		cli::cli_alert_warning("Resource validation error: {e$message}")
		return(TRUE)
	})
}

.strip_invalid_xml_chars <- function(x) {
	gsub("[\x01-\x08\x0b\x0c\x0e-\x1f\x7f]", "", x)
}

.test_read <- function(input_path,
							 testencoding, 
							 testlinenrs) {
	
	#assign("last.warning", NULL, envir = baseenv())
	input_path <- toString(input_path)
	tryCatch(
		{
			myCon <- file(input_path, encoding = testencoding)
			myLines <- readLines(myCon, n = testlinenrs)
			close(myCon)
			
			if(	sum(stringr::str_length(myLines))==0) {
				return("error")
			}
			return (myLines)
		},
		
		error = function(c)
		{
			close(myCon)
			return("error")
		},
		
		warning = function(c)
		{
			close(myCon)
			return("error")
			#paste("warning:", warnings())
		},
		message = function(c)
		{
			return("error")
			close(myCon)
			#"message"
		}
	)
}



