#' Helper: Makes valid names for all transcripts in a corpus
#' 
#' Makes valid names for all transcript objects in a corpus object based on the names passed in 'transcriptNames' parameter.
#' In particular, the functions also corrects names, which have to be non-empty and unique.
#' The following options are performed in the mentioned order.
#' 
#' @param transcriptNames Vector of character strings; Names of the transcripts to validate.
#' @param searchPatterns Vector of character strings; Search pattern as regular expression. Leave empty for no search-replace in the names.
#' @param searchReplacements Vector of character strings; Replacements for search. Leave empty for no search-replace in the names.
#' @param toUpperCase Logical; Convert transcript names all to upper case.
#' @param toLowerCase Logical; Convert transcript names all to lower case.
#' @param trim Logical; Remove leading and trailing spaces in names.
#' @param defaultForEmptyNames Character string; Default value for empty transcript names (e.g., resulting from search-replace operations)
#'
#' @return List
#' @export
#'
#' @example inst/examples/helper_transcriptNames_make.R
#' 
helper_transcriptNames_make <-function(transcriptNames,
									   searchPatterns       = character(),
									   searchReplacements   = character(),
									   toUpperCase          = FALSE,
									   toLowerCase          = FALSE,
									   trim                 = FALSE,
									   defaultForEmptyNames = "no_name"
									   ) {
	
	if (is.null(searchPatterns)) {
		searchPatterns <- character()
	}
	if (is.null(searchReplacements)) {
		searchReplacements <- character()
	}

	#--- let's start
	names.original.ids  <- transcriptNames
	names.ok.ids <- transcriptNames
	
	#---search replace
	if (length(searchPatterns)>0) {
		if(length(searchPatterns)!=length(searchReplacements)) {
			warning("The patterns 'searchPatterns' and 'searchReplacements' are not of the same length. No replacements made.")
		} else {
			names(searchReplacements) <- searchPatterns
			for (i in 1:length(searchReplacements)) {
				names.ok.ids <-  stringr::str_replace_all(string=names.ok.ids, pattern=searchPatterns[i], replacement=searchReplacements[i])
			}
		}
	}	
	
	#--- upper case
	if (toUpperCase) {
		names.ok.ids <- stringr::str_to_upper(names.ok.ids)
	}
	
	#--- lower case
	if (toLowerCase) {
		names.ok.ids <- stringr::str_to_lower(names.ok.ids)
	}
	
	#--- trim
	if (trim) {
		names.ok.ids <- stringr::str_trim(names.ok.ids)
	}
	
	#--- empty names
	empty.ids <- which(names.ok.ids=="")
	if (length(empty.ids)>0) {
		names.ok.ids[names.ok.ids==""] <- defaultForEmptyNames[1]
	}
	
	#--- duplicates
	duplicated.ids <- which(duplicated(names.original.ids))
	names.ok.ids <- make.unique(names.ok.ids)
	
	#---results
	result <-list(
		names.ok.ids=names.ok.ids,
		names.original.ids=names.original.ids,
		names.modified.ids=setdiff(names.ok.ids, names.original.ids),
		modified=!identical(names.ok.ids, names.original.ids),
		empty.ids=empty.ids,
		duplicated.ids=duplicated.ids)
	
	return(result)
}