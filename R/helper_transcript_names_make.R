#' Helper: Makes valid names for all transcripts in a corpus
#' 
#' Makes valid names for all transcript objects in a corpus object based on the names passed in 'transcriptNames' parameter.
#' In particular, the functions also corrects names, which have to be non-empty and unique.
#' The following options are performed in the mentioned order.
#' 
#' @param transcriptNames Vector of character strings; Names of the transcripts to validate.
#' @param extractPatterns Vector of character strings; Extract pattern as regular expression. Leave empty for no search-replace in the names.
#' @param searchPatterns Vector of character strings; Search pattern as regular expression. Leave empty for no search-replace in the names.
#' @param searchReplacements Vector of character strings; Replacements for search. Leave empty for no search-replace in the names.
#' @param toUpper Logical; Convert transcript names all to upper case.
#' @param toLower Logical; Convert transcript names all to lower case.
#' @param trim Logical; Remove leading and trailing spaces in names.
#' @param defaultEmpty Character string; Default value for empty transcript names (e.g., resulting from search-replace operations)
#'
#' @return List
#' @export
#'
#' @example inst/examples/helper_transcript_names_make.R
#' 
helper_transcript_names_make <- function(transcriptNames,
									   extractPatterns      = NULL,
									   searchPatterns       = NULL,
									   searchReplacements   = NULL,
									   toUpper          = FALSE,
									   toLower          = FALSE,
									   trim                 = FALSE,
									   defaultEmpty = "no_name"
									   ) {
	
#test with settings from corpus x
	#													  transcriptNames           <- transcriptNames
	#													  extractPatterns           <- x@import.names.modify$extractPatterns
	#													  searchPatterns            <- x@import.names.modify$searchPatterns
	#													  searchReplacements        <- x@import.names.modify$searchReplacements
	#													  toUpper               <- x@import.names.modify$toUpper
	#													  toLower               <- x@import.names.modify$toLower
	#													  trim                      <- x@import.names.modify$trim
	#													  defaultEmpty      <- x@import.names.modify$defaultEmpty

#test with own settings
	#transcriptNames <- transcriptNames
	#searchPatterns       <- "_*\\d\\d\\d\\d[-_ ]\\d\\d[-_ ]\\d\\d.*"
	#searchReplacements   <- ""
	#toUpper          <- FALSE
	#toLower          <- FALSE
	#trim                 <- FALSE
	#defaultEmpty <- "no_name"

	
	#--- let's start
	names.original.ids  <- transcriptNames
	names.ok.ids        <- transcriptNames

	
	#---search extract: names.ok.ids
	if (!is.null(extractPatterns)) {
		if (length(extractPatterns)>0) {
			for (i in 1:length(extractPatterns)) {
				if (extractPatterns[[i]]=="") {
					
				} else {
					names.ok.ids <-  stringr::str_extract(string=names.ok.ids, pattern=extractPatterns[[i]])
				}
			}
		}
	}	

	#---search replace: names.ok.ids
	if (!is.null(searchPatterns)) {
		if (is.null(searchReplacements)) {
			stop("Replacement pattern is missing.")
		}
		
		if (length(searchPatterns)>0) {
			
			if(length(searchPatterns)!=length(searchReplacements)) {
				warning("The patterns 'searchPatterns' and 'searchReplacements' are not of the same length. No replacements made.")
			} else {
				names(searchReplacements) <- searchPatterns
				for (i in 1:length(searchReplacements)) {
					if (searchPatterns[[i]]=="") {
						
					} else {
						names.ok.ids <-  stringr::str_replace_all(string=names.ok.ids, pattern=searchPatterns[i], replacement=searchReplacements[i])
					}
				}
			}
		}
	}	
	
	#--- upper case
	if (toUpper) {
		names.ok.ids <- stringr::str_to_upper(names.ok.ids)
	}
	
	#--- lower case
	if (toLower) {
		names.ok.ids <- stringr::str_to_lower(names.ok.ids)
	}

	#--- trim
	if (trim) {
		names.ok.ids <- stringr::str_trim(names.ok.ids)
	}
	
	#--- empty names
	empty.ids <- which(names.ok.ids=="")
	if (length(empty.ids)>0) {
		names.ok.ids[names.ok.ids==""] <- defaultEmpty[1]
	}
	
	#--- duplicates
	duplicated.ids <- which(duplicated(names.ok.ids))
	names.ok.ids   <- make.unique(names.ok.ids)
	#---results
	result <- list(
		names.ok.ids       = names.ok.ids,
		names.original.ids = names.original.ids,
		names.modified.ids = setdiff(names.ok.ids, names.original.ids),
		modified           = !identical(names.ok.ids, names.original.ids),
		empty.ids          = empty.ids,
		duplicated.ids     = duplicated.ids)

	return(result)
}
