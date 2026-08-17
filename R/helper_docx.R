#' Merge .docx files in a folder
#'
#' Loads all .docx files in a folder and merges them to a singe .docx file.
#'
#'
#' @param folderInput Character string; Path to a existing folder containing the .docx files
#' @param pathTemplateInput Character string; Path to .docx file used as a template, where the other files will be inserted.
#' @param pathOutput Character string; Optional. Output path were to save result. If parameter is not set, the print transcripts will only be returned.
#' @param recursive Logical;  if \code{TRUE} folder input will be searched recursively.
#' @param filterRegex Character string; optional regular expression applied to the matched .docx file paths. Only files matching the expression are included in the merge. Default \code{NULL} (no filtering).
#'
#' @return merged .docx in officer format,
#'
#' @export
#'
#' @seealso \link{export_docx}
#'
#'
#==== FUNCTIONS ====
merge_docx <- function(folderInput,
					   pathTemplateInput,
					   pathOutput=NULL,
					   recursive=TRUE,
					   filterRegex=NULL) {
	if (1==2) {
		folderInput        <- '/Users/oliverehmer/Library/CloudStorage/OneDrive-Persoenlich/Corpus/corpus_work/temporary/oliverehmer/sequences__2026-01-13__20-00-15'
		pathOutput <- "/Users/oliverehmer/Downloads/test.docx"
		pathTemplateInput  <- '/Users/oliverehmer/Library/CloudStorage/OneDrive-Persoenlich/Corpus/corpus_work/word/_templates/word/template_sequences_NO_colors_v15.dotx'
		recursive          <- TRUE
		filterRegex        <- NULL

		folderInput        <- mergefolder
		pathTemplateInput  <- template_path
		pathOutput         <- file.path(mergefolder, "merged_transcripts.docx")
		recursive          <- TRUE
	}


	#==== CHECKS ====
	if (!dir.exists(folderInput)) {
		cli::cli_abort("Input folder does not exist. Modify parameter {.arg folderInput}.")
	}
	if (!file.exists(pathTemplateInput)){
		cli::cli_abort("Input template does not exist. Modify parameter {.arg pathTemplateInput}.")
	}

	# Find all .docx files in folder and sub folders
	word_files <- fs::dir_ls(path = folderInput, recurse = recursive, regexp = "\\.docx$", type = "file", ignore.case = TRUE)
	# Apply optional filter
	if (!is.null(filterRegex) && nzchar(filterRegex)) {
		word_files <- word_files[grepl(filterRegex, word_files)]
	}
	# Stop execution if no files found
	if (length(word_files) == 0) {
		return ("No .docx files found!")
	}
	
	# Create a new empty Word document
	#destination_docx <- read_docx()
	destination_docx <- officer::read_docx(path = pathTemplateInput)
	
	# Loop through all found Word files
	act::helper_progress_set("Merging transcripts", length(word_files))
	i<-1
	for (i in seq_along(word_files)) {
		act::helper_progress_tick()
		
		# load for testing
		#temp_doc <- officer::read_docx(word_files[i])
		#View(temp_doc)
		
		# Append file content to the combined document
		destination_docx <- officer::body_add_docx(destination_docx, src = word_files[i])
		#?officer::body_add_docx
		
		# Add a page break except after the last file
		if (i < length(word_files)) {
			destination_docx <- officer::body_add_break(destination_docx)
		}
	}
	
	# Save the combined document
	if(!is.null(pathOutput)) {
		print(destination_docx, target = pathOutput)		
	}
	return(destination_docx)
}