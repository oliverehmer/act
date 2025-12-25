#' Merge .docx files in a folder 
#'
#' Loads all .docx files in a folder and merges them to a singe .docx file.
#' 
#' 
#' @param folderInput Character string; Path to a existing folder containing the .docx files
#' @param pathTemplateInput Character string; Path to .docx file used as a template, where the other files will be inserted.
#' @param folderOutput Character string; Optional. Output path were to save result. If parameter is not set, the print transcripts will only be returned.
#' @param folderOutput Logical;  if \code{TRUE} folderInut will be searched recursively.
#' 
#' @return merged .docx in \link{officeR} format,  
#' 
#' @export
#' 
#' @seealso \code{search_cuts_printtranscript}, \link{export_docx}
#' 
#' @example inst/examples/search_cuts_printtranscript.R
#'
#'
#==== FUNCTIONS ====
merge_docx <- function(folderInput, 
					   pathTemplateInput, 
					   pathOutput=NULL, 
					   recursive=TRUE) {
	if (1==2) {
		folderInput        <- mergefolder
		pathTemplateInput  <- template_path
		pathOutput         <- file.path(mergefolder, "merged_transcripts.docx")
		recursive          <- TRUE
	}
	
	
	#==== CHECKS ====
	if (!dir.exists(folderInput)) {
		stop("Input folder  does not exist. Modify parameter 'folderInput'.")
	}
	if (!file.exists(pathTemplateInput)){
		stop("Input template does not exist. Modify parameter 'pathTemplateInput'.")
	}	
	
	# Find all .docx files in folder and sub folders
	word_files <- fs::dir_ls(path = folderInput, recurse = recursive, regexp = "\\.docx$", type = "file")
	# Stop execution if no files found
	if (length(word_files) == 0) {
		return ("No .docx files found!")
	}
	
	# Create a new empty Word document
	#destination_docx <- read_docx()
	destination_docx <- officer::read_docx(path = pathTemplateInput)
	
	# Loop through all found Word files
	act::helper_progress_set("Merging transcripts", length(word_files))
	for (i in seq_along(word_files)) {
		act::helper_progress_tick()
		
		# load for testing
		#temp_doc <- officer::read_docx(word_files[i])
		
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