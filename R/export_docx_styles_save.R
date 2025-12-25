#' Save styles matrix for .docx transcripts
#'
#' @param exportStyles Data frame; export styles matrix.
#' @param path Character string; path where the matrix will be saved.
#' @param encoding Character string; encoding of the file.
#'
#' @return nothing
#' 
#' @export
#'
#' @example inst/examples/matrix_save.R
#' 
export_docx_styles_save <- function(exportStyles, 
						path, 
						encoding="UTF-8") {
	
	if (exists("exportStyles")==FALSE)	{	stop("Object 'exportStyles' does not exist")	}
	utils::write.table(exportStyles, file = path, sep = ";",  row.names=FALSE, qmethod = "double", fileEncoding= encoding)
}


