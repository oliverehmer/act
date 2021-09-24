#' Save replacement matrix
#'
#' @param replacementMatrix Data frame; replacement matrix.
#' @param path Character string; path where the matrix will be saved.
#' @param myFileEncoding Character string; encoding of the file.
#'
#' @return nothing
#' @export
#'
#' @example inst/examples/matrix_save.R
#' 
matrix_save <- function(replacementMatrix, 
						path, 
						myFileEncoding="UTF-8") {
	
	if (exists("replacementMatrix")==FALSE)	{	stop("Object 'replacementMatrix' does not exist")	}
	utils::write.table(replacementMatrix, file = path, sep = ";",  row.names=FALSE, qmethod = "double", fileEncoding= myFileEncoding)
}


