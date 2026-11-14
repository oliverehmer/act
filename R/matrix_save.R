#' Save replacement matrix
#'
#' @param replacementMatrix Data frame; replacement matrix.
#' @param path Character string; path where the matrix will be saved. A path ending in \code{.xlsx} is written as an Excel file, any other as csv (separator \code{;}).
#' @param encoding Character string; encoding of the file.
#'
#' @return nothing
#' @export
#'
#' @example inst/examples/matrix_save.R
#' 
matrix_save <- function(replacementMatrix, 
						path, 
						encoding="UTF-8") {
	
	if (exists("replacementMatrix")==FALSE)	{	cli::cli_abort("Object {.arg replacementMatrix} does not exist")	}
	# a path ending in .xlsx is an Excel file (matrix_load() reads it as one; a csv under that name could not be loaded)
	if (tolower(tools::file_ext(path)) == "xlsx") {
		openxlsx2::write_xlsx(x = replacementMatrix, file = path, na.strings = NULL)
		return(invisible(NULL))
	}
	utils::write.table(replacementMatrix, file = path, sep = ";",  row.names=FALSE, qmethod = "double", fileEncoding= encoding)
}


