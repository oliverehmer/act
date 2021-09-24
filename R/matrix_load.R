#' Load replacement matrix
#'
#' This function is only for checking how the normalization matrix will be loaded internally.
#'
#' @param path Character string; path to the replacement matrix (a CSV file). If argument is left open, the default replacement matrix of the package will be returned.
#' @param myFileEncoding Character string; encoding of the file.
#'
#' @return Data.frame
#' @export
#'
#' @example inst/examples/matrix_load.R
#' 
matrix_load <- function(path=NULL, 
						myFileEncoding="UTF-8") {
	
	if (is.null(path)) {
		myFileEncoding="UTF-8"
		path <- system.file("extdata", "normalization", "normalizationMatrix.csv", package="act")
		if (!file.exists(path)) {
			stop("The has been a problem locating the default 'normalization matrix'. Please install the act package again.")
		}
	}

	if (file.exists(path)==FALSE) 	{			stop("Path to normalization CSV does not exist")		}

	temp <- utils::read.table(path, header = TRUE, sep = ";", fileEncoding = myFileEncoding, encoding=myFileEncoding )
	
	if ("search" %in% colnames(temp)==FALSE) 	{
		stop("Column 'search' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")
	}
	if ("replace" %in% colnames(temp)==FALSE) 	{
		stop("Column 'replace' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")
	}
	
	attr(temp, 'path') <- path
	
	if (nrow(temp)==0) {
		warning("Replacement matrix does not contain any rows.")
	}
	
	return(temp)
}