#' Load replacement matrix
#'
#' Loads a normalization/replacement matrix from a .csv or .xlsx file.
#'
#' The following placeholders are supported in the `replace` column:
#' - `\{SPACE\}` is replaced by a single space character `" "`
#' - `\{NOTHING\}` is replaced by an empty string `""`
#'
#' Placeholders can be part of a longer string, e.g. `abc\{SPACE\}def`
#' becomes `abc def`.
#'
#' @param path Character string; path to the replacement matrix (.csv or .xlsx file). If argument is left open, the default replacement matrix of the package will be returned.
#' @param encoding Character string; encoding of the file (only used for .csv files).
#'
#' @return Data.frame
#' @export
#'
#' @example inst/examples/matrix_load.R
#'
matrix_load <- function(path=NULL,
						encoding="UTF-8") {

	if (is.null(path)) {
		encoding="UTF-8"
		path <- system.file("extdata", "normalization", "normalization_matrix.csv", package="act")
		if (!file.exists(path)) {
			cli::cli_abort("The has been a problem locating the default 'normalization matrix'. Please install the act package again.")
		}
	}

	if (!file.exists(path)) {
		cli::cli_abort("Path to normalization matrix does not exist: {.path {path}}")
	}

	package_path <- system.file("extdata", "normalization", "normalization_matrix.csv", package = "act")
	if (path != package_path && file.exists(package_path)) {
		if (!.validate_resource(path, package_path)) {
			path <- package_path
		}
	}

	ext <- tolower(tools::file_ext(path))
	if (ext == "xlsx") {
		temp <- openxlsx2::read_xlsx(path, sheet = 1)
		temp <- as.data.frame(temp, stringsAsFactors = FALSE)
	} else {
		temp <- utils::read.table(path, header = TRUE, sep = ";", fileEncoding = encoding, encoding = encoding)
	}

	if (!"search" %in% colnames(temp)) {
		cli::cli_abort("Column {.arg search} is missing in normalization matrix. File needs to contain columns {.arg search} and {.arg replace}")
	}
	if (!"replace" %in% colnames(temp)) {
		cli::cli_abort("Column {.arg replace} is missing in normalization matrix. File needs to contain columns {.arg search} and {.arg replace}")
	}

	temp$replace[is.na(temp$replace)] <- ""
	temp$replace <- stringr::str_replace_all(temp$replace, "\\{SPACE\\}", " ")
	temp$replace <- stringr::str_replace_all(temp$replace, "\\{NOTHING\\}", "")

	attr(temp, 'path') <- path

	if (nrow(temp)==0) {
		cli::cli_warn("Replacement matrix does not contain any rows.")
	}

	return(temp)
}