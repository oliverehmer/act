#' Create a new corpus object
#'
#' Create a new corpus object and loads annotation files. Currently 'ELAN' .eaf, 'EXMARaLDA .exb and 'Praat' .TextGrid files are supported.
#'  
#' The parameter \code{pathsAnnotationFiles} defines where the annotation files are located.
#' If \code{skipDoubleFiles=TRUE} duplicated files will be skipped, otherwise the will be renamed.
#' If \code{importFiles=TRUE} the corpus object will be created but files will not be loaded. To load the files then call \link{corpus_import}.
#' 
#' The parameter \code{pathsMediaFiles} defines where the corresponding media files are located.
#' If \code{assignMedia=TRUE} the paths defined in \code{x@paths.media.files} will be scanned for media files and will be matched to the transcript object based on their names.
#' Only the the file types set in \code{options()$act.fileformats.audio} and \code{options()$act.fileformats.video} will be recognized. 
#' You can modify these options to recognize other media types.
#'
#' See \code{@import.results} of the corpus object to check the results of importing the files.
#' To get a detailed overview of the corpus object use \code{act::info(x)}, for a summary use \code{act::info_summarized(x)}.
#'
#' @param pathsAnnotationFiles Vector of character strings; paths to annotations files or folders that contain annotation files.
#' @param pathsMediaFiles Vector of character strings; paths to media files or folders that contain media files.
#' @param name Character string; name of the corpus to be created.
#' @param importFiles Logical; if \code{TRUE} annotation files will be imported immediately when the function is called, if \code{FALSE} corpus object will be created without importing the annotation files.
#' @param skipDoubleFiles Logical; if \code{TRUE} transcripts with the same names will be skipped (only one of them will be added), if \code{FALSE} transcripts will be renamed to make the names unique. 
#' @param createFullText Logical; if \code{TRUE} full text will be created.
#' @param assignMedia Logical; if \code{TRUE} the folder(s) specified in \code{@paths.media.files} of your corpus object will be scanned for media.
#' @param pathNormalizationMatrix Character string; path to the replacement matrix used for normalizing the annotations; if argument left open, the default normalization matrix of the package will be used.  
#' @param namesSearchPatterns Vector of character strings; Search pattern as regular expression. Leave empty for no search-replace in the names.
#' @param namesSearchReplacements Vector of character strings; Replacements for search. Leave empty for no search-replace in the names.
#' @param namesToUpperCase Logical; Convert transcript names all to upper case.
#' @param namesToLowerCase Logical; Convert transcript names all to lower case.
#' @param namesTrim Logical; Remove leading and trailing spaces in names.
#' @param namesDefaultForEmptyNames Character string; Default value for empty transcript names (e.g., resulting from search-replace operations)
#'
#' @return Corpus object.
#' 
#' @seealso \link{corpus_import}, \link{examplecorpus}
#' 
#' @export
#'
#' @example inst/examples/corpus_new.R
#' 
#' 
corpus_new <- function(pathsAnnotationFiles, 
					   pathsMediaFiles           = NULL, 
					   name                      = "New Corpus", 
					   importFiles               = TRUE, 
					   skipDoubleFiles           = TRUE, 
					   createFullText            = TRUE, 
					   assignMedia               = TRUE,
					   pathNormalizationMatrix   = NULL,
					   namesSearchPatterns       = character(),
					   namesSearchReplacements   = character(),
					   namesToUpperCase          = FALSE,
					   namesToLowerCase          = FALSE,
					   namesTrim                 = TRUE,
					   namesDefaultForEmptyNames = "no_name"
) {
	
	if (missing(pathsAnnotationFiles)) {
		stop("No input files or folder(s) specified in parameter 'pathsAnnotationFiles'")
	}
	#--- create a new corpus
	x <- methods::new("corpus")
	
	#--- assign the parameters
	x@name                                  <- name
	x@import.skip.double.files              <- skipDoubleFiles
	x@paths.annotation.files                <- gsub("/*$", "", pathsAnnotationFiles, perl=TRUE)
	x@import.modify.transcript.names        <- list(  searchPatterns       = namesSearchPatterns,
													  searchReplacements   = namesSearchReplacements,
													  toUpperCase          = namesToUpperCase,
													  toLowerCase          = namesToLowerCase,
													  trim                 = namesTrim,
													  defaultForEmptyNames = namesDefaultForEmptyNames)
	#media paths
	if (!missing(pathsMediaFiles)) {
		if (!is.null(pathsMediaFiles)) {
			x@paths.media.files <- gsub("/*$", "", pathsMediaFiles, perl=TRUE)
		}
	}
	#normalization matrix
	act_replacementMatrix <- act::matrix_load(pathNormalizationMatrix, "UTF-8")
	if (is.null(act_replacementMatrix))  {	stop("Normalization matrix not read.")		}
	x@normalization.matrix <- act_replacementMatrix
	
	#--- update history
	x@history <- list(
		list(modification  ="corpus_new",
			 systime       = Sys.time()
		)
	)
	
	#--- if media files will not be assigned: check
	if (!assignMedia) {
		paths <- x@paths.media.files
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- sprintf("%s of %s media path(s) in 'x@paths.media.files' do not exist.", length(paths.dont.exist), length(x@paths.media.files))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			warning(message)
		}
	}
	
	#--- if files are not imported: check
	if (!importFiles) {
		paths <- x@paths.annotation.files
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- sprintf("%s of %s path(s) in 'x@paths.annotation.files' do not exist.", length(paths.dont.exist), length(x@paths.annotation.files))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			warning(message)
		}
	}
	
	#--- import files
	if (importFiles) {
		return(act::corpus_import(x=x, createFullText=createFullText, assignMedia=assignMedia))
	} else {
		return(x)
	}
}
