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
#' @param createFulltext Logical; if \code{TRUE} full text will be created.
#' @param assignMedia Logical; if \code{TRUE} the folder(s) specified in \code{@paths.media.files} of your corpus object will be scanned for media.
#' @param pathNormalizationMatrix Character string; path to the replacement matrix used for normalizing the annotations; if argument left open, the default normalization matrix of the package will be used.  
#' @param namesInclude Character strings; Only files matching this regular expression will be imported into the corpus.
#' @param namesExclude Character strings; Files matching this regular expression will be skipped and not imported into the corpus.
#' @param namesExtractPatterns Vector of character strings; Only the part of the file name matching these expressions will be taken as trasncript name.
#' @param namesSearchPatterns Vector of character strings; Search pattern as regular expression. Leave empty for no search-replace in the names.
#' @param namesSearchReplacements Vector of character strings; Replacements for search. Leave empty for no search-replace in the names.
#' @param namesToUpper Logical; Convert transcript names all to upper case.
#' @param namesToLower Logical; Convert transcript names all to lower case.
#' @param namesTrim Logical; Remove leading and trailing spaces in names.
#' @param namesDefault Character string; Default value for empty transcript names (e.g., resulting from search-replace operations)
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
corpus_new <- function(pathsAnnotationFiles      = NULL, 
					   pathsMediaFiles           = NULL, 
					   name                      = "New Corpus", 
					   importFiles               = TRUE, 
					   skipDoubleFiles           = TRUE, 
					   createFulltext            = TRUE, 
					   assignMedia               = TRUE,
					   pathNormalizationMatrix   = NULL,
					   namesInclude              = character(),
					   namesExclude              = character(),
					   namesExtractPatterns      = character(),
					   namesSearchPatterns       = character(),
					   namesSearchReplacements   = character(),
					   namesToUpper              = FALSE,
					   namesToLower              = FALSE,
					   namesTrim                 = TRUE,
					   namesDefault              = "no_name"
) {
	
	
	#--- create a new corpus
	x <- methods::new("corpus")
	
	if (missing(namesInclude)) {
		namesInclude <- character()
	}
	if (missing(namesExclude)) {
		namesExclude <- character()
	}
	if (missing(namesSearchPatterns)) {
		namesSearchPatterns <- character()
	}
	if (missing(namesExtractPatterns)) {
		namesExtractPatterns <- character()
	}
	if (missing(namesSearchReplacements)) {
		namesSearchReplacements <- character()
	}

	# if value is set to ""
	if (!length(namesInclude)==0) {
		if (namesInclude=='') {
			namesInclude <- character()
		}
	}
	if (!length(namesExclude)==0) {
		if (namesExclude=='') {
			namesExclude <- character()
		}
	}
	

	
	#--- assign the parameters
	x@name                       <- name
	x@import.skip.double.files   <- skipDoubleFiles
	x@import.names.include       <- namesInclude
	x@import.names.exclude       <- namesExclude
	x@paths.annotation.files     <- gsub("/*$", "", pathsAnnotationFiles, perl=TRUE)
	x@import.names.modify        <- list(   extractPatterns      = namesExtractPatterns,
											searchPatterns       = namesSearchPatterns,
											searchReplacements   = namesSearchReplacements,
											toUpper          = namesToUpper,
											toLower          = namesToLower,
											trim                 = namesTrim,
											defaultEmpty = namesDefault)
	#media paths
	if (!is.null(pathsMediaFiles)) {
		x@paths.media.files <- gsub("/*$", "", pathsMediaFiles, perl=TRUE)
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
	
	#--- if annotation files are not imported: check
	if (!missing(pathsAnnotationFiles)) {
		if (!importFiles) {
			paths <- x@paths.annotation.files
			paths.dont.exist <- which(!file.exists(paths))
			if (length(paths.dont.exist)>0) {
				message <- sprintf("%s of %s path(s) in 'x@paths.annotation.files' do not exist.", length(paths.dont.exist), length(x@paths.annotation.files))
				m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
				message <- stringr::str_c(message,"\n", m, collapse="\n")
				warning(unique(message))
			}
		}
	}
	
	#--- import files
	if (missing(pathsAnnotationFiles)) {
		return(x)
	}
	
	if (importFiles) {
		return(act::corpus_import(x=x, 
								  createFulltext=createFulltext, 
								  assignMedia=assignMedia))
		
	} else {
		return(x)
	}
}
