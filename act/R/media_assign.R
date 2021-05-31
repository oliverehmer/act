#' Assign media file links to transcript objects
#'
#' Searches for media files in folders and assigns the links to transcript objects in a corpus. 
#' The function uses the name of the transcript to find the media files, 
#' e.g. the function assumes that the annotation files have the same name as the media files, except from the suffix/the file type.
#'
#' Only the the file types set in \code{options()$act.fileformats.audio} and \code{options()$act.fileformats.video} will be recognized. 
#' You can modify these options to recognize other media types.
#'
#' @param x Corpus object.
#' @param searchPaths Vector of character strings; paths where media files should be searched; if path is not defined, the paths given in \code{x@paths.media.files} will be used).
#' @param searchInSubfolders Logical; if \code{FALSE} only the main level of the directory will be scanned for media, if \code{TRUE} sub folders will be scanned for media, too.
#' @param filterFile Character string; Regular expression of files to look for. 
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#' @param deleteExistingMedia Logical; if \code{TRUE} existing media links will be deleted, if \code{FALSE} existing media links will be preserved and new links will be added.
#' @param onlyUniqueFiles Logical; if \code{TRUE} media files with the same name (in different locations) will only be added once; if \code{FALSE} all media files found will be added, irrespective of possible doublets.
#'
#'
#' @return Corpus object.
#' 
#' @seealso \link{media_delete}, \link{media_getPathToExistingFile}
#' 
#' @export
#'
#' @example inst/examples/media_assign.R
#' 
media_assign <- function(x, 
						 searchPaths        = NULL, 
						 searchInSubfolders = TRUE, 
						 filterFile         = "",
						 transcriptNames    = NULL, 
						 deleteExistingMedia= TRUE, 
						 onlyUniqueFiles    = TRUE) {
	
	#searchPaths        <- NULL 
	#searchInSubfolders <- TRUE 
	#filterFile         <- ""
	#transcriptNames    <- NULL 
	#deleteExistingMedia<- TRUE 
	#onlyUniqueFiles    <- TRUE
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }

	message <- c()

	if (is.null(searchPaths)) {
		paths <- x@paths.media.files
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, sprintf("%s of %s media path(s) in 'x@paths.media.files' do not exist.", length(paths.dont.exist), length(x@paths.media.files)))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			
			paths <- paths[-paths.dont.exist]
		}
	} else {
		paths <- searchPaths
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, sprintf("%s of %s media path(s) in the parameter 'searchPaths' do not exist.", length(paths.dont.exist), length(x@paths.media.files)))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			
			paths <- paths[-paths.dont.exist]
		}
	}
	
	#--- if there are no paths
	if (length(paths)==0) {
		message <- c(message, "No valid media paths.")
		message <- paste(message, sep='\n', collapse='\n')
		warning (message)
		return (x)
	} 
	
	
	#--- make list of all file paths
	paths.new <- c()
	for (path in paths) {
		#remove tailing slashes first
		path 	<- gsub("/*$", "", path , perl=TRUE)
		#if it is a directory
		if(dir.exists(path)) {
			#get all files in folders
			paths.sub <- list.files(path, recursive=searchInSubfolders, pattern=filterFile, ignore.case=TRUE,  full.names=TRUE)
			paths.new <- c(paths.new, paths.sub)
		} else {
			#it must be a file
			paths.new <- c(paths.new, path)
		}
	}
	
	#--- if there are no files at all in the folders
	if (length(paths.new)==0) { 
		if (length(message)>0){
			warning(paste(message,sep="\n", collapse="\n"))
		}
		return (x)
	}
	

	#--- get only the media files
	filterFile.media <- c(options()$act.fileformats.audio, options()$act.fileformats.video)
	filterFile.media <- stringr::str_flatten(filterFile.media, collapse="|")
	filterFile.media <- stringr::str_flatten(c("(?i)\\.(", filterFile.media, ")"), collapse="")
	paths.new <- unlist(paths.new[stringr::str_which(string=paths.new, pattern=filterFile.media, )		])
	if (length(paths.new)==0) {
		message<- c(message, "No media files found. Please check 'x@paths.media.files'.")
		warning(paste(message,sep="\n", collapse="\n"))
		return (x)
	}

	
	#--- get names
	file.names <- basename(paths.new)

	#--- if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}

	#--- set progress bar
	helper_progress_set("Assigning media", length(transcriptNames))
	
	#--- run through transcripts in the corpus file
	for (nameTranscript in transcriptNames) {
		#update progress bar
		helper_progress_tick()
		
		#get transcript name
		nameTranscript	<- x@transcripts[[nameTranscript]]@name
		#nameTranscript	<- gsub(" ", "_", nameTranscript)
		search <- paste("^", nameTranscript, sep="")
		myMediaFiles <-	unlist(paths.new[grep(pattern=search, file.names)])
		
		if (onlyUniqueFiles) {
			#get only file name
			myMediaFilenames <- basename(myMediaFiles)
			#select for file paths only unique file names
			myMediaFiles <- myMediaFiles[!duplicated(myMediaFilenames)]
		}
		
		if (deleteExistingMedia) {
			x@transcripts[[nameTranscript]]@media.path <- myMediaFiles
		} else {
			x@transcripts[[nameTranscript]]@media.path <- c(x@transcripts[[nameTranscript]]@media.path, myMediaFiles)
		}
	}
	
	#--- show warnings
	if (length(message)>0){
		warning(paste(message,sep="\n", collapse="\n"))
	}
	
	#--- return corpus object
	return (x)
}