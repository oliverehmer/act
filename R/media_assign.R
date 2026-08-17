#' Assign media file links to transcript objects
#'
#' Searches for media files in folders and assigns the links to transcript objects in a corpus. 
#' The function uses the name of the transcript to find the media files, 
#' e.g. the function assumes that the annotation files have the same name as the media files, except from the suffix/the file type.
#'
#' Only the the file types set in \code{options()$act.media.fileformats.audio} and \code{options()$act.media.fileformats.video} will be recognized. 
#' You can modify these options to recognize other media types.
#'
#' @param x Corpus object.
#' @param searchPaths Vector of character strings; paths where media files should be searched; if path is not defined, the paths given in \code{x@paths.media.files} will be used).
#' @param searchSubfolders Logical; if \code{FALSE} only the main level of the directory will be scanned for media, if \code{TRUE} sub folders will be scanned for media, too.
#' @param filterFile Character string; Regular expression of files to look for. 
#' @param namesExtractPattern Character string; Regular Expression to match a part of the transcript name to seach for media files.
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#' @param deleteExistingMedia Logical; if \code{TRUE} existing media links will be deleted, if \code{FALSE} existing media links will be preserved and new links will be added.
#' @param onlyUniqueFiles Logical; if \code{TRUE} media files with the same name (in different locations) will only be added once; if \code{FALSE} all media files found will be added, irrespective of possible doublets.
#' @param readMetadata Logical; if \code{TRUE} the read-derived media columns (\code{length.sec}, \code{video.*}, \code{startsec} etc.) are filled via \code{media_metadata_read} after assignment. Default \code{FALSE} (fast; no file reading).
#'
#'
#' @return Corpus object.
#'
#' @seealso \link{media_select}, \link{media_delete}, \link{media_path_to_existing_file}
#' 
#' @export
#'
#' @example inst/examples/media_assign.R
#' 
media_assign <- function(x,
						 searchPaths                 = NULL,
						 searchSubfolders            = TRUE,
						 filterFile                  = "",
						 namesExtractPattern         = '',
						 transcriptNames             = NULL,
						 deleteExistingMedia         = TRUE,
						 onlyUniqueFiles             = TRUE,
						 readMetadata                = FALSE) {
	# x <- corpus
	# searchPaths        <- NULL
	# searchSubfolders <- TRUE
	# filterFile         <- ""
	# filterFile         <- "mp4"
	# transcriptNames    <- NULL
	# deleteExistingMedia<- TRUE
	# onlyUniqueFiles    <- TRUE
	

	#					   filterFile          <- ""
	#					   namesExtractPattern <- '[a-zA-Z]+_[a-zA-Z]*[0-9]+'

	.assert_corpus(x, missing = missing(x))
	
	message <- c()

	if (is.null(namesExtractPattern)) {
		namesExtractPattern <- character()
	}
	
	if (is.null(searchPaths)) {
		paths <- x@paths.media.files
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, .media_missing_paths_summary(paths[paths.dont.exist]))
			paths <- paths[-paths.dont.exist]
		}
	} else {
		paths <- searchPaths
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, .media_missing_paths_summary(paths[paths.dont.exist]))
			paths <- paths[-paths.dont.exist]
		}
	}
	
	#--- if there are no paths
#	if (length(paths)==0) {
#		message <- c(message, "No valid media paths.")
#		message <- paste(message, sep='\n', collapse='\n')
#		warning (message)
#		return (x)
#	} 
	
	#--- make list of all file paths
	paths.new <- c()
	for (path in paths) {
		#remove tailing slashes first
		path 	<- gsub("/*$", "", path , perl=TRUE)
		#if it is a directory
		if(dir.exists(path)) {
			#get all files in folders
			paths.sub <- list.files(path, 
									recursive=searchSubfolders, 
									pattern=filterFile, 
									ignore.case=TRUE,  
									full.names=TRUE)
			paths.new <- c(paths.new, paths.sub)
		} else {
			#it must be a file
			paths.new <- c(paths.new, path)
		}
	}
	
	#--- if there are no files at all in the folders
	if (length(paths.new)==0) { 
		if (length(message)>0){
			cli::cli_warn(unique(message))
		}
		return (x)
	}
	
	#--- get only the media files
	filterFile.media <- c(options()$act.media.fileformats.audio, options()$act.media.fileformats.video)
	filterFile.media <- stringr::str_flatten(filterFile.media, collapse="|")
	filterFile.media <- stringr::str_flatten(c("(?i)\\.(", filterFile.media, ")"), collapse="")
	paths.new <- unlist(paths.new[stringr::str_which(string=paths.new, pattern=filterFile.media, )		])
	if (length(paths.new)==0) {
		message<- c(message, "No media files found. Please check 'x@paths.media.files'.")
		cli::cli_warn(unique(message))
		return (x)
	}

	#--- get names
	file.names <- basename(paths.new)

	#--- if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}

	#--- pre-index media files by extracted ID (O(1) lookup per transcript
	#    instead of O(N) grep, avoids quadratic blow-up on large corpora)
	paths_by_id <- NULL
	if (nzchar(namesExtractPattern)) {
		media_ids   <- stringr::str_to_lower(stringr::str_extract(file.names, namesExtractPattern))
		paths_by_id <- split(paths.new, media_ids)
	}

	#--- set progress bar
#	if (exists('helper_progress_set')) {
	if (!exists("pb", envir = act.environment)) {
		act.environment$pb <- NULL
	}

	helper_progress_set("Assigning media", length(transcriptNames))
#	}

	#--- run through transcripts in the corpus file
	for (nameTranscript in transcriptNames) {
		#print(nameTranscript)

		#update progress bar
		#if (exists('helper_progress_tick')) {
			helper_progress_tick()
		#}

		#get transcript name
		nameTranscript	<- x@transcripts[[nameTranscript]]@name
		#nameTranscript	<- gsub(" ", "_", nameTranscript)

		#if no part to extract from transcript name is set
		if (namesExtractPattern=='') {
			#use entire name
			search <- paste("^", nameTranscript, sep="")
			myMediaFiles <- unlist(paths.new[grep(pattern=search, file.names, ignore.case=TRUE)])
		} else {
			#extract that part
			search <- stringr::str_extract(string=nameTranscript, pattern=namesExtractPattern)
			if (is.na(search) || !nzchar(search)) {
				myMediaFiles <- character(0)
			} else {
				myMediaFiles <- paths_by_id[[stringr::str_to_lower(search)]]
				if (is.null(myMediaFiles)) myMediaFiles <- character(0)
			}
		}

		if (onlyUniqueFiles) {
			#select for file paths only unique file names
			myMediaFiles <- myMediaFiles[!duplicated(basename(myMediaFiles))]
		}

		if (deleteExistingMedia) {
			x@transcripts[[nameTranscript]]@media <- media_build(myMediaFiles)
		} else {
			x@transcripts[[nameTranscript]]@media <- rbind(
				x@transcripts[[nameTranscript]]@media,
				media_build(myMediaFiles))
		}
		if (readMetadata) {
			x@transcripts[[nameTranscript]]@media <- media_metadata_fill(x@transcripts[[nameTranscript]]@media)
		}
	}
	
	#--- show warnings
	if (length(message)>0){
		cli::cli_warn(unique(message))
	}
	
	#--- return corpus object
	return (x)
}


# Build compact warning lines for media paths that do not exist, grouped by
# storage medium. On macOS, paths under /Volumes/<name> are grouped by that
# mount; other paths are grouped by their containing folder. Per group the
# message reports only a count (never individual paths): if the group root is
# not present it is reported as a disconnected storage medium, otherwise the
# files are reported as not found.
.media_missing_paths_summary <- function(missing_paths) {
	if (length(missing_paths) == 0) { return(character(0)) }
	vol   <- stringr::str_match(missing_paths, "^(/Volumes/[^/]+)")[, 2]
	roots <- ifelse(is.na(vol), dirname(missing_paths), vol)
	out   <- character(0)
	for (r in unique(roots)) {
		n <- sum(roots == r)
		if (dir.exists(r)) {
			out <- c(out, sprintf("%s: %s file(s) not found.", r, n))
		} else {
			out <- c(out, sprintf("Storage medium not connected: %s (%s file(s) unavailable)", r, n))
		}
	}
	out
}
