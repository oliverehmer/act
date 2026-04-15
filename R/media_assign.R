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
#' @param searchSubfolders Logical; if \code{FALSE} only the main level of the directory will be scanned for media, if \code{TRUE} sub folders will be scanned for media, too.
#' @param filterFile Character string; Regular expression of files to look for. 
#' @param namesExtractPattern Character string; Regular Expression to match a part of the transcript name to seach for media files.
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#' @param deleteExistingMedia Logical; if \code{TRUE} existing media links will be deleted, if \code{FALSE} existing media links will be preserved and new links will be added.
#' @param onlyUniqueFiles Logical; if \code{TRUE} media files with the same name (in different locations) will only be added once; if \code{FALSE} all media files found will be added, irrespective of possible doublets.
#' @param audioAsFallback Logical; if \code{TRUE} audio media files will only be assigned if no video files were found; if \code{FALSE} all media files found will be added, irrespective of type.
#' @param videoMaxFiles Integer or NULL; maximum number of video files to assign per transcript. NULL means no limit.
#' @param audioMaxFiles Integer or NULL; maximum number of audio files to assign per transcript. NULL means no limit.
#' @param videoPriority Character vector or NULL; regex patterns matched against video filenames for prioritization. First matching pattern wins. E.g. \code{c("hand.*_al", "_al", "hand.*_ac", "_ac")}.
#' @param audioPriority Character vector or NULL; file extensions for audio prioritization. First matching extension wins. E.g. \code{c("aif", "wav", "mp3")}.
#'
#'
#' @return Corpus object.
#' 
#' @seealso \link{media_delete}, \link{media_path_to_existing_file}
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
						 audioAsFallback             = TRUE,
						 videoMaxFiles               = NULL,
						 audioMaxFiles               = NULL,
						 videoPriority               = NULL,
						 audioPriority               = NULL) {
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
	#					   audioAsFallback    <- TRUE

	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.") } }
	
	message <- c()

	if (is.null(namesExtractPattern)) {
		namesExtractPattern <- character()
	}
	
	if (is.null(searchPaths)) {
		paths <- x@paths.media.files
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, sprintf("%s of %s media path(s) in 'x@paths.media.files' do(s) not exist.", length(paths.dont.exist), length(x@paths.media.files)))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			
			paths <- paths[-paths.dont.exist]
		}
	} else {
		paths <- searchPaths
		paths.dont.exist <- which(!file.exists(paths))
		if (length(paths.dont.exist)>0) {
			message <- c(message, sprintf("%s of %s media path(s) in the parameter 'searchPaths' do(s) not exist.", length(paths.dont.exist), length(x@paths.media.files)))
			m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
			message <- stringr::str_c(message,"\n", m, collapse="\n")
			
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
	filterFile.media <- c(options()$act.fileformats.audio, options()$act.fileformats.video)
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
			
		} else {
			#extract that part 
			search <- stringr::str_extract(string=nameTranscript, pattern=namesExtractPattern)
			if (is.na(search)) {
				search <- ''
			} 
		}
		
		#get all media files
		myMediaFiles <-	unlist(paths.new[grep(pattern=search, file.names)])

		myMediaFiles.video <- myMediaFiles[tolower(tools::file_ext(myMediaFiles)) %in% tolower(options()$act.fileformats.video)]
		myMediaFiles.audio <- myMediaFiles[tolower(tools::file_ext(myMediaFiles)) %in% tolower(options()$act.fileformats.audio)]

		if (!is.null(videoPriority) && length(myMediaFiles.video) > 0) {
			myMediaFiles.video <- .media_prioritize(myMediaFiles.video, videoPriority, "name")
		}
		if (!is.null(videoMaxFiles) && length(myMediaFiles.video) > videoMaxFiles) {
			myMediaFiles.video <- myMediaFiles.video[seq_len(videoMaxFiles)]
		}

		if (!is.null(audioPriority) && length(myMediaFiles.audio) > 0) {
			myMediaFiles.audio <- .media_prioritize(myMediaFiles.audio, audioPriority, "ext")
		}
		if (!is.null(audioMaxFiles) && length(myMediaFiles.audio) > audioMaxFiles) {
			myMediaFiles.audio <- myMediaFiles.audio[seq_len(audioMaxFiles)]
		}

		if (audioAsFallback) {
			if (length(myMediaFiles.video) > 0) {
				myMediaFiles <- myMediaFiles.video
			} else {
				myMediaFiles <- myMediaFiles.audio
			}
		} else {
			myMediaFiles <- c(myMediaFiles.video, myMediaFiles.audio)
		}
		
		names(myMediaFiles) <- stringr::str_to_lower(
			tools::file_path_sans_ext(basename(myMediaFiles))
		)
		
		if (onlyUniqueFiles) {
			#select for file paths only unique file names
			myMediaFiles <- myMediaFiles[!duplicated(basename(myMediaFiles))]
		}
		
		if (deleteExistingMedia) {
			x@transcripts[[nameTranscript]]@media.path <- myMediaFiles
		} else {
			x@transcripts[[nameTranscript]]@media.path <- c(x@transcripts[[nameTranscript]]@media.path, myMediaFiles)
		}
	}
	
	#--- show warnings
	if (length(message)>0){
		cli::cli_warn(unique(message))
	}
	
	#--- return corpus object
	return (x)
}

.media_prioritize <- function(files, priority, match_by = "name") {
	for (p in priority) {
		if (match_by == "name") {
			matched <- files[stringr::str_detect(basename(files), p)]
		} else {
			matched <- files[tolower(tools::file_ext(files)) == tolower(p)]
		}
		if (length(matched) > 0) return(matched)
	}
	return(files)
}