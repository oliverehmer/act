#' Import annotation files into corpus object
#' 
#' Scans all path specified in if \code{x@paths.annotation.files} for annotation files. 
#' Supported file formats will be loaded as transcript objects into the corpus object. 
#' All previously loaded transcript objects will be deleted.
#' 
#' If \code{assignMedia=TRUE} the paths defined in \code{x@paths.media.files} will be scanned for media files.
#' Based on their file names the media files and annotations files will be matched.
#' Only the the file types set in \code{options()$act.fileformats.audio} and \code{options()$act.fileformats.video} will be recognized. 
#' You can modify these options to recognize other media types.
#' 
#' See \code{@import.results} of the corpus object to check the results of importing the files.
#' To get a detailed overview of the corpus object use \code{act::info(x)}, for a summary use \code{act::info_summarized(x)}.
#' 
#' @param x Corpus object.
#' @param filterFilesInclude Character string; Regular expression which files should be loaded. 
#' @param createFullText Logical; if \code{TRUE} full text will be created.
#' @param assignMedia Logical; if \code{TRUE} the folder(s) specified in \code{@paths.media.files} of your corpus object will be scanned for media. 
#'
#' @return Corpus object.
#' 
#' @seealso \link{corpus_new}, \link{examplecorpus}
#' 
#' @export
#' 
#' @example inst/examples/corpus_import.R
#' 
corpus_import <- function(x, 
						  filterFilesInclude = "", 
						  createFullText     = TRUE, 
						  assignMedia        = TRUE) {
	
	 
	#filterFilesInclude <- ""
	#createFullText     <- TRUE 
	#assignMedia        <- TRUE
	#x<-a
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	#filterFilesInclude<-""
	
	#--- check if files and folders exist
	paths <- x@paths.annotation.files
	paths.dont.exist <- which(!file.exists(paths))
	message <- c()
	if (length(paths.dont.exist)>0) {
		message <- c(message, sprintf("%s of %s path(s) in 'x@paths.annotation.files' do not exist.", length(paths.dont.exist), length(x@paths.annotation.files)))
		m       <- stringr::str_c("    ",paths[paths.dont.exist], collapse="\n")
		message <- stringr::str_c(message,"\n", m, collapse="\n")
			
		paths <- paths[-paths.dont.exist]
	}
	if (length(paths)==0) {
		message <- c(message, "  No existing input paths.")
		message <- paste(message, sep='\n', collapse='\n')
		stop(message)
	} 
	
	#--- make list of all file paths
	paths.new <- c()
	for (path in paths) {
		#remove tailing slashes first
		path 	<- gsub("/*$", "", path , perl=TRUE)
		#if it is a directory
		if(dir.exists(path)) {
			#get all files in folders
			paths.sub <- list.files(path, recursive=getOption("act.import.scanSubfolders", TRUE), pattern=filterFilesInclude, ignore.case=TRUE,  full.names=TRUE)
			paths.new <- c(paths.new, paths.sub)
		} else {
			#it must be a file
			paths.new <- c(paths.new, path)
		}
	}
	
	#--- get only supported file formats 
	supportedFileFormats <- "(?i)\\.(eaf|exb|textgrid)"
	paths.new <- unlist(paths.new[stringr::str_which(string=paths.new, pattern=supportedFileFormats, )		])
	if (length(paths.new)==0) {
		stop("No annotation files found. Please check 'x@paths.annotation.files'.")
	}
	
	#--- make the names
	transcript.names <- basename(paths.new)
	transcript.names <- tools::file_path_sans_ext(transcript.names)
	transcript.names.info <- helper_transcriptNames_make (transcriptNames           = transcript.names,
														  searchPatterns            = x@import.modify.transcript.names$searchPatterns,
														  searchReplacements        = x@import.modify.transcript.names$searchReplacements,
														  toUpperCase               = x@import.modify.transcript.names$toUpperCase,
														  toLowerCase               = x@import.modify.transcript.names$toLowerCase,
														  trim                      = x@import.modify.transcript.names$trim,
														  defaultForEmptyNames      = x@import.modify.transcript.names$defaultForEmptyNames
	)
	
	
	results <- data.frame( file.name         = basename(paths.new),
						   transcript.name   = transcript.names.info$names.ok.ids,
						   status            = "load",
						   message           = "",
						   duplicated        = duplicated(transcript.names),
						   file.path         = paths.new, 
						   stringsAsFactors  = FALSE)

	#--- how to deal with double transcripts 
	if (any(results$duplicated)) {
		if (x@import.skip.double.files) {
			#skip duplicates
			results$status[results$duplicated]            <- "skipped"
			results$message[results$duplicated]           <- "Non-unique transcript names"
		} else {
			transcript.name.old <- results$transcript.name
			results$transcript.name <- make.unique(results$transcript.name)
			results$message[results$transcript.name!=transcript.name.old]           <- "Renamed because of non-unique transcript names"
		}
	}

	#--- read files
	#set progress bar
	helper_progress_set("Importing files", nrow(results))
	test <- list()
	counter <- 0
	i<- 1
	if (nrow(results) > 0) {
		for (i in 1:nrow(results) ) {
			#update progress bar
			helper_progress_tick()
			
			#import only the files that are to be loaded (and not skipped)
			if (results$status[i]=="load") {
				#import file
				new.transcript <- act::import(filePath=results$file.path[i], 
											  transcriptName=results$transcript.name[i])
				
				if (is.null(new.transcript)) {
					results$status[i]  <- "error"
					results$message[i] <- paste (results$message[i], "Unknown error", sep=", ", collapse=", ")
				} else {
					
					#get the result from this file
					if (new.transcript@import.result=="ok") {
						#importing was ok
						results$status[i] <- "ok"
						
						#if there is a message
						if (new.transcript@load.message!="") {
							if (results$message[i]=="") {
								results$message[i] <- new.transcript@load.message
							}
							results$message[i] <- paste (results$message[i], new.transcript@load.message, sep=", ", collapse=", ")
						}
						
						#add to the list
						counter <- counter+1
						test[[counter]] <- new.transcript
						
					} else {
						#load error
						results$status[i]  <- "error"
						results$message[i] <- paste (results$message[i], new.transcript@load.message, sep=", ", collapse=", ")						
					}
				}
			}
		}
	}
	
	#--- add results to corpus
	#View(results)
	#test
	x@import.results <- results
	
	#--- update history
	x@history[[length(x@history)+1]] <- list(
		modification  ="corpus_import",
		systime       = Sys.time(),
		results       = "See 'x@import.results'"
	)
	
	#--- add transcripts to corpus
	if (length(test)==0) {
		stop("No annotation files found in input path(s).")	
	} else {
		x <- act::transcripts_add(x, 
								 test, 
								 createFullText=createFullText, 
								 assignMedia=assignMedia)
	}
	
	#--- show warnings
	if (length(message)>0){
		warning(paste(message,sep="\n", collapse="\n"))
	}
	
	return(x)
}