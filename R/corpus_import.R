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
#' @param createFulltext Logical; if \code{TRUE} full text will be created.
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
						  createFulltext     = TRUE,
						  assignMedia        = TRUE,
						  cureTranscripts    = TRUE,
						  verbose            = TRUE) {
	
	 
	#createFulltext     <- TRUE 
	#assignMedia        <- TRUE
	#x<-corpus
	if (missing(x)) 	{cli::cli_abort("Corpus object in parameter {.arg x} is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{cli::cli_abort("Parameter {.arg x} needs to be a corpus object.") } }
	
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
		cli::cli_warn(unique(message))
		return(x)
	} 
	
	#--- make list of all file paths
	paths.new <- c()
	for (path in paths) {
		#remove tailing slashes first
		path 	<- gsub("/*$", "", path , perl=TRUE)
		#if it is a directory
		if(dir.exists(path)) {
			#get all files in folders
			paths.sub <- list.files(path, recursive=getOption("act.import.scanSubfolders", TRUE), ignore.case=TRUE,  full.names=TRUE)
			paths.new <- c(paths.new, paths.sub)
		} else {
			#it must be a file
			paths.new <- c(paths.new, path)
		}
	}

	#include/exclude
	if (!is.null(x@import.names.include)) {
		if (!length(x@import.names.include)==0) {
			paths.new <- paths.new[grep(pattern=x@import.names.include, 	basename(paths.new))]
		}
	}
	if (!is.null(x@import.names.exclude)) {
		if (!length(x@import.names.exclude)==0) {
			paths.new <- paths.new[!grepl(pattern=x@import.names.exclude, 	basename(paths.new))]
		}
	}
	
	#--- get only supported file formats 
	supportedFileFormats <- "(?i)\\.(eaf|exb|srt|textgrid)"
	paths.new <- unlist(paths.new[stringr::str_which(string=paths.new, pattern=supportedFileFormats, )		])
	if (length(paths.new)==0) {
		cli::cli_abort("No annotation files found. Please check {.code x@paths.annotation.files}.")
	}
	
	#--- make the names
	transcriptNames <- basename(paths.new)
	transcriptNames <- tools::file_path_sans_ext(transcriptNames)
	transcriptNames.info <- helper_transcript_names_make (transcriptNames           = transcriptNames,
														  extractPatterns           = x@import.names.modify$extractPatterns,
														  searchPatterns            = x@import.names.modify$searchPatterns,
														  searchReplacements        = x@import.names.modify$searchReplacements,
														  toUpper               = x@import.names.modify$toUpper,
														  toLower               = x@import.names.modify$toLower,
														  trim                      = x@import.names.modify$trim,
														  defaultEmpty      = x@import.names.modify$defaultEmpty
	)
	
	results <- data.frame( file.name         = basename(paths.new),
						   transcriptName   = transcriptNames.info$names.before.unique,
						   status            = "load",
						   message           = "",
						   duplicated        = duplicated(transcriptNames.info$names.before.unique),
						   filePath         = paths.new,
						   stringsAsFactors  = FALSE)
#View(results)

	#--- backward compatibility: convert logical to character
	duplicate_handling <- x@import.skip.double.files
	if (is.logical(duplicate_handling)) {
		duplicate_handling <- if (isTRUE(duplicate_handling)) "warn_keep_first" else "allow"
	}

	#--- how to deal with double transcripts
	has_duplicates <- any(duplicated(transcriptNames.info$names.before.unique))
	if (has_duplicates) {
		if (duplicate_handling == "error") {
			dup_names <- unique(transcriptNames.info$names.before.unique[duplicated(transcriptNames.info$names.before.unique)])
			cli::cli_abort("Duplicate transcript name(s): {.val {dup_names}}")
		} else if (duplicate_handling == "allow") {
			results$transcriptName <- transcriptNames.info$names.ok.ids
			renamed_idx <- which(transcriptNames.info$names.ok.ids != transcriptNames.info$names.before.unique)
			results$message[renamed_idx] <- "Renamed because of non-unique transcript names"
		} else if (duplicate_handling %in% c("warn_keep_first", "warn_keep_newest")) {
			dup_names <- unique(transcriptNames.info$names.before.unique[duplicated(transcriptNames.info$names.before.unique)])
			if (duplicate_handling == "warn_keep_newest") {
				for (dn in dup_names) {
					dup_idx <- which(transcriptNames.info$names.before.unique == dn)
					file_dates <- file.info(results$filePath[dup_idx])$mtime
					keep_idx <- dup_idx[which.max(file_dates)]
					skip_idx <- setdiff(dup_idx, keep_idx)
					results$status[skip_idx] <- "skipped"
					results$message[skip_idx] <- paste0("Duplicate: newer file kept (", basename(results$filePath[keep_idx]), ")")
					results$message[keep_idx] <- paste0("Kept (duplicate of: ", paste(basename(results$filePath[skip_idx]), collapse = ", "), ")")
				}
			} else {
				skip_idx <- which(duplicated(transcriptNames.info$names.before.unique))
				results$status[skip_idx] <- "skipped"
				results$message[skip_idx] <- "Non-unique transcript names"
			}
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
				new.transcript <- act::import(filePath=results$filePath[i], 
											  transcriptName=results$transcriptName[i])
				
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

	#--- cure imported transcripts
	if (isTRUE(cureTranscripts) && length(test) > 0) {
		helper_progress_set("Curing transcripts", length(test))
		n_cured <- 0L
		for (j in seq_along(test)) {
			helper_progress_tick()
			test[[j]] <- act::transcripts_cure_single(
				t                         = test[[j]],
				annotationsTimesReversed  = TRUE,
				annotationsOverlap        = TRUE,
				annotationsTimesBelowZero = TRUE,
				transcriptLengthZero      = TRUE,
				annotationsZeroDuration   = TRUE,
				tiersMissing              = TRUE,
				warning                   = isTRUE(verbose)
			)
			h <- test[[j]]@history[[length(test[[j]]@history)]]
			if (identical(h$modification, "transcripts_cure_single")) {
				if (h$annotationsTimesReversed.deleted.count > 0
					|| h$annotationsTimesBelowZero.deleted.count > 0
					|| h$annotationsTimesBelowZero.corrected.count > 0
					|| h$annotationsOverlap.corrected.count > 0
					|| isTRUE(h$transcriptLengthZero.corrected)
					|| h$annotationsZeroDuration.merged.count > 0
					|| h$annotationsZeroDuration.extended.count > 0
					|| h$annotationsZeroDuration.deleted.count > 0
					|| h$tiersMissing.added.count > 0) {
					n_cured <- n_cured + 1L
				}
			}
		}
		if (verbose && n_cured > 0L) {
			cli::cli_inform(c(
				"!" = "{n_cured} of {length(test)} transcript(s) were cured during import.",
				"i" = "Details per transcript in {.code x@transcripts[[i]]@history}.",
				"i" = "To export cured transcripts, use {.code act::helper_corpus_export_cured()}."
			))
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
		cli::cli_abort("No annotation files found in input path(s).")
	} else {
		x <- act::transcripts_add(x=x, 
								 test, 
								 createFulltext=createFulltext, 
								 assignMedia=assignMedia)
	}

	#--- show warnings
	if (length(message)>0){
		cli::cli_warn(unique(message))
	}

	
	return(x)
}
