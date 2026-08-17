#' Create .srt subtitles for all search results
#'
#' Subtitles in 'Subrib Title' .srt format will be created for each search result.
#' The subtitles will be inserted into the column defined in \code{s@cuts.column.srt}.
#' 
#' \emph{Span} \cr
#' If you want to extend the cut before or after each search result, you can modify \code{@cuts.span.beforesec} and \code{@cuts.span.aftersec} in your search object.
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param folderOutput Character string; path to an existing folder for file export. If \code{NULL} (default), no files will be written to disk; the srt subtitles are inserted only into \code{s@results}. If set, .srt files will be written to disk in addition.
#' @param speakerShow Logical; if \code{TRUE} name of speaker will be shown before the content of the annotation.
#' @param speakerWidth Integer; width of speaker abbreviation, -1 for full name without shortening.
#' @param speakerEnding Character string; string that is added at the end of the speaker name.
#'
#'
#' @return Search object; 
#' @export
#'
#' @example inst/examples/search_cuts_srt.R
#' 
#' 
search_cuts_srt <- function(x, 
							s, 
							cutSpanBeforesec = NULL,
							cutSpanAftersec = NULL,
							folderOutput=NULL, 
							speakerShow=TRUE, 
							speakerWidth=3, 
							speakerEnding=":" ) {
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	if (is.null(s@results$transcriptName)) 		{ cli::cli_abort("Data frame s@results does not contain column {.arg transcriptName}") 	}

	if (!is.null(cutSpanBeforesec)) 	{
		if (length(cutSpanBeforesec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanBeforesec)) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to be a numeric value.")
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	if (!is.null(cutSpanAftersec)) 	{
		if (length(cutSpanAftersec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanAftersec)) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to be a numeric value.")
		}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}
	#--- check if output folder is given
	destination_folder <- NULL
	if (!is.null(folderOutput)) {
		if (!options()$act.export.filename.fromColumnName %in% colnames(s@results)) {
			cli::cli_abort("The column defined in the option 'options()$act.export.filename.fromColumnName' does not exist in the data.frame with the search results.")
		}
		destination_folder <- normalizePath(folderOutput, winslash = "/", mustWork = FALSE)
		if (destination_folder!="") {
			if (dir.exists(destination_folder)==FALSE) 	{
				cli::cli_abort("Output folder does not exist.")
			}
			
			#create a sub folder for the search
			if (s@name!="") 	{
				destination_folder <- file.path(destination_folder, s@name)
				dir.create(destination_folder, showWarnings = FALSE)
				if (file.exists(destination_folder)==FALSE) 		{
					cli::cli_abort("Unable to create output directory")
				}
			}
		}	
	}
	
	myWarnings <- ""
	
	#create sub folders for text
	if (!is.null(destination_folder)) {
		dir.create(file.path(destination_folder, "srt"), showWarnings = FALSE)
	}
	
	#set progress bar
	helper_progress_set("Creating subtitles",max(1,nrow(s@results)))
	
	if (nrow(s@results)) {
		
		i <- 1
		for (i in 1:nrow(s@results)) 	{
			#update progress bar
			helper_progress_tick()
			
			#=== get transcript
			t <- NULL
			
			if (is.null(s@results$transcriptName[i])) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
				
			} else {
				t <- x@transcripts[[ s@results$transcriptName[i] ]]
				
				if (is.null(t)) {
					#transcript not found
					myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
				}
			}
			
			if (!is.null(t)) {
				#=== assemble_file NAME
				filename.col <- options()$act.export.filename.fromColumnName
				if (filename.col %in% colnames(s@results)) {
					filename <- as.character(s@results[i, filename.col])
				} else {
					filename <- as.character(i)
				}
				if (!exists("filename")) {
					filename <- as.character(i)
				} else if (is.na(filename)) {
					filename <- as.character(i)
				} else if (length(filename)==0) {
					filename <- as.character(i)
				} else {
					if (filename == "") {filename <- as.character(i)}
				}
				
				#=== get start & end
				startsec 	<- max(0, s@results$startsec[i] - s@cuts.span.beforesec)
				endsec 		<- min(s@results$endsec[i] + s@cuts.span.aftersec, t@length.sec)
				
				#assemble file PATH
				myfilePath <- NULL
				if (!is.null(destination_folder)) {
					myfilePath <- file.path(destination_folder, "srt", paste(filename, ".srt", sep=""))
				} 
				
				srt <- act::export_srt(   t                     = x@transcripts[[ s@results$transcriptName[i] ]],
										  pathOutput            = myfilePath,
										  filterTierNames       = s@filter.tier.names,
										  filterSectionStartsec = startsec,
										  filterSectionEndsec   = endsec,
										  speakerShow          = speakerShow,
										  speakerWidth         = speakerWidth,
										  speakerEnding        = speakerEnding
				)   
				
				
				#insert into column
				#add srt column if missing
				if (!s@cuts.column.srt %in% colnames(s@results)) {
					s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
					colnames(s@results)[ncol(s@results)] <- s@cuts.column.srt
				}
				#insert srt into search results
				output <- stringr::str_flatten(srt, collapse="\n")
				s@results[i, s@cuts.column.srt] <- output
				
			}
		} #next i
		
		
		# if output folder is given
		if (!is.null(destination_folder)) {
			
			#--- save modified results
			# R
			filename <- paste("searchResults_", s@name, ".RData", sep="")
			path_R 	    <-	file.path(destination_folder, filename)
			save(s, file = path_R)
			
			# CSV
			filename <- paste("searchResults_", s@name, ".csv", sep="")
			path_CSV 		<- file.path(destination_folder, filename)
			act::search_results_export(s, path_CSV, saveAsCSV = TRUE)
			
			# XLSX
			filename <- paste("searchResults_", s@name, ".xlsx", sep="")
			path_XLSX  <- file.path(destination_folder, filename)
			act::search_results_export(s, path_XLSX)
		}
	}
	#=== print warnings
	if (!myWarnings=="") {
		cli::cli_warn(unique(myWarnings))		
	}
	
	#=== give modified results back
	return(s)
}
