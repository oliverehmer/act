#' Export transcripts of a corpus
#' 
#' Exports all (or some) transcript objects in a corpus object to different annotation file formats.
#' If only some transcripts or tiers should be affected set the parameter \code{filterTranscriptNames} and \code{filterTierNames}.
#' In case that you want to select transcripts and/or tiers by using regular expressions use the function \code{act::search_makefilter} first. 
#'
#' @param x Corpus object.
#' @param outputFolder Character string; path to a folder where the transcription files will be saved. By default the forlder will be created recursively it does not exist.
#' @param filterTranscriptNames Vector of character strings; names of transcripts to be included. If left unspecified, all transcripts will be exported.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param formats Vector with one or more character strings; output formats, accepted values: 'eaf', 'exb', 'srt', 'textgrid', 'printtranscript'. If left unspecified, all supported formats will be exported.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created (affects only 'eaf' and 'exb' files).
#' @param createOutputfolder Logical; if \code{TRUE} the outputfolder will be created recursively in case that it does not exist.
#' @param l Layout object. layout of print transcripts (affects only 'printtranscript' files).
#'
#' 
#' @export
#' 
#' @seealso \link{export_eaf}, \link{export_textgrid}, \link{import_textgrid}
#'
#' @example inst/examples/corpus_export.R
#' 
corpus_export <-  function(x, 
						   outputFolder, 
						   filterTranscriptNames=NULL, 
						   filterTierNames=NULL, 
						   formats=c("eaf","exb","srt","textgrid", "printtranscript"), 
						   createMediaLinks=TRUE,
						   createOutputfolder=TRUE,
						   l=NULL) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	
	if (missing(outputFolder)) { stop("No output folder specified in parameter 'outputFolder'") }
	
	if (!dir.exists(outputFolder)) {
		if (createOutputfolder) {
			dir.create(outputFolder, recursive=TRUE)
			if (!dir.exists(outputFolder)) {
				stop("Error while crating the output. Modify the parameter 'outputFolder'.")
			} 
		} else {
			stop("Output folder does not exist. Modify parameter 'outputFolder'.")
		}
	}
	
	if (is.null(filterTranscriptNames)) {
		filterTranscriptNames <- names(x@transcripts)
	}
	
	#set progress bar
	helper_progress_set("Exporting",length(filterTranscriptNames))
	
	for (i in filterTranscriptNames) {
		#update progress bar
		helper_progress_tick()
		
		#print(i)
		#formats<-"eaf"
		#outputFolder<-"/Users/oliverehmer/Desktop/export/"
		#filterTierNames<-NULL
		#createMediaLinks <- TRUE
		
		if ( "eaf" %in% stringr::str_to_lower(formats)) {
			outputPath <- file.path(outputFolder, paste(x@transcripts[[i]]@name, "eaf", sep="."))
			act::export_eaf(t=x@transcripts[[i]], outputPath=outputPath, filterTierNames=filterTierNames, createMediaLinks=createMediaLinks)
		}
		
		if ( "exb" %in% stringr::str_to_lower(formats)) {
			outputPath <- file.path(outputFolder, paste(x@transcripts[[i]]@name, "exb", sep="."))
			act::export_exb(t=x@transcripts[[i]], outputPath=outputPath, filterTierNames=filterTierNames, createMediaLinks=createMediaLinks)
		}
		
		if ( "srt" %in% stringr::str_to_lower(formats)) {
			outputPath <- file.path(outputFolder, paste(x@transcripts[[i]]@name, "srt", sep="."))
			act::export_srt(t=x@transcripts[[i]], outputPath=outputPath, filterTierNames=filterTierNames)
		}
		
		if ( "textgrid" %in% stringr::str_to_lower(formats)) {
			outputPath <- file.path(outputFolder, paste(x@transcripts[[i]]@name, "TextGrid", sep="."))
			export_textgrid(t=x@transcripts[[i]], outputPath=outputPath,filterTierNames=filterTierNames )
		}
		
		if ( "printtranscript" %in% stringr::str_to_lower(formats)) {
			outputPath <- file.path(outputFolder, paste(x@transcripts[[i]]@name, "txt", sep="."))
			export_printtranscript(t=x@transcripts[[i]], l=l, outputPath=outputPath, filterTierNames=filterTierNames )
		}
	}
}