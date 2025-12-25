#' Export transcripts of a corpus
#' 
#' Exports all (or some) transcript objects in a corpus object to different annotation file formats.
#' If only some transcripts or tiers should be affected set the parameter \code{filterTranscriptNames} and \code{filterTierNames}.
#' In case that you want to select transcripts and/or tiers by using regular expressions use the function \code{act::search_makefilter} first. 
#'
#' @param x Corpus object.
#' @param folderOutput Character string; path to a folder where the transcription files will be saved. By default the forlder will be created recursively it does not exist.
#' @param filterTranscriptNames Vector of character strings; names of transcripts to be included. If left unspecified, all transcripts will be exported.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param formats Vector with one or more character strings; output formats, accepted values: 'eaf', 'exb', and 'textgrid', 'srt' and 'edl', 'docx' and 'txt'. If left unspecified, all supported formats will be exported.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created (affects only 'eaf' and 'exb' files).
#' @param createFolderOutput Logical; if \code{TRUE} the output folder will be created recursively in case that it does not exist.
#' @param l Layout object. layout of print transcripts (affects only 'txt' and 'docx' files).
#'
#' 
#' @export
#' 
#' @seealso \link{export_eaf}, \link{export_textgrid}, \link{export_exb}, \link{export_txt}, \link{export_docx}, \link{export_rpraat}, \link{export_srt}  

#'
#' @example inst/examples/corpus_export.R
#' 
corpus_export <-  function(x, 
						   folderOutput, 
						   filterTranscriptNames=NULL, 
						   filterTierNames=NULL, 
						   formats=c("docx", "eaf","exb", "edl", "srt", "textgrid", "txt"), 
						   createMediaLinks=TRUE,
						   createFolderOutput=TRUE,
						   l=NULL) {
	
	if (1==2) {
		
		#x<-examplecorpus
		#folderOutput  <-  '/Users/oliverehmer/Desktop/test'
		#formats<-c("docx", "eaf","exb", "edl", "srt", "textgrid", "txt")
		#formats<-c("txt")
		#print(i)
		#formats<-"eaf"
		#folderOutput<-"/Users/oliverehmer/Desktop/export/"
		#filterTierNames<-NULL
		#createMediaLinks <- FALSE
		#l<- layout_spa_icas
	}
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	
	if (missing(folderOutput)) { stop("No output folder specified in parameter 'folderOutput'") }
	
	if (!dir.exists(folderOutput)) {
		if (createFolderOutput) {
			dir.create(folderOutput, recursive=TRUE)
			if (!dir.exists(folderOutput)) {
				stop("Error while crating the output. Modify the parameter 'folderOutput'.")
			} 
		} else {
			stop("Output folder does not exist. Modify parameter 'folderOutput'.")
		}
	}
	
	if (is.null(filterTranscriptNames)) {
		filterTranscriptNames <- names(x@transcripts)
	} else {
		filterTranscriptNames <- intersect(filterTranscriptNames, names(x@transcripts))
		if (length(filterTranscriptNames)==0) {
			stop("No transcripts to export. Possibly modify parameter 'filterTranscriptNames'.")
		}
	}
	
	#set progress bar
	helper_progress_set("Exporting",length(filterTranscriptNames))
	
	for (i in filterTranscriptNames) {
		#update progress bar
		helper_progress_tick()
		
		if ( "eaf" %in% stringr::str_to_lower(formats)) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "eaf", sep="."))
			act::export_eaf(t=x@transcripts[[i]], pathOutput=pathOutput, filterTierNames=filterTierNames, createMediaLinks=createMediaLinks)
		}
		
		if ( "exb" %in% stringr::str_to_lower(formats)) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "exb", sep="."))
			act::export_exb(t=x@transcripts[[i]], pathOutput=pathOutput, filterTierNames=filterTierNames, createMediaLinks=createMediaLinks)
		}
		
		if ( "textgrid" %in% stringr::str_to_lower(formats)) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "TextGrid", sep="."))
			export_textgrid(t=x@transcripts[[i]], pathOutput=pathOutput,filterTierNames=filterTierNames )
		}
		
		if ( "docx" %in% stringr::str_to_lower(formats) ) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "docx", sep="."))
			export_docx(t=x@transcripts[[i]], l=l, pathOutput=pathOutput, filterTierNames=filterTierNames )
		}
		
		if ( "txt" %in% stringr::str_to_lower(formats) ) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "txt", sep="."))
			export_txt(t=x@transcripts[[i]], l=l, pathOutput=pathOutput, filterTierNames=filterTierNames )
		}
		
		if ( "edl" %in% stringr::str_to_lower(formats)) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "edl", sep="."))
			act::export_edl(t=x@transcripts[[i]], pathOutput=pathOutput, filterTierNames=filterTierNames)
		}		
		
		if ( "srt" %in% stringr::str_to_lower(formats)) {
			pathOutput <- file.path(folderOutput, paste(x@transcripts[[i]]@name, "srt", sep="."))
			act::export_srt(t=x@transcripts[[i]], pathOutput=pathOutput, filterTierNames=filterTierNames)
		}
	}
}
