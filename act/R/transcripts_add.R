#' Add transcripts to a corpus
#'
#' Add a single or multiple transcript objects to a corpus object. 
#' 
#' The name of the transcript objects have to be unique in the act package. 
#' The \code{@name} attribute of each transcript object will be set as identifier in the list of transcripts in the corpus object.
#' By default, transcripts with non unique names will be renamed. 
#' If you prefer to import.skip.double.files, set the parameter \code{skipDuplicates=TRUE}.
#' Skipped/renamed transcripts will be reported in 
#' 
#' @param x Corpus object
#' @param ... transcript object, list of transcript objects, corpus object.
#' @param skipDuplicates Logical; If \code{FALSE} double transcripts will be renamed to make the names unique, if \code{TRUE} double transcripts will not be added.
#' @param createFullText Logical; if \code{TRUE} full text will be created.
#' @param assignMedia Logical; if \code{TRUE} the folder(s) specified in \code{@paths.media.files} of your corpus object will be scanned for media. 
#'
#' @return Corpus object
#' @export
#'
#' @example inst/examples/transcripts_add.R
#' 
#' 
transcripts_add <- function(x, 
							..., 
							skipDuplicates=FALSE, 
							createFullText=TRUE, 
							assignMedia=TRUE) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	if (missing(...)) 	{stop("Missing transcript object(s) in parameter '...'.") 	} 
	
	#--- get list with all transcript objects from arguments
	arguments <- list(...)
	transcripts.new <- list()
	for (argument in arguments) {
		#add all transcripts of a corpus
		if (class(argument)[1]=="corpus"){
			if (length(argument@transcripts)>0) {
				transcripts.new <- c(transcripts.new, argument@transcripts)
			}
		}
		#add transcript
		if (class(argument)[1]=="transcript"){
			transcripts.new <- c(transcripts.new, argument)
		}
		
		#add transcripts from a list
		if (class(argument)[1]=="list"){
			for (element in argument) {
				if (class(element)[1]=="transcript"){
					transcripts.new <- c(transcripts.new, element)
				}
			}
		}
	}
	if (length(transcripts.new)==0) 	{stop("No valid transcript object(s) found in parameter '...'.") 	} 
	
	#---remember ids of transcripts in list
	transcripts_previous_ids <- names(x@transcripts)	
	
	#--- add transcripts
	x@transcripts <- c(x@transcripts, transcripts.new)
	
	#--- correct the names
	transcript.names      <- act::helper_transcriptNames_get(x)
	transcript.names.info <- helper_transcriptNames_make (transcriptNames           = transcript.names,
														  searchPatterns            = x@import.modify.transcript.names$searchPatterns,
														  searchReplacements        = x@import.modify.transcript.names$searchReplacements,
														  toUpperCase               = x@import.modify.transcript.names$toUpperCase,
														  toLowerCase               = x@import.modify.transcript.names$toLowerCase,
														  trim                      = x@import.modify.transcript.names$trim,
														  defaultForEmptyNames      = x@import.modify.transcript.names$defaultForEmptyNames)
	
	#--- set the new names
	x<-act::helper_transcriptNames_set(x, transcript.names.info$names.ok.ids)
	
	#--- skip
	transcripts_skipped_nr <-0
	transcripts_skipped_ids <-c()
	if (skipDuplicates) {
		#get the numbers of the transcripts to skip
		skip <- transcript.names.info$duplicated.ids[ which(transcript.names.info$duplicated.ids > length(transcripts_previous_ids))]
		
		#skip those
		x@transcripts           <- x@transcripts[-skip]
		
		#remeber numer and ids
		transcripts_skipped_nr  <- length(skip)
		transcripts_skipped_ids <- transcript.names.info$names.original.ids[skip]
	} 
	
	#get names/ids of added transcripts
	transcripts_added_ids <- setdiff(names(x@transcripts), transcripts_previous_ids)
	if (length(transcripts_added_ids)==0) {transcripts_added_ids <-c()}
	
	#=== update
	if (length(transcripts_added_ids)!=0) 	{	
		#=== normalize transcription content in object : normalized content will be in $content.norm
		x <- act::transcripts_update_normalization(x, transcriptNames=transcripts_added_ids)
		
		#=== create full text for searches
		if (createFullText) {	x <- act::transcripts_update_fulltexts(x,transcriptNames=transcripts_added_ids) }
		
		#=== scan for media
		if (assignMedia)	{	x <- act::media_assign(x, transcriptNames=transcripts_added_ids)	}
	}
	
	#===
	x@history[[length(x@history)+1]] <- list(
		modification               = "transcripts_add",
		systime                    = Sys.time(),
		transcripts.added.count    = length(transcripts_added_ids),
		transcripts.added.ids      = transcripts_added_ids,
		transcripts.skipped.count  = transcripts_skipped_nr,
		transcripts.skipped.ids    = transcripts_skipped_ids,
		transcripts.renamed.count  = length(transcript.names.info$names.modified.ids),
		transcripts.renamed.ids    = transcript.names.info$names.modified.ids
	)
	
	return(x)
}
