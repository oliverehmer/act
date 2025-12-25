#' Update full texts
#'
#' Creates/updates the full texts of the transcripts in a corpus.
#'The full text may be created in two different ways: 
#'- The contents of a transcription will be joined consecutively based on the time information. 
#'- The contents of each tier will be joined consecutively, and then the next tier will be joined.
#' 
#' @param x Corpus object.
#' @param searchMode Character string; Which full text should be created; accepts the following values: \code{fulltext.bytier, fulltext.bytime, fulltext}.  
#' @param transcriptNames Vector of character strings; Names of the transcripts you want to update; leave empty if you want to process all transcripts that need an update.
#' @param tierNames Vector of character strings; Names of the tiers to include in the fulltext.
#' @param forceUpdate Logical; If \code{TRUE} fulltexts will be created in any case, if \code{FALSE} fulltexts will be only be created if there was a modification to the transcript since the last creation of the fulltexts.
#' 
#' @return Corpus object.
#' @export
#'
#' @examples
#' library(act)
#' 
#' examplecorpus <- act::transcripts_update_fulltexts(x=examplecorpus)
#' 
transcripts_update_fulltexts <- function(x,
										 searchMode      = c("fulltext", "fulltext.bytier", "fulltext.bytime"),
										 transcriptNames = NULL, 
										 tierNames       = NULL, 
										 forceUpdate     = FALSE) {
	#=== check x object
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (is.null(x@transcripts)) 	{
		warning("No transcripts found in corpus object x.")	
		return(x)
	}
	
	#  if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}
	
	#=== check arguments
	searchMode <- match.arg(searchMode)
	
	#=== check which transcripts need an update
	transcriptNames.update <- c()
	for (i in transcriptNames) {
		
		#=== check if update is necessary
		#- if update is forced
		#- if transcript content has been modified since last update
		#- if tier filter has been changed
		
		updateThis <- FALSE
		if (forceUpdate) {
			updateThis <- TRUE			
		} else {
			if (length(x@transcripts[[i]]@fulltext.systime)==0) {
				#fulltext has never been created
				updateThis <- TRUE
			} else {
				if (length(x@transcripts[[i]]@modification.systime)==0) {
				} else {
					#if transcript has been modified since last update
					if (x@transcripts[[i]]@modification.systime>x@transcripts[[i]]@fulltext.systime) {
						updateThis <- TRUE
					} else if (x@transcripts[[i]]@normalization.systime>x@transcripts[[i]]@fulltext.systime) {
						updateThis <- TRUE
					} else {
						
					}
				}
			}
		}
		#check if the current tier filter is not identical with the last tier filter
		if (!updateThis) {
			tierNames.forthistranscript <- x@transcripts[[i]]@tiers$name
			if (!is.null(tierNames)) {
				tierNames.forthistranscript <- tierNames.forthistranscript[tierNames.forthistranscript %in% tierNames]
			}
			if(!identical(x@transcripts[[i]]@fulltext.filter.tier.names, tierNames.forthistranscript)) {
				updateThis <- TRUE
			}
		}
		if (updateThis) {
			transcriptNames.update <- c(transcriptNames.update, i)
		}
	}
	
	
	#===iterate through all transcripts and annotations
	if (length(transcriptNames.update)>0) {
		
		#=== set progress bar
		helper_progress_set("Updating fulltexts", length(transcriptNames.update))
		
		for (i in transcriptNames.update) {
			#=== update progress bar
			helper_progress_tick()
			
			myFulltext.bytime.orig <-""
			myFulltext.bytime.norm <-""
			myFulltext.bytier.orig <-""
			myFulltext.bytier.norm <-""
			
			#do not get all columns but only the original ones
			#add the other later as empty cols
			mycols <- c("annotationID" ,
					   "tierName",
					   "startsec" ,
					   "endsec",
					   "content",
					   "content.norm"   )
			ann <- x@transcripts[[i]]@annotations[, mycols]
			
			#=== get filter by tiers
			tierNames.forthistranscript <- x@transcripts[[i]]@tiers$name
			if (!is.null(tierNames)) {
				tierNames.forthistranscript <- tierNames.forthistranscript[tierNames.forthistranscript %in% tierNames]
			}
			
			if (!is.null(ann))  	{
				if (nrow(ann)>0) 	{

					#===add new cols
					ann <- cbind.data.frame(ann, 
													  separator=as.character(""), 
													  char.separator=0, 
													  char.content.orig=0, 
													  char.content.norm=0, 
													  char.content.orig.plussep=0,
													  char.content.norm.plussep=0, 
													  char.orig.bytime.start=0, 
													  char.norm.bytime.start=0 , 
													  stringsAsFactors = FALSE)
					
					#---------------------- by time
					if (searchMode=="fulltext" | searchMode=="fulltext.bytime") {
						
						#=== sort annotations: start sec - tier name
						ann <- 	ann[order(ann$startsec, ann$tierName), ]
						
						#=== filter by tiers
						include <- which(ann$tierName %in% tierNames.forthistranscript)
						
						#=== make vector with separator character
						#check if tier of preceeding interval is the same
						included.sametier 				<- c(FALSE, ann$tierName[include[2:length(include)]] == ann$tierName[include[1:length(include)-1]])
						
						#make vector containing corresponding separators
						included.separator				<- unlist(lapply(included.sametier, function(x) if(x==TRUE) {options()$act.separator_between_intervals} else {options()$act.separator_between_tiers}))
						
						#set the separators in annotations
						#ann$separator 				<- ""  	# rep("", nrow(ann))
						ann$separator[include] 		<- included.separator
						
						#=== calculate the lengths only of included recordsets
						#separator
						ann$char.separator[include]				<- nchar(ann$separator[include])
						
						#content
						ann$char.content.orig[include]			<- nchar(ann$content[include] )
						ann$char.content.norm[include]			<- nchar(ann$content.norm[include] )
						
						#separator + content
						ann$char.content.orig.plussep[include]	<- ann$char.separator[include] + ann$char.content.orig[include]
						ann$char.content.norm.plussep[include]	<- ann$char.separator[include] + ann$char.content.norm[include]
						
						#end position within full text
						#-- original leads to error in recognizing the first hit
						ann$char.orig.bytime.end   	<- cumsum(ann$char.content.orig.plussep)
						ann$char.norm.bytime.end    	<- cumsum(ann$char.content.norm.plussep)
						
						#start position within full text
						#-- original leads to error in recognizing the first hit
						ann$char.orig.bytime.start  	<- ann$char.orig.bytime.end - ann$char.content.orig.plussep + 1
						ann$char.norm.bytime.start 	<- ann$char.norm.bytime.end - ann$char.content.norm.plussep+ 1
						
						#=== create full text
						myFulltext.bytime.orig 						<- paste(ann$separator[include], ann$content[include], sep="", collapse="")
						myFulltext.bytime.norm 						<- paste(ann$separator[include], ann$content.norm[include], sep="", collapse="")
						
						#end full text with separator for tiers
						myFulltext.bytime.orig 						<- paste(myFulltext.bytime.orig, options()$act.separator_between_tiers, sep="", collapse="")
						myFulltext.bytime.norm 						<- paste(myFulltext.bytime.norm, options()$act.separator_between_tiers, sep="", collapse="")
					}
					
					#---------------------- by tier
					if (searchMode=="fulltext" | searchMode=="fulltext.bytier") {
						
						#=== sort annotations: tier name - start sec
						ann <- 	ann[order(ann$tierName, ann$startsec), ]
						
						#=== filter by tiers
						include <- which(ann$tierName %in% tierNames.forthistranscript)
						
						#=== make vector with separator character
						#check if tier of preceeding interval is the same
						included.sametier 					<- c(FALSE, ann$tierName[include[2:length(include)]] == ann$tierName[include[1:length(include)-1]])
						
						#make vector containing corresponding separators
						included.separator					<- unlist(lapply(included.sametier, function(x) if(x==TRUE) {options()$act.separator_between_intervals} else {options()$act.separator_between_tiers}))
						
						#set the separators in annotations
						ann$separator 					<- rep("", nrow(ann))
						ann$separator[include] 			<- included.separator
						
						#=== calculate the lengths only of included recordsets
						#separator
						ann$char.separator[include]				<- nchar(ann$separator[include])
						
						#content
						ann$char.content.orig[include]			<- nchar(ann$content[include] )
						ann$char.content.norm[include]			<- nchar(ann$content.norm[include] )
						
						#separator + content
						ann$char.content.orig.plussep[include]	<- ann$char.separator[include] + ann$char.content.orig[include]
						ann$char.content.norm.plussep[include]	<- ann$char.separator[include] + ann$char.content.norm[include]
						
						#end position within full text
						ann$char.orig.bytier.end  		<- cumsum(ann$char.content.orig.plussep)
						ann$char.norm.bytier.end    		<- cumsum(ann$char.content.norm.plussep)
						
						#star position within full text
						ann$char.orig.bytier.start  	<- ann$char.orig.bytier.end  - ann$char.content.orig.plussep + 1
						ann$char.norm.bytier.start  	<- ann$char.norm.bytier.end - ann$char.content.norm.plussep + 1
						
						#=== create full text
						myFulltext.bytier.orig 						<- paste(ann$separator[include], ann$content[include], sep="", collapse="")
						myFulltext.bytier.norm 						<- paste(ann$separator[include], ann$content.norm[include], sep="", collapse="")
						
						#end full text with separator for tiers
						myFulltext.bytier.orig 						<- paste(myFulltext.bytier.orig, options()$act.separator_between_tiers, sep="", collapse="")
						myFulltext.bytier.norm 						<- paste(myFulltext.bytier.norm, options()$act.separator_between_tiers, sep="", collapse="")
					}
					
					#=== get rid of superfluous columns
					ann <- ann[,setdiff(colnames(ann), c("separator", "char.separator","char.content.orig","char.content.norm", "char.content.orig.plussep","char.content.norm.plussep"))]
					
					#=== save modified annotations back to corpus object
					x@transcripts[[i]]@annotations <- ann 
				}
			}
			
			#=== save full text to corpus object
			x@transcripts[[i]]@fulltext.bytime.orig <- myFulltext.bytime.orig 
			x@transcripts[[i]]@fulltext.bytime.norm <- myFulltext.bytime.norm 
			
			x@transcripts[[i]]@fulltext.bytier.orig <- myFulltext.bytier.orig 
			x@transcripts[[i]]@fulltext.bytier.norm <- myFulltext.bytier.norm 
			
			x@transcripts[[i]]@fulltext.systime     	  <- Sys.time()
			x@transcripts[[i]]@fulltext.filter.tier.names <- tierNames.forthistranscript
		}
	}
	return(x)
}