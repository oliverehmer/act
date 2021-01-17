#' Information about corpus and transcript objects
#'
#' Gives detailed information about the contents of a corpus object or a transcript object that is passed as parameter to the function.
#' In the case that you want to pass a transcript object form a corpus object, make sure that you access the transcript using double [[]] brackets.
#' 
#' To get summarized information about the transcript and corpus objects use \code{act::info_summarized}.
#' 
#' @param ... object; either  a corpus or a transcript object.
#'
#' @return List. 
#' 
#' @seealso \link{info_summarized}
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' act::info(examplecorpus)
#' 
#' act::info(examplecorpus@transcripts[[1]])
#' 
#' 
info <- function(...) {
	dots <- list(...)                
	if(length(dots) == 0) {stop ("You need to pass a corpus object or a transcript object to this function.")}
	x <-NULL
	t <-NULL
	if (class(dots[[1]])=="corpus") {
		x <- dots[[1]]	
	} else if (class(dots[[1]])=="transcript" ) {
		t <- dots[[1]]	
	} else {
		stop ("You need to pass a corpus object or a transcript object to this function. ")
	}

		#=== corpus
	if (!is.null(x)) {

		#--- transcripts
		transcripts <- 	data.frame( 
			transcript.name      =character(),
			length.sec           =double(),
			length.formatted     =character(),
			tiers.count          =integer(),
			annotations.count    =integer(),
			path                 =character(),
			file.encoding        =character(),
			import.result          =character(),
			load.message         =character(),
			media.path.count     =integer(),
			modification.systime = character(),
			stringsAsFactors     =FALSE
		)
		if (length(x@transcripts)>0) {
			for (i in 1:length(x@transcripts)) {
				myRow <- c(
					transcript.name      = x@transcripts[[i]]@name,
					length               = x@transcripts[[i]]@length,
					length.formatted     = helper_format_time(x@transcripts[[i]]@length),
					tiers.count          = nrow(x@transcripts[[i]]@tiers),
					annotations.count    = nrow(x@transcripts[[i]]@annotations),
					path                 = x@transcripts[[i]]@file.path,
					file.encoding        = x@transcripts[[i]]@file.encoding,
					import.result          = x@transcripts[[i]]@import.result,
					load.message    = x@transcripts[[i]]@load.message,
					media.path.count     = length(x@transcripts[[i]]@media.path),
					modification.systime = as.character(x@transcripts[[i]]@modification.systime)
				)
				transcripts[nrow(transcripts)+1,] <- myRow
			}
		}

		#--- tiers
		#=== base data
		temp <- act::tiers_all(x)
		
		#=== Collapse by tier type
		name.unique <- unique(temp$name)
		temp2 <-data.frame(name                        =character(),
						  tiers.count                  =integer(),
						  transcripts.count            =integer(),
						  transcripts.names            =character(),
						  annotations.count            =integer(),
						  words.org.count              =integer(),
						  words.norm.count             =integer(),
						  
						  interval.tiers.count         =integer(),
						  interval.transcripts.count   =integer(),
						  interval.transcripts.names   =character(),
						  interval.annotations.count   =integer(),
						  interval.words.org.count     =integer(),
						  interval.words.norm.count    =integer(),
						  
						  text.tiers.count             =integer(),
						  text.transcripts.count       =integer(),
						  text.transcripts.names       =character(),
						  text.annotations.count       =integer(),
						  text.tiers.words.org.count   =integer(),
						  text.tiers.words.norm.count  =integer(),
						  
						  stringsAsFactors=FALSE)
		
		if (length(name.unique)>0) {
			for (i in 1:length(name.unique)) {
				tiers.current                              <- temp[which(temp$name==name.unique[i]),]
				tiers.current.interval                     <- temp[which(temp$name==name.unique[i] & temp$type=="IntervalTier"),]
				tiers.current.text                         <- temp[which(temp$name==name.unique[i] & temp$type=="TextTier"),]
				
				myRow <- c(
					name                              = name.unique[i],
					tiers.count 		              = nrow(tiers.current),
					transcripts.count                 = length(unique(tiers.current$transcripts.name)),
					transcripts.names                 = paste(unique(tiers.current$transcripts.name), sep="|", collapse="|"),
					annotations.count                 = sum(tiers.current$annotations.count),
					words.org.count                   = sum(tiers.current$words.org.count),
					words.norm.count                  = sum(tiers.current$words.norm.count),
					
					interval.tiers.count              = nrow(tiers.current.interval),
					interval.transcripts.count        = length(unique(tiers.current.interval$transcripts.name)),
					interval.transcripts.names        = paste(unique(tiers.current.interval$transcripts.name), sep="|", collapse="|"),
					interval.annotations.count        = sum(tiers.current.interval$annotations.count),
					interval.words.org.count          = sum(tiers.current.interval$words.org.count),
					interval.words.norm.count         = sum(tiers.current.interval$words.norm.count),
					
					text.tiers.count                  = nrow(tiers.current.text),
					text.transcripts.count            = length(unique(tiers.current.text$transcripts.name)),
					text.transcripts.names            = paste(unique(tiers.current.text$transcripts.name), sep="|", collapse="|"),
					text.annotations.count            = sum(tiers.current.text$annotations.count),
					text.tiers.words.org.count        = sum(tiers.current.text$words.org.count),
					text.tiers.words.norm.count       = sum(tiers.current.text$words.norm.count)
				)
				temp2[nrow(temp2)+1,] <- myRow
			}
			temp2 <- temp2[order(temp2$name),]
		}

		#--- list info
		info <- list(transcripts=transcripts, 
					 tiers=temp2)
		return(info)
	}

	if (!is.null(t)) {
		#--- tiers
		tiers.names <- t@tiers$name
		tiers.count <- nrow(t@tiers)
		
		#--- tiers detailed
		tiers.detailed <- t@tiers
		for (i in 1:nrow(tiers.detailed)) {
			#--- get annotations
			ids <-which(t@annotations$tier.name==tiers.detailed$name[i])
			#--- number of annotations
			tiers.detailed$annotations.count[i] <- length(ids)
			#--- words org
			words.org.count <- lapply(t@annotations$content[ids], FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
			tiers.detailed$words.org.count[i] <- sum(unlist(words.org.count))
			#--- words norm
			words.norm.count <- lapply(t@annotations$content.norm[ids], FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
			tiers.detailed$words.norm.count[i] <- sum(unlist(words.norm.count))
		}

		#--- annotations
		annotations.count <- sum(nrow(t@annotations))
		
		#--- words org
		words.org.count <- lapply(t@annotations$content, FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
		words.org.count <- sum(unlist(words.org.count))
		
		#--- words norm
		words.norm.count <- lapply(t@annotations$content.norm, FUN=stringr::str_count, pattern=options()$act.wordCount.regex)
		words.norm.count <- sum(unlist(words.norm.count))
		
		info <- list(length.formatted  = helper_format_time(t@length),
					 length.sec        = t@length,
					 words.org.count   = words.org.count,
					 words.norm.count  = words.norm.count,					 
					 annotations.count = annotations.count,
					 tiers.count       = tiers.count,
					 tiers.names       = tiers.names,
					 tiers.detailed    = tiers.detailed
					 )
		return(info)
	}
}
