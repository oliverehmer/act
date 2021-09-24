#' Export a transcript object to a 'EXMARaLDA' .exb file
#'
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#' 
#' The .exb file will be written to the file specified in \code{outputPath}.
#' If \code{outputPath} is left empty, the function will return the contents of the .exb itself.
#'
#' @param t Transcript object; transcript to be exported.
#' @param outputPath Character string; path where .exb file will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created.
#'
#' @return Contents of the .exb file (only if \code{outputPath} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \code{export_eaf}, \code{export_printtranscript}, \code{export_rpraat}, \code{export_srt}, \code{export_textgrid}  
#' 
#' @example inst/examples/export_exb.R
#' 
#' 
export_exb <- function(t, 
					   outputPath=NULL, 
					   filterTierNames=NULL, 
					   filterSectionStartsec = NULL, 
					   filterSectionEndsec = NULL, 
					   createMediaLinks=TRUE) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	} else { if (class(t)[[1]]!="transcript") 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(outputPath)) {
		if (!dir.exists(dirname(outputPath))) {
			stop("Output folder does not exist. Modify parameter 'outputPath'.")
		}
	}
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsWithReversedTimes=TRUE, overlappingAnnotations=TRUE, annotationsWithTimesBelowZero=TRUE, missingTiers=TRUE, showWarning=TRUE)

	# --- Convert Point tiers to interval tiers
	if ('TextTier' %in% t@tiers$type) {
		tempcorpus <- methods::new("corpus")
		tempcorpus@transcripts <- list(t)
		names(tempcorpus@transcripts) <- t@name
		tempcorpus <- act::tiers_convert(tempcorpus, pointToInterval=TRUE)
		t <- tempcorpus@transcripts[[1]]
	}
	
	#--- get annotations from transcript
	myAnnotations <- t@annotations
	
	#convert annotations to html safe characters
	myAnnotations$content   <-	XML::xmlValue(XML::xmlTextNode(as.vector(myAnnotations$content)))
	myAnnotations$tier.name <-	XML::xmlValue(XML::xmlTextNode(as.vector(myAnnotations$tier.name)))
	
	
	#--- get only relevant columns
	myCols <- c("tier.name", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myAnnotations))) {
		stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	myAnnotations <- myAnnotations[,myCols]
	
	#convert annotations to html save characters
	myAnnotations$content <-	XML::xmlValue(XML::xmlTextNode(as.vector(myAnnotations$content)))
	myAnnotations$tier.name <-	XML::xmlValue(XML::xmlTextNode(as.vector(myAnnotations$tier.name)))
	
	#--- generate EAF-XML-document
	myEXB <- paste(
		       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
               '<!-- (c) http://www.rrz.uni-hamburg.de/exmaralda -->',
		       '<basic-transcription>',        
		       '%s',                                                    #head
		       '%s',                                                    #basic body
		       '</basic-transcription>',
		       sep='\n')

	#--- head
	head    <- paste(
		       '   <head>',
	           '%s',                                  #meta
		       '%s',                                  #speakers
		       '   </head>',
		       sep='\n')
	
	#--- meta
	meta    <- paste(
		       '      <meta-information>',
		       '         <project-name></project-name>',
	           '         <transcription-name>%s</transcription-name>',     #transcription name
	           '%s',                                                       # referenced files
	           '         <ud-meta-information></ud-meta-information>',
	           '         <comment></comment>',
		       '         <transcription-convention></transcription-convention>',
	           '      </meta-information>',
		       sep='\n')

	
	#--- files
	files   <- '         <referenced-file url="%s" />'
	
	#--- speaktertable
	speakertable  <- paste(
	           '      <speakertable>',
               '%s',                       # speaker
	           '      </speakertable>',
	           sep='\n')
	
	#--- speaker
	speaker  <- paste(
		'         <speaker id="%s">',                                       #ID
		'            <abbreviation>%s</abbreviation>',                      #abbreviation
		'            <sex value="" />',
		'            <languages-used></languages-used>',
		'            <l1 />',
		'            <l2 />',
		'            <ud-speaker-information></ud-speaker-information>',
		'            <comment />',
		'         </speaker>',
		sep='\n')

	#--- basic body
	basic_body    <- paste(
		'   <basic-body>',
		'%s',                     #timeline
		'%s',                     #tiers
		'   </basic-body>',
		sep='\n')
	
	timeline <- paste(
		'      <common-timeline>',
		'%s',                            #tli
		'      </common-timeline>',
		sep='\n')
	
	tli <- paste(
		'         <tli id="%s" time="%s" />',
		sep='\n')
	
	tier <- paste(
		'      <tier id="%s" speaker="%s" category="v" type="t" display-name="%s [v]" >',
		'%s',                                                                            #events
		'      </tier>',
		sep='\n')
	
	events <- paste(
		'         <event start="%s" end="%s">%s</event>',
		sep='\n')
	
	#==== fill the structures

	#--- files
	if (createMediaLinks) {
		files   <- sprintf(files, basename(t@media.path))
		files   <- paste(files, collapse='\n')
		if (length(files)==0) {
			files <- '<referenced-file url=""/>'
		}	
	} else {
		files <- '<referenced-file url=""/>'
	}
	
	#--- meta
	meta  <- sprintf(meta, t@name, files)
	#cat(meta)
	
	#--- speaker
	#create list of speakers
	numberofspeakers <- max(1, nrow(t@tiers))
	speakernames <- paste("SPK", 1:numberofspeakers, sep="")
	speaker   <- sprintf(speaker, speakernames,  speakernames)
	speaker   <- paste(speaker, collapse='\n')
	#cat(speaker)
	
	#--- speakertable
	speakertable   <- sprintf(speakertable, speaker)
	#cat(speakertable)

	#--- head
	head  <- sprintf(head, meta, speakertable)
	#cat(head)

	#--- timeline 
	#generate times 
	if (nrow(myAnnotations)>0) {
		#--- get all times
		alltimes <- c(	myAnnotations$startSec, myAnnotations$endSec)
		if (min (alltimes>0)) {
			alltimes <- c(0,alltimes)
		}
		if (max (alltimes< t@length.sec )) {
			alltimes <- c(alltimes, t@length.sec)
		}
		alltimes <- alltimes[order(alltimes)]
		alltimes <- unique(alltimes)
		alltimes <- as.data.frame(cbind(ts=paste("T", 1:length(alltimes),sep=""), value=as.character(alltimes)), stringsAsFactors=FALSE)
		
		#--- merge with annotations
		myAnnotations <- merge(myAnnotations, alltimes, by.x = "startSec", by.y = "value")
		myColnames <- colnames(myAnnotations)
		myColnames[myColnames=="ts"] <-"tsStart" 
		colnames(myAnnotations) <- myColnames
		
		myAnnotations <- merge(myAnnotations, alltimes, by.x = "endSec", by.y = "value")
		myColnames <- colnames(myAnnotations)
		myColnames[myColnames=="ts"] <-"tsEnd" 
		colnames(myAnnotations) <- myColnames
		#generate timeline
		tli   <- sprintf(tli, alltimes$ts, alltimes$value)
    } else {
		tli   <-         '<tli id="T0"/>\n<tli id="T1"/>'
    }
	tli <- paste(tli, collapse="\n")
	tli <- stringr::str_replace(tli, 'time="0"', 'time="0.0"')
	timeline <- sprintf(timeline, tli)
	timeline <- paste(timeline, collapse="\n")
	#cat(timeline)
	
	#iterate through all tiers
	if (nrow(t@tiers)==0) {
		tiers <- '      <tier id="TIE0" speaker="SPK0" category="v" type="t" display-name="X [v]">\n</tier>'
	} else {
		#sort annotations by tier names and start time
		myAnnotations <- myAnnotations[order(ordered(myAnnotations$tier.name, levels = t@tiers$name), myAnnotations$startSec),]
		
		#for each tier
		tierNr <- 1
		tier_block <- list()
		for (tierNr in 1:nrow(t@tiers)) 	{
			speaker_name <- speakernames[tierNr]
			
			event_block <- sprintf(events, myAnnotations$tsStart[myAnnotations$tier.name==t@tiers$name[tierNr]], 
								   myAnnotations$tsEnd[myAnnotations$tier.name==t@tiers$name[tierNr]], 
								   myAnnotations$content[myAnnotations$tier.name==t@tiers$name[tierNr]] 
								   )
			
			event_block <- paste(event_block, collapse="\n")
			#cat(event_block)
			
			tier_block[tierNr] <- sprintf(tier, t@tiers$name[tierNr], speaker_name, speaker_name, event_block)
			tier_block[tierNr] <- paste(tier_block[tierNr], collapse="\n")
			#cat(tier_block[[tierNr]])
		}
		tiers <- paste(unlist(tier_block), collapse="\n")
	}
	basic_body <- sprintf(basic_body, timeline, tiers)
	
	#=== put everything together
	myEXB <- sprintf(myEXB, head, basic_body)
	#cat(myEXB)
			
	if (is.null(outputPath)) 	{
		return(myEXB)
	} else {
		#---write to file
		fileConn <- file(outputPath, open="wb")
		writeBin(charToRaw(myEXB), fileConn, endian="little")
		close(fileConn)			
	}
}
