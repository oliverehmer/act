#' Export 'EXMARaLDA' .exb file
#'
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#' 
#' The .exb file will be written to the file specified in \code{pathOutput}.
#' If \code{pathOutput} is left empty, the function will return the contents of the .exb itself.
#'
#' @param t Transcript object; transcript to be exported.
#' @param pathOutput Character string; path where .exb file will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created.
#'
#' @return Contents of the .exb file (only if \code{pathOutput} is left empty)
#' @export
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_txt}, \link{export_docx}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}  
#' 
#' @example inst/examples/export_exb.R
#' 
#' 
export_exb <- function(t, 
					   pathOutput=NULL, 
					   filterTierNames=NULL, 
					   filterSectionStartsec = NULL, 
					   filterSectionEndsec = NULL, 
					   createMediaLinks=TRUE) {
	
	if (missing(t)) 	{stop("Transcript object in parameter 't' is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{stop("Parameter 't' needs to be a transcript object.") 	} }
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			stop("Output folder does not exist. Modify parameter 'pathOutput'.")
		}
	}
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=TRUE, tiersMissing=TRUE, warning=TRUE)

	# --- Convert Point tiers to interval tiers
	if ('TextTier' %in% t@tiers$type) {
		tempcorpus <- methods::new("corpus")
		tempcorpus@transcripts <- list(t)
		names(tempcorpus@transcripts) <- t@name
		tempcorpus <- act::tiers_convert(tempcorpus, pointToInterval=TRUE)
		t <- tempcorpus@transcripts[[1]]
	}
	
	#--- get annotations from transcript
	ann <- t@annotations
	
	#convert annotations to html safe characters
	ann$content   <-	XML::xmlValue(XML::xmlTextNode(as.vector(ann$content)))
	ann$tierName <-	XML::xmlValue(XML::xmlTextNode(as.vector(ann$tierName)))
	
	
	#--- get only relevant columns
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		stop(paste("Missing colums. Annotations need to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	ann <- ann[,myCols]
	
	#convert annotations to html save characters
	ann$content <-	XML::xmlValue(XML::xmlTextNode(as.vector(ann$content)))
	ann$tierName <-	XML::xmlValue(XML::xmlTextNode(as.vector(ann$tierName)))
	
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
	if (nrow(ann)>0) {
		#--- get all times
		alltimes <- c(	ann$startsec, ann$endsec)
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
		ann <- merge(ann, alltimes, by.x = "startsec", by.y = "value")
		myColnames <- colnames(ann)
		myColnames[myColnames=="ts"] <-"tsStart" 
		colnames(ann) <- myColnames
		
		ann <- merge(ann, alltimes, by.x = "endsec", by.y = "value")
		myColnames <- colnames(ann)
		myColnames[myColnames=="ts"] <-"tsEnd" 
		colnames(ann) <- myColnames
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
		ann <- ann[order(ordered(ann$tierName, levels = t@tiers$name), ann$startsec),]
		
		#for each tier
		tierNr <- 1
		tier_block <- list()
		for (tierNr in 1:nrow(t@tiers)) 	{
			speaker_name <- speakernames[tierNr]
			
			event_block <- sprintf(events, ann$tsStart[ann$tierName==t@tiers$name[tierNr]], 
								   ann$tsEnd[ann$tierName==t@tiers$name[tierNr]], 
								   ann$content[ann$tierName==t@tiers$name[tierNr]] 
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
			
	if (is.null(pathOutput)) 	{
		return(myEXB)
	} else {
		#---write to file
		fileConn <- file(pathOutput, open="wb")
		writeBin(charToRaw(myEXB), fileConn, endian="little")
		close(fileConn)			
	}
}
