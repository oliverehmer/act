#' Export 'ELAN' .eaf file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_export} for exporting annotation files.
#' 
#' The .eaf file will be written to the file specified in \code{pathOutput}.
#' If \code{pathOutput} is left empty, the function will return the contents of the .eaf itself.
#'
#' @param t Transcript object; transcript to be exported.
#' @param pathOutput Character string; path where .eaf file will be saved.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created.
#' @param mediaMaxFiles Integer or \code{NULL}; maximum number of media files to link (\code{NULL} = all).
#' @param mediaWriteOffset Logical; if \code{TRUE} a negative \code{TIME_ORIGIN} attribute (in milliseconds) is written for clip media (rows with \code{kind == "clip"} and \code{startsec > 0}), derived from \code{startsec}. Default \code{FALSE} (no offset). Note: clip media are currently not linked at all in the exported .eaf, because 'ELAN' does not support negative media offsets - only full recordings (offset 0) are linked. This parameter takes effect once clip linking is re-enabled in the code.
#'
#' @return Contents of the .eaf file (only if \code{pathOutput} is left empty)
#' @export
#' 
#' @seealso \code{corpus_export}, \link{export_exb}, \link{export_txt}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}, \link{export_docx}   
#' 
#' @example inst/examples/export_eaf.R
#' 
export_eaf <- function(t, 
					   pathOutput=NULL, 
					   filterTierNames=NULL, 
					   filterSectionStartsec = NULL,
					   filterSectionEndsec = NULL,
					   createMediaLinks=TRUE,
					   mediaMaxFiles=NULL,
					   mediaWriteOffset=FALSE) {
	
	.assert_transcript(t, missing = missing(t))
	
	#--- check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			cli::cli_abort("Output folder does not exist. Modify parameter {.arg pathOutput}.")
		}
	}
	
	#	t<- meinkorpus@transcripts[["ARG_I_CHI_Santi"]]
	#	pathOutput<-NULL
	#	filterTierNames<-NULL
	#	filterSectionStartsec <- NULL
	#	filterSectionEndsec <- NULL 
	#	createMediaLinks <- TRUE
	#	t<- meinkorpus@transcripts[[7]]
	
	#=== Get data
	#--- Filter and cure transcript
	t <- act::transcripts_filter_single(t, filterTierNames=filterTierNames, filterSectionStartsec = filterSectionStartsec, filterSectionEndsec = filterSectionEndsec)
	t <- act::transcripts_cure_single(t, annotationsTimesReversed=TRUE, annotationsOverlap=TRUE, annotationsTimesBelowZero=TRUE, transcriptLengthZero=TRUE, annotationsZeroDuration=TRUE, tiersMissing=TRUE, warning=TRUE)
	
	# --- Convert Point tiers to interval tiers
	if ('TextTier' %in% t@tiers$type) {
		tempcorpus <- methods::new("corpus")
		tempcorpus@transcripts <- list(t)
		names(tempcorpus@transcripts) <- t@name
		tempcorpus <- act::tiers_convert(tempcorpus, pointToInterval=TRUE)
		t <- tempcorpus@transcripts[[1]]
	}
	#---get annotations
	ann <- t@annotations
	
	#--- get only relevant columns
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		cli::cli_abort("Missing columns. Annotations need to contain: {.val {myCols}}")
	}
	ann <- ann[,myCols]
	
	# XML escape helper: escape special characters for XML output
	xml_escape <- function(x) {
		x <- gsub("&",  "&amp;",  x, fixed = TRUE)
		x <- gsub("<",  "&lt;",   x, fixed = TRUE)
		x <- gsub(">",  "&gt;",   x, fixed = TRUE)
		x <- gsub("\"", "&quot;", x, fixed = TRUE)
		x
	}
	
	#--- generate EAF-XML-document
	myEAF <-               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	myEAF <- append(myEAF, "<ANNOTATION_DOCUMENT AUTHOR=\"\" DATE=\"2018-05-04T19:33:08+01:00\" FORMAT=\"3.0\" VERSION=\"3.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.mpi.nl/tools/elan/EAFv3.0.xsd\">" )
	
	#--- generate media header
	myEAF <- append(myEAF, "    <HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\">")
	if (createMediaLinks==TRUE) 	{
		media_sel <- act::media_select(t@media, maxFiles = mediaMaxFiles, apply = FALSE)
		if (nrow(media_sel)>0) 		{
			elan_supports_negative_offset <- FALSE
			for (i in seq_len(nrow(media_sel))) {
				is_clip <- "kind" %in% colnames(media_sel) && !is.na(media_sel$kind[i]) && media_sel$kind[i] == "clip"
				#--- clips are not linked while ELAN cannot use negative offsets
				if (is_clip && !elan_supports_negative_offset) {
					next
				}
				mediaPath <- media_sel$path[i]
				myMimeType <- "unknown"
				if (tolower(tools::file_ext(mediaPath)) %in% c("wav")) {
					myMimeType <-"audio/x-wav"
				} else if (tolower(tools::file_ext(mediaPath)) %in% c("mp3", "aif", "aiff")) {
					myMimeType <-"audio/*"
				} else if (tolower(tools::file_ext(mediaPath)) %in% c("mp4", "mov", "mpg")) {
					myMimeType <-"video/mp4"
				}
				#--- optional TIME_ORIGIN offset for clips
				time_origin_attr <- ""
				if (isTRUE(mediaWriteOffset) && is_clip) {
					ss <- media_sel$startsec[i]
					if (!is.na(ss) && ss > 0) {
						time_origin_attr <- sprintf(" TIME_ORIGIN=\"%d\"", as.integer(round(-ss * 1000)))
					}
				}
				myEAF <- append(myEAF, sprintf("         <MEDIA_DESCRIPTOR MEDIA_URL=\"file:///%s\" MIME_TYPE=\"%s\"%s/>", mediaPath, myMimeType, time_origin_attr))
			}
		}
	}
	
	#<PROPERTY NAME="URN">urn:nl-mpi-tools-elan-eaf:cfb1957f-d6fa-4f7d-b124-6a74700d014d</PROPERTY>
	#<PROPERTY NAME="lastUsedAnnotationId">21</PROPERTY>
	myEAF <- append(myEAF, "    </HEADER>")
	
	if (nrow(ann)>0) {
		#--- generate time order
		ann$annotationID <- paste("a",1:nrow(ann),sep="")
		ann$TIME_SLOT_REF1 <- paste("ts",1:nrow(ann),sep="")
		ann$TIME_SLOT_REF2 <- paste("ts", (nrow(ann)+1):(nrow(ann)*2),sep="")
		ann$startsec <- as.integer(ann$startsec*1000)
		ann$endsec <- as.integer(ann$endsec*1000)
		
		myEAF <- append(myEAF,         "    <TIME_ORDER>")
		myEAF <- append(myEAF, sprintf("        <TIME_SLOT TIME_SLOT_ID=\"%s\" TIME_VALUE=\"%s\"/>", ann$TIME_SLOT_REF1, ann$startsec))
		myEAF <- append(myEAF, sprintf("        <TIME_SLOT TIME_SLOT_ID=\"%s\" TIME_VALUE=\"%s\"/>", ann$TIME_SLOT_REF2, ann$endsec))
		myEAF <- append(myEAF,         "    </TIME_ORDER>")
	} else {
		myEAF <- append(myEAF,         "    <TIME_ORDER>")
		myEAF <- append(myEAF,         "    </TIME_ORDER>")
	}
	
	#iterate through all tierNames
	if (nrow(t@tiers)>0) {
		tierNr <- 1
		#myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\"/>", t@tiers$name, sep="\n"))
		
		for (tierNr in 1:nrow(t@tiers))		{
			#--- get annotations within tier
			annotations.tier <- ann[ann$tierName==t@tiers$name[tierNr],]

			if (nrow(annotations.tier)==0) {
				#--- generate tier AND close
				myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\"/>", xml_escape(t@tiers$name[tierNr])))
				
			} else {
				#--- generate tier
				myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\">", xml_escape(t@tiers$name[tierNr])))
				
				if (t@tiers$type[tierNr] == "IntervalTier")
				{
				} else if (t@tiers$type[tierNr] == "TextTier") {
					#--- add end times
					annotations.tier$endsec <- annotations.tier$startsec + 100
				}
				
				#--- check for overlap of intervals, if there are more than one intervals
				if (nrow(annotations.tier)>1) 	{
					#get intervals whose endsec is bigger then the startsec of the following
					overlaps <- annotations.tier$endsec[1:nrow(annotations.tier)-1]>annotations.tier$startsec[2:nrow(annotations.tier)]
					
					#if there are
					if (any(overlaps==TRUE))
					{
						#get the indices of those intervals
						overlaps <- c(1:length(overlaps))[overlaps]
						
						#replace endsec with startsec of the following interval
						annotations.tier$endsec[overlaps]<- annotations.tier$startsec[overlaps+1]
					}
				}
				
				annotations <- paste(        "        <ANNOTATION>",
									 sprintf("            <ALIGNABLE_ANNOTATION ANNOTATION_ID=\"%s\" TIME_SLOT_REF1=\"%s\" TIME_SLOT_REF2=\"%s\">", annotations.tier$annotationID, annotations.tier$TIME_SLOT_REF1, annotations.tier$TIME_SLOT_REF2),
									 sprintf("                <ANNOTATION_VALUE>%s</ANNOTATION_VALUE>", xml_escape(annotations.tier$content) ),
									         "            </ALIGNABLE_ANNOTATION>",
									         "        </ANNOTATION>", sep="\n")
				
				myEAF <- append(myEAF, annotations)
				myEAF <- append(myEAF,         "    </TIER>")
			}
		}
	}
	
	#---
	myEAF <- append(myEAF, "    <LINGUISTIC_TYPE GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"praat\" TIME_ALIGNABLE=\"true\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Time subdivision of parent annotation\'s time interval, no time gaps allowed within this interval\" STEREOTYPE=\"Time_Subdivision\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Symbolic subdivision of a parent annotation. Annotations refering to the same parent are ordered\" STEREOTYPE=\"Symbolic_Subdivision\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"1-1 association with a parent annotation\" STEREOTYPE=\"Symbolic_Association\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Time alignable annotations within the parent annotation's time interval, gaps are allowed\" STEREOTYPE=\"Included_In\"/>")
	
	myEAF <- append(myEAF, "</ANNOTATION_DOCUMENT>")
	

	if (is.null(pathOutput)) 	{
		return(myEAF)
	} else {
		#---write to file as UTF-8 (matches encoding declared in XML header)
		myEAF <- stringr::str_flatten(myEAF, collapse="\n")
		myEAF <- enc2utf8(myEAF)
		fileConn <- file(pathOutput, open="wb")
		writeBin(charToRaw(myEAF), fileConn, endian="little")
		close(fileConn)
	}
}
