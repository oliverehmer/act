#' Import a single 'ELAN' '*.eaf' file
#' 
#' Advice: In most situations it is more convenient to use \code{act::corpus_new}, \code{act::corpus_import} for importing annotation files.
#'  
#' Imports the contents of a 'ELAN' .eaf file and returns a transcript object.
#' The input to this function is either the path to an .eaf file or the contents of a .eaf file obtained from the \code{@file.content} of an existing transcript object by \code{readLines()}.
#' If you pass 'fileContent' you need to pass 'transcriptName' as parameter, too.
#' 
#' Please note:
#' - 'ELAN' offers a variety of tier types, some including dependencies from other tiers. Therefore not all annotations do actually have a time value. Missing values will be detected in the superordinate tier or will be interpolated. You will not be able to recognize interpolated values in the annotations.  
#' - Please also note that dependencies between tiers in you .eaf file are not reflected in the transcript object within the act package.
#' 
#' @param filePath Character string; input path of a single 'ELAN' .eaf file.
#' @param fileContent Vector of character strings; contents of an 'ELAN' .eaf file read by \code{readLines()}.
#' @param transcriptName Character string; name of the transcript.
#' 
#' @return Transcript object.
#' 
#' @seealso \code{corpus_import}, \code{corpus_new}, \code{import}, \code{import_exb}, \code{import_rpraat}, \code{import_textgrid}  
#' 
#' @export
#'
#' @example inst/examples/import_eaf.R
#' 
import_eaf <- function(filePath=NULL, 
					   fileContent=NULL, 
					   transcriptName=NULL) {
	
	# filePath<- rstudioapi::selectFile()
	# fileContent<-NULL
	# transcriptName<-NULL
	if (is.null(filePath) & is.null(fileContent)) {
		stop("You need to pass as parameter eiter a file path to a TextGrid file (filePath) or the contents of a TextGrid file (fileContent) as parameter.")
	}
	if (!is.null(filePath) & !is.null(fileContent)) {
		stop("Please pass only filePath or fileContent as parameter, not both.")
	}
	if (!is.null(fileContent) & is.null(transcriptName)) {
		stop("If you pass 'fileContent' you need to pass 'transcriptName' as parameter, too.")
	}

	#--- get transcript name
	t 					<- methods::new("transcript")
	t@file.path 			<- if(is.null(filePath)) {""} else {filePath}
	
	#--- get transcript name
	if (!is.null(transcriptName)) {
		t@name <- transcriptName
	} else {
		if(!is.null(filePath)) {
			t@name <- tools::file_path_sans_ext(basename(filePath))			
		} else {
			t@name <- "imported transcript"
		}
	}
	
	t@file.type 			 <- "eaf"
	t@file.encoding          <- "UTF-8"
	t@import.result 		 <- "ok"
	t@load.message 	         <- ""
	t@modification.systime   <- character()
	#t@annotations				<- .emptyAnnotations

	if (!is.null(filePath)) {
		#--- check if file exists
		if (!file.exists(filePath)) {
			t@import.result 		<- "error"
			t@load.message   <- "File does not exist."
			return(t)
		}
		
		#=== READ lines
		myCon <- file(filePath, encoding = "UTF-8")
		myeaf <- readLines(myCon, warn=FALSE)
		close(myCon)
		t@file.encoding <- "UTF-8"
		if (!length(options()$act.import.storeFileContentInTranscript)==0) {
			if (options()$act.import.storeFileContentInTranscript==TRUE) {
				t@file.content <- myeaf
			}
		}
	}
	if (!is.null(fileContent)) {
		myeaf <- fileContent 
	}
	
	if(is.null(myeaf)) 	{
		t@import.result 		<- "error"
		t@load.message   <- "File not read."
		return(t)
	} 
	
	#=== merge lines into a long text
	myeaf.merge <- stringr::str_c(myeaf, collapse = "\n")
	if (!stringr::str_detect(myeaf.merge, pattern="http://www.mpi.nl/tools/elan/EAFv3.0.xsd" )) {
		t@import.result 		<- "error"
		t@load.message   <- "File not recognized as eaf file version EAFv3.0.xsd"
		return(t)
	}
	#remove a strange unicode character
	myeaf.merge <- stringr::str_replace_all(myeaf.merge, pattern='\\x1B', replacement='')
	
	#== extract media links
	regex_media <- '(?s)(?<=\\bMEDIA_URL=").+?(?=")'
	medialinks <- stringr::str_match_all(myeaf.merge, regex_media)
	medialinks <- unique(unlist(medialinks))
	medialinks <- stringr::str_replace_all(string=medialinks, pattern="file:///", replacement="")
	
	#=== extract linguistic_types and constraints
	regex_constraints <- '(?s)(<LINGUISTIC_TYPE)(.*?CONSTRAINTS="(.*?)")(.*?LINGUISTIC_TYPE_ID="(.*?)")(.*?/>)'
	constraints <- stringr::str_match_all(myeaf.merge, regex_constraints)
	constraints <- do.call(rbind, lapply(constraints, data.frame, stringsAsFactors=FALSE))
	constraints <- data.frame(cbind(type.id=constraints[,6], 
									constraint=constraints[,4]), 
							  stringsAsFactors=FALSE)
	if (nrow(constraints)==0) {
		constraints <- data.frame(type.id=as.character("no-constraint"), constraint=as.character("No_Constraint"), stringsAsFactors=FALSE)
	} else {
		constraints <- rbind(constraints, c(type.id="no-constraint",constraint="No_Constraint"), stringsAsFactors=FALSE)
	}
	#View(constraints)
	
	#== extract time slots
	regex_timeorder <- '(?s)<TIME_ORDER.+?/TIME_ORDER>'
	timeorder <- unlist(unlist(stringr::str_match(myeaf.merge, regex_timeorder)))
	regex_timeslots <- '(?s)(<TIME_SLOT.*?TIME_SLOT_ID="(.*?)")(.*?)(TIME_VALUE="(.*?)")?(/>)'
	timeslots <- stringr::str_match_all(timeorder, regex_timeslots)
	timeslots <- do.call(rbind, lapply(timeslots, data.frame, stringsAsFactors=FALSE))
	#View(timeslots)
	timeslots <- data.frame(cbind(timeSlotID=timeslots[,3], value=timeslots[,6]), stringsAsFactors=FALSE)
	timeslots$value <- as.double(timeslots$value)/1000
	#View(timeslots)
	
	#=== extract annotations
	#Split the entire eaf
	#get position of beginning of "<tier"
	pos.tier <- data.frame(stringr::str_locate_all(myeaf.merge, pattern="<TIER")[[1]],stringsAsFactors=FALSE )
	annotations.block <- c()
	if (nrow(pos.tier)>0) {
		pos.lingtype <- stringr::str_locate(myeaf.merge, pattern="<LINGUISTIC_TYPE")[1,1]
		if (nrow(pos.tier)==1) {
			annotations.block <- unlist(stringr::str_sub(myeaf.merge, pos.tier$start, pos.lingtype-1))
		} else {
			repl <- c(pos.tier$start[2:nrow(pos.tier)], pos.lingtype)
			pos.tier$end <- repl-1
			annotations.block <- unlist(stringr::str_sub(myeaf.merge, pos.tier$start, pos.tier$end))
		}
		#print(stringr::str_sub(annotations.block,1,200))
		#length(annotations.block)
		#cat(annotations.block)
	}
	
	#=== extract tier info
	tier.ling_type_ref <- '(<TIER)(.*LINGUISTIC_TYPE_REF="(.*?)")?'
	tier.ling_type_ref <- stringr::str_match_all(annotations.block, tier.ling_type_ref)
	tier.ling_type_ref <- do.call(rbind, lapply(tier.ling_type_ref, data.frame, stringsAsFactors=FALSE))
	tier.ling_type_ref <- tier.ling_type_ref[,4]
	#View(tier.ling_type_ref)
	
	regex_name <- '(?s)(<TIER)(.*TIER_ID="(.*?)")?'
	tier.name <- stringr::str_match_all(annotations.block, regex_name)
	tier.name <- do.call(rbind, lapply(tier.name, data.frame, stringsAsFactors=FALSE))
	tier.name <- tier.name[,4]
	#View(tier.name)
	
	regex_tierparent_ref <- '(?s)(<TIER)(.*PARENT_REF="(.*?)")?'
	tier.parent_ref <- stringr::str_match_all(annotations.block, regex_tierparent_ref)
	tier.parent_ref <- do.call(rbind, lapply(tier.parent_ref, data.frame, stringsAsFactors=FALSE))
	tier.parent_ref <- tier.parent_ref[,4]
	#View(tier.parent_ref)
	
	if (is.null (tier.name)) {
		#if there are no tiers in the file
		tiers <- data.frame(tier.name=as.character(), 
							tier.type.ref=as.character(), 
							tier.parent.ref=as.character(), 
							tier.hierarchy=as.character(),
							stringsAsFactors = FALSE)
		
	} else {
		
		tiers <- data.frame(cbind(tier.name=tier.name, 
									 tier.type.ref=tier.ling_type_ref, 
									 tier.parent.ref=tier.parent_ref, 
									 tier.hierarchy=NA),
							stringsAsFactors = FALSE)
		#View(tiers)
		
		#construct the hierarchy of the tiers
		tiers$tier.hierarchy[is.na(tiers$tier.parent.ref)]<- 1
		hierarchy <- 0
		
		for (i in 1:nrow(tiers)) {
			#exit loop if all tiers have already a place in the hierarcy   
			if (!any(is.na(tiers$tier.hierarchy))) {
				break
			}
			hierarchy <- hierarchy+1
			tiernames.currentlevel <- tiers$tier.name[which(tiers$tier.hierarchy==hierarchy)]
			if (length(tiernames.currentlevel)!=0) {
				for (j in 1:length(tiernames.currentlevel)) {
					tiernames.currentlevel
					tiers$tier.hierarchy[which(tiers$tier.parent.ref==tiernames.currentlevel[j])] <- hierarchy+1
				}
			}
		}
		#View(tiers)
		
		#=get the actual constraint of the tier
		#remember the order
		tiers$order <- 1:nrow(tiers)
		#merge with constraints
		tiers <- merge(x = tiers, y = constraints, by.x = "tier.type.ref", by.y="type.id", all.x = TRUE)
		#restore initial order
		tiers <- tiers[order(tiers$order),]
		tiers$constraint[is.na(tiers$constraint)]<- "No_Constraint"
		#View(constraints)
		#View(tiers.m)
		
		#---create unique tierNames
		#	while (length(tiers$tier.name[duplicated(tiers$tier.name)])>0)  	{
		#		#get multiple tierNames
		#		multiple <- tiers$tier.name[duplicated(tiers$tier.name)]
		#		for (myName in multiple) {
		#			tiers$tier.name[tiers$tier.name==myName]<- paste(tiers$tier.name[tiers$tier.name==myName],1:length(tiers$tier.name[tiers$tier.name==myName]),sep="-")
		#		}
		#		t@import.result 		<- "ok"
		#		t@load.message   <- "Some tiers have been renamed since their names were not unique."
		#}
	
		#---create unique tierNames
		if (length(tiers$tier.name[duplicated(tiers$tier.name)])>0) {
			tiers$tier.name <- make.unique(tiers$tier.name)
			t@import.result 		<- "ok"
			t@load.message   <- "Some tiers have been renamed since their names were not unique."
		}
		
			
		annotations <- data.frame()
		if (nrow(tiers)>0) {
			#--- iterate through tiers and extract info
			# Those are the difficult referring types:
			regex_ann_referring <- '(?s)(<REF_ANNOTATION)(.ANNOTATION_ID="(.*?)")(.ANNOTATION_REF="(.*?)")(.PREVIOUS_ANNOTATION="(.*?)")?(.*?<ANNOTATION_VALUE>(.*?)</ANNOTATION_VALUE>)'
			constraints_ann_referring <- c("Symbolic_Association",    "Symbolic_Subdivision")
			
			#These are the other types (there might be more), but this is not checked: 
			regex_ann_alignable <- '(?s)(<ALIGNABLE_ANNOTATION)(.*?ANNOTATION_ID="(.*?)")(.*?TIME_SLOT_REF1="(.*?)")(.*?TIME_SLOT_REF2="(.*?)">)(.*?<ANNOTATION_VALUE>(.*?)</ANNOTATION_VALUE>)'
			constraints_ann_alignable <- c("Time_Subdivision",    "Included_In", "No_Constraint")
			
			#i <- 1
			#tiers[i,]
			#annotations.block[i]
			#i <- 3
			#cat(annotations.block[i])
			#View(tiers)
			for (i in 1:nrow(tiers)) {
				#get the values of the annotations, using different regular expressions
				
				if (tiers$constraint[i] %in% constraints_ann_referring) {
					myRegEx <- regex_ann_referring
					annotations.tier <- stringr::str_match_all(annotations.block[i], myRegEx)
					annotations.tier <- do.call(rbind, lapply(annotations.tier, data.frame, stringsAsFactors=FALSE))
					#View(annotations.tier)
					if (nrow(annotations.tier)>0) {
						annotations.tier <- data.frame(cbind(         tier=tiers$tier.name[i], 
														   type=tiers$tier.type[i],
														   hierarchy=tiers$tier.hierarchy[i],
														   id=annotations.tier[, 4], 
														   content=annotations.tier[, 10], 
														   ts1=NA,
														   ts2=NA, 
														   annotation.ref=annotations.tier[, 6], 
														   annotation.previous=annotations.tier[, 8]),
													   stringsAsFactors=FALSE)
					}
				} else {
					myRegEx <- regex_ann_alignable
					annotations.tier <- stringr::str_match_all(annotations.block[i], myRegEx)
					annotations.tier <- do.call(rbind, lapply(annotations.tier, data.frame, stringsAsFactors=FALSE))
					#View(annotations.tier)
					if (nrow(annotations.tier)>0) {
						annotations.tier <- data.frame(cbind(         tier=tiers$tier.name[i], 
														   type=tiers$tier.type[i],
														   hierarchy=tiers$tier.hierarchy[i],
														   id=annotations.tier[, 4], 
														   content=annotations.tier[, 10], 
														   ts1=annotations.tier[, 6],
														   ts2=annotations.tier[, 8], 
														   annotation.ref=NA, 
														   annotation.previous=NA),
													   stringsAsFactors=FALSE)
					}
				}
				if (nrow(annotations.tier)>0) {
					if (nrow(annotations)>0) {
						annotations <- rbind(annotations, annotations.tier)
					} else {
						annotations <-  annotations.tier
					}
				}
			}
			#View(annotations)
			#View(annotations.tier)
			
			#there are no annotations
			if (nrow(annotations)==0) {
				
			} else {
				
				#===replace ts IDs by real numbers
				# remember the order of the annotations
				annotations$order <- 1:nrow(annotations)
				
				#ts1
				annotations <- merge(x = annotations, y = timeslots, by.x = "ts1", by.y="timeSlotID", all.x = TRUE)
				mycolnames <- colnames(annotations)
				mycolnames[length(mycolnames)] <- "startSec"
				colnames(annotations) <- mycolnames
				
				#ts2
				annotations <- merge(x = annotations, y = timeslots, by.x = "ts2", by.y="timeSlotID", all.x = TRUE)
				mycolnames <- colnames(annotations)
				mycolnames[length(mycolnames)] <- "endSec"
				colnames(annotations) <- mycolnames
				
				#restore order!
				annotations <- annotations[order(annotations$order),]
				#View(annotations)
				annotations.safe <- annotations
				
				#=== resolve referring annotations
				allrefids <- unique(annotations$annotation.ref)
				allrefids <- allrefids[!is.na(allrefids)]
				tiers.copy <- tiers[order(tiers$tier.hierarchy), ]
				all_tiers <- tiers.copy$tier.name
				#all_tiers <- unique(annotations$tier[which(annotations$annotation.ref!="")]) 
				
				#=mark which are the first and the last annotations in a group
				annotations <- cbind(annotations, position=NA)
				if (nrow(annotations)==1) {
					annotations$position[1] <- 1 # "first"
				} else {
					annotations$position[!is.na(annotations$annotation.ref) & is.na(annotations$annotation.previous)] <- 1# "first"
				}
				
				firstannotations <- which(annotations$position=="1") #"first"
				for (i in firstannotations) {
					counter <- 1
					lastannotation <- NA
					#print ("..")
					#print(i)
					#take their id and search for the next
					search <- annotations$id[i]
					#do a long loop
					for (j in 1:10) {
						#print(j)
						#get the annotation that is referred to
						nextannotation <- which(annotations$annotation.previous==search)
						if (length(nextannotation)==0) {
							if (!is.na(lastannotation)) {
								#	annotations$position[lastannotation]<-"last"
							}
							break
						} else {
							annotations$annotation.previous[nextannotation]<-"found"
							counter <- counter+1
							annotations$position[nextannotation] <- counter
							search <- annotations$id[nextannotation]
							#remember the last annotation
							lastannotation <- nextannotation
						}
						
					}
				}
				#View(annotations)
				annotations.safe <- annotations
				
				#iterate through all tiers with referring annotations tiers
				i <- 2
				#all_tiers[i]
				j <- 1
				#allrefids[j]
				#order / sort by tiers, annotarion referred to and Position()
				annotations <- annotations[order(annotations$tier, annotations$annotation.ref, annotations$position),]
				#View(annotations)
				for (i in 1:length(all_tiers)) {
					for (j in 1:length(allrefids)) {
						#cat ("i",i, " j",j, "\n")
						#cat(all_tiers[i],"\n" )
						#cat(allrefids[j],"\n" )
						
						ids <- which(annotations$tier==all_tiers[i] & annotations$annotation.ref==allrefids[j])
						
						if (length(ids)==1) {
							#if there is only one referring annotation
							#--> simply take the time values of the annotation referred to
							ann.referred.to <- annotations[which(annotations$id==allrefids[j])[1], ]
							annotations[ids, ]$startSec <- ann.referred.to$startSec
							annotations[ids, ]$endSec <-   ann.referred.to$endSec
							annotations[ids, ]$ts1 <- "fromsuperordinated"
						} 
						if (length(ids)>1) {
							
							#-- first annotation in group
							#--> split up the time values 
							ann.referred.to <- annotations[which(annotations$id==allrefids[j])[1], ]
							minSec <- ann.referred.to$startSec
							maxSec <- ann.referred.to$endSec
							
							newTimes		<- seq(from=minSec, to=maxSec, length.out=length(ids)+1)
							newTimes.start	<- newTimes[1:length(newTimes)-1]
							newTimes.end	<- newTimes[2:length(newTimes)]
							
							annotations[ids, ]$startSec <- newTimes.start
							annotations[ids, ]$endSec <- newTimes.end
							annotations[ids, ]$ts1 <- "interpolated"
						}
					}
				}
				
				#restore order()
				annotations <- annotations[order(annotations$order),]
				#View(annotations)
				#View(tiers)
				
				#=== resolve empty time slots of annotations
				#run through all annotations
				i <- 4
				for (m in 1:nrow(tiers)) {
					if (nrow(annotations)>1) {
	
						for (i in 1:(nrow(annotations)-1)) {
							#print(i)
							#check if the end value is empty
							if (is.na(annotations$endSec[i])) {
								#if we are lucky, the following annotation has a start value and we are done
								if (!is.na(annotations$startSec[i+1])) {
									annotations$endSec[i] <- annotations$startSec[i+1]
								}
								
								#otherwise search for the next end value
								counter <- 0
								for (j in i:nrow(annotations)) {
									if (!is.na(annotations$endSec[j])) {
										break
									}
								}
								#the span that we need to calculate is this from: i startsec to j endsec
								#how many annotations are spanned:  j-i+1
								# we this need even one more timevalue for the sequence: j-i+2
								if (!is.na(annotations$startSec[i]) & !is.na(annotations$endSec[j])) {
									newTimes		<- seq(from=annotations$startSec[i], to=annotations$endSec[j], length.out=j-i+2)
									newTimes.start	<- newTimes[1:length(newTimes)-1]
									newTimes.end	<- newTimes[2:length(newTimes)]
									
									#assign new time values to annotations
									l <- 0
									for (k in i:j) {
										l <- l+1
										annotations$startSec[k] <- newTimes.start[l]
										annotations$endSec[k] <- newTimes.end[l]
										annotations[k, ]$ts1 <- "interpolated"
										annotations[k, ]$ts2 <- "interpolated"				
									}					
								}
							}
						}					

					}
					if (!any(is.na(c(annotations$startSec, annotations$endSec)))) {
						break
					}
				}
			}
		}
		#View(annotations)
		
		if (nrow(annotations)==0) {
			#empty myAnnotations is already set at the beginning
			myAnnotations <- data.frame()
		} else {
			annotationID <- c(1:nrow(annotations))
			myAnnotations <- data.frame(
				annotationID  			= as.integer(annotationID),
				
				tier.name  				= annotations$tier,
				startSec  				= as.double(annotations$startSec),
				endSec  				= as.double(annotations$endSec),
				content  				= as.character(annotations$content),
				
				content.norm  			= as.character(""),
				char.orig.bytime.start  = rep(as.integer(NA),nrow(annotations)),
				char.orig.bytime.end 	= rep(as.integer(NA),nrow(annotations)),
				char.norm.bytime.start 	= rep(as.integer(NA),nrow(annotations)),
				char.norm.bytime.end 	= rep(as.integer(NA),nrow(annotations)),
				char.orig.bytier.start  = rep(as.integer(NA),nrow(annotations)),
				char.orig.bytier.end  	= rep(as.integer(NA),nrow(annotations)),
				char.norm.bytier.start  = rep(as.integer(NA),nrow(annotations)),
				char.norm.bytier.end  	= rep(as.integer(NA),nrow(annotations)),
				row.names 				= annotationID, 
				stringsAsFactors		= FALSE)
			rownames(myAnnotations) 	<- myAnnotations$annotationID
			
			#===set correct column format
			t@annotations$annotationID	<- as.integer(t@annotations$annotationID)
			t@annotations$startSec		<- as.double(t@annotations$startSec)
			t@annotations$endSec  		<- as.double(t@annotations$endSec)
			t@annotations$content  		<- as.character(t@annotations$content)
			
		}
		
		#===set transcript length
		t@length.sec <- 0
		
		#=== if it is not a completely empty transcript
		if (!nrow(myAnnotations)==0)  	{
			#=== get rid of empty intervals
			if (options()$act.import.readEmptyIntervals==FALSE) 		{
				myAnnotations <- myAnnotations[myAnnotations$content!="",]
			}
			myAnnotations <- myAnnotations[is.na(myAnnotations["content"])==FALSE,]
			
			if (nrow(myAnnotations)>0) 		{
				#=== sort transcript by start times
				myAnnotations <- myAnnotations[order(myAnnotations$startSec, myAnnotations$tier.name), ]
				
				#=== set endSec of points to startSec
				myAnnotations$endSec[is.na(myAnnotations$endSec)] <- myAnnotations$startSec[is.na(myAnnotations$endSec)]
				
				#=== set annotations.id again
				myAnnotations$annotationID <- c(1:nrow(myAnnotations))
				
				#=== set the new row names
				rownames(myAnnotations) <- myAnnotations$annotationID
				#View(myAnnotations)
			}
			#make transcript 1 sec longer than last annotation
			t@length.sec <- max( t@length.sec, as.double(annotations$startSec)+1, as.double(annotations$endSec)+1)
			
			#=== html conversion
			myAnnotations$content      <- textutils::HTMLdecode(myAnnotations$content)
			myAnnotations$tier.name    <- textutils::HTMLdecode(myAnnotations$tier.name)
			
		}
		#=== html conversion
		tiers$tier.name     	   <- textutils::HTMLdecode(tiers$tier.name)
	}
	
	#=== assign other values to object
	#annotations
	t@annotations 	<- myAnnotations
	
	#media
	t@media.path <- medialinks
	
	#tiers: all are interval tiers 
	t@tiers <- act::helper_tiers_new_table(tierNames=tiers$tier.name)
	
	t@history <- list( 
				   list(modification                               = "import_eaf",
				   	 systime                                       = Sys.time()
				   )
	             )
	
	return(t)
}

