#' Merge several transcripts (works with transcript objects directly)
#' 
#' Merges several transcripts. One transcript is the destination transcript (the transcript that will be updated). 
#' The other transcripts are the update transcripts and contain the updates. 
#' The update transcripts need to contain a tier in which the update sections are marked with a specific character string.
#' 
#' You may chose between the following two options:
#' - The update sections in the destination transcript will first be erased completely and then the updates will be filled in.
#' - The update sections in the destination transcript will NOT be erased completely. Rater only the contents of tiers will be erased that are also present in the update tiers. e.g. if your destination transcript contains more tiers than the update transcripts, the contents of those tiers will be preserved in the destination tier during the update.
#' 
#' @param destinationTranscript Transcript object; transcript that serves as destination (and will receive the updates).
#' @param updateTranscripts List of transcript objects; transcript objects that will be inserted into the destination transcripts (entirely or in part).
#' @param identifierTier Character string;  regular expression that identifies the tier in which the sections are marked, that will be inserted into destinationTranscript.
#' @param identifierPattern Character string; regular expression that identifies the sections that will be inserted into destinationTranscript.
#' @param eraseCompletely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the updateTranscripts be erased.
#' @param keepOld Logical; if \code{TRUE} annotations that are replaced during the merge are preserved in \code{OLD_} prefixed tiers (e.g. \code{OLD_TeaF}). Default is \code{FALSE}.
#' @param insertCheckTier Logical; if \code{TRUE} a \code{CHECK-update} tier is inserted with marker annotations at positions where annotations were truncated. Default is \code{FALSE}. Cut annotations are always reported via CLI regardless of this setting.
#' @param healCuts Logical; if \code{TRUE} truncated annotations at the boundaries of update regions are automatically joined with adjacent annotations that have the same content. Default is \code{FALSE}.
#'
#' @return Transcript object
#' 
#' @seealso \link{transcript_update}
#' 
#' @export
#'
#' @example inst/examples/transcripts_merge.R
#' 
transcripts_merge <- function (destinationTranscript,
								updateTranscripts,
								identifierTier        = "update",
								identifierPattern     = ".+",
								eraseCompletely       = TRUE,
								keepOld               = FALSE,
								insertCheckTier       = FALSE,
								healCuts              = FALSE) {


		#destinationTranscript 	<- x@transcripts[['update_destination']]
		#destinationTranscript 	<- x@transcripts['update_destination']
		#updateTranscripts 		<- x@transcripts[c('update_update1', 'update_update2')]
		#updateTranscripts 		<- list(x@transcripts[['update_update1']], x@transcripts[['update_update2']])
		#updateTranscripts		<- x@transcripts[['update_update1']]
		#updateTranscripts		<- x@transcripts['update_update1']
		#identifierTier <- "update"
		#identifierPattern <- ".+"
		#eraseCompletely <- TRUE
		#pathOverview <- "/Users/oliverehmer/Library/CloudStorage/OneDrive-Persönlich/Corpus/corpus_work/update/out_destination_updated/tier_comparison_update_destination.xlsx"

	
	#x<-corpus
	#destinationTranscriptName <- 'destination'
	#destinationTranscript <- x@transcripts[[destinationTranscriptName]]
	
	#updateTranscriptNames <- c('A')
	#updateTranscripts <- x@transcripts[ids]
	
	#identifierTier='status-progress'
	#identifierPattern=".+"
	#eraseCompletely<-TRUE
	
	
	if (missing(destinationTranscript)) 	{cli::cli_abort("Transcript object in parameter {.arg destinationTranscript} is missing.") 	}
	if (missing(updateTranscripts)) 	{cli::cli_abort("Transcript object(s) in parameter {.arg updateTranscripts} is/are missing.") 	}
	
	#check: destinationTranscript
	if(typeof(destinationTranscript)=="list") {
		if (length(destinationTranscript)>1) {
			cli::cli_abort("Parameter {.arg destinationTranscript} may only be a single transcript object.")
		}
		destinationTranscript <- destinationTranscript[[1]]
	}
	if (!methods::is(destinationTranscript,"transcript")) 	{
		cli::cli_abort("Parameter {.arg destinationTranscript} needs to be a {.cls transcript} object.")
	}
	#--> result should be a single Object as accessed by [[]]
	
	#check: updateTranscripts
	if(typeof(updateTranscripts)!="list") {
		updateTranscripts <- list(updateTranscripts)
	}
	allAreTransripts <- TRUE
	for (transUpdate in updateTranscripts) {
		if(!methods::is(transUpdate,"transcript")) {
			allAreTransripts <- FALSE
		}
	}
	if (!allAreTransripts) 	{
		cli::cli_abort("Parameter {.arg updateTranscripts} may only contain transcript objects.")
	}
	#--> result should be a list
	
	#--- helper_tiers_merge_tables
	tierTable <- act::helper_tiers_merge_tables(destinationTranscript,updateTranscripts )
	
	#--- sort table with identifier tier in position 1	
	sortVector <- unique(c(identifierTier, tierTable$name))
	tierTable  <- act::helper_tiers_sort_table( tierTable = tierTable, 
												sortVector = sortVector 
	)
	
	#--- tiers to object 
	destinationTranscript@tiers <- tierTable
	
	#--- get annotations of destinaton transcript
	ann <- destinationTranscript@annotations

	#--- collector for cut annotations
	cut_info <- data.frame(time = numeric(), side = character(), tiers = character(),
	                       region = integer(), stringsAsFactors = FALSE)
	region_counter <- 0L

	#for all transcripts in updates
	for (transUpdate in updateTranscripts) {
		
		#skip transcript if update is empty
		#get all annotations
		myDestIntervals <- transUpdate@annotations
		
		#if tier identifier is specified, get first tiers that fits condition
		if (!is.null(identifierTier)) {
			#get all intervals in this tier
			myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$tierName, identifierTier), ]
			
			#if a specifier in this tier is set,  select those that fit condition
			if (!is.null(identifierPattern)) {
				myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$content, identifierPattern), ]
			}
		} else {
			#auto create an update interval, starting with first and ending with last interval
			startsec <- min(myDestIntervals$startsec)
			endsec <- max(myDestIntervals$endsec)
			myDestIntervals <- myDestIntervals[1:1,]
			
			myDestIntervals$startsec[1] 	<- startsec
			myDestIntervals$endsec[1] 	<- endsec
		}
		
		if(nrow(myDestIntervals)>0) {
			#run through all update intervalls
			for (i in 1:nrow(myDestIntervals)) {
				myDestInterval <- myDestIntervals[i,]
				region_counter <- region_counter + 1L

				#get annotations form the update transcript
				annUpdate <- transUpdate@annotations
				annUpdate <- annUpdate[((annUpdate$startsec>=myDestInterval$startsec & annUpdate$startsec<=myDestInterval$endsec) | (annUpdate$endsec>myDestInterval$startsec & annUpdate$endsec<=myDestInterval$endsec) | (annUpdate$startsec<myDestInterval$startsec & annUpdate$endsec>myDestInterval$endsec)), ]
				
				#---truncate intervals that do no fall entirely into the intervall
				#intervals that start before
				for (j in 1:nrow(annUpdate)) {
					annUpdate$startsec[j] <- max(annUpdate$startsec[j], myDestInterval$startsec)
				}
				
				#intervals that end after
				for (j in 1:nrow(annUpdate)) {
					annUpdate$endsec[j] <- min(annUpdate$endsec[j], myDestInterval$endsec)
				}
				
				#destination
				if (eraseCompletely==TRUE) {
					#for for all tiers

					#split annotations that span the entire update region
					span_ids <- which(ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$endsec & !startsWith(ann$tierName, "OLD_"))
					if (length(span_ids) > 0) {
						after_ann <- ann[span_ids, ]
						after_ann$startsec <- myDestInterval$endsec
						max_id <- max(as.integer(ann$annotationID), na.rm = TRUE)
						after_ann$annotationID <- seq(max_id + 1L, max_id + nrow(after_ann))
						ann$endsec[span_ids] <- myDestInterval$startsec
						ann <- dplyr::bind_rows(ann, after_ann)
					}

					#identify annotations in the update region
					in_region <- which(ann$startsec>=myDestInterval$startsec & ann$endsec<=myDestInterval$endsec)

					if (keepOld && length(in_region) > 0) {
						old_ann <- ann[in_region, ]
						old_ann$tierName <- paste0("OLD_", old_ann$tierName)
						ann <- ann[-in_region, ]
						ann <- dplyr::bind_rows(ann, old_ann)
					} else {
						ann <- ann[ann$startsec<myDestInterval$startsec | ann$endsec>myDestInterval$endsec,]
					}

					#truncate intervals that reach into the update area
					cut_start_ids <- which(ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$startsec & !startsWith(ann$tierName, "OLD_"))
					if (length(cut_start_ids) > 0) {
						cut_tiers_start <- paste(unique(ann$tierName[cut_start_ids]), collapse = ", ")
						cut_info <- rbind(cut_info, data.frame(time = myDestInterval$startsec, side = "start", tiers = cut_tiers_start, region = region_counter, stringsAsFactors = FALSE))
						ann$endsec[cut_start_ids] <- myDestInterval$startsec
					}

					#truncate intervals that reach out of the update area
					cut_end_ids <- which(ann$startsec<myDestInterval$endsec & ann$endsec>myDestInterval$endsec & !startsWith(ann$tierName, "OLD_"))
					if (length(cut_end_ids) > 0) {
						cut_tiers_end <- paste(unique(ann$tierName[cut_end_ids]), collapse = ", ")
						cut_info <- rbind(cut_info, data.frame(time = myDestInterval$endsec, side = "end", tiers = cut_tiers_end, region = region_counter, stringsAsFactors = FALSE))
						ann$startsec[cut_end_ids] <- myDestInterval$endsec
					}

				} else {
					#for tiers that are in update transcript

					#split annotations that span the entire update region (only in update tiers)
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_span <- which(ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$endsec & !startsWith(ann$tierName, "OLD_"))
					span_ids <- intersect(myIDs_tiers, myIDS_span)
					if (length(span_ids) > 0) {
						after_ann <- ann[span_ids, ]
						after_ann$startsec <- myDestInterval$endsec
						max_id <- max(as.integer(ann$annotationID), na.rm = TRUE)
						after_ann$annotationID <- seq(max_id + 1L, max_id + nrow(after_ann))
						ann$endsec[span_ids] <- myDestInterval$startsec
						ann <- dplyr::bind_rows(ann, after_ann)
					}

					#get IDs of annotations in tiers that are also in the update file
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec>myDestInterval$startsec & ann$endsec<myDestInterval$endsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)

					if (keepOld && length(myIDs_tiers_timesection) > 0) {
						old_ann <- ann[myIDs_tiers_timesection, ]
						old_ann$tierName <- paste0("OLD_", old_ann$tierName)
						ann <- ann[-myIDs_tiers_timesection, ]
						ann <- dplyr::bind_rows(ann, old_ann)
					} else if (length(myIDs_tiers_timesection)>0) {
						ann <- ann[-myIDs_tiers_timesection,]
					}
					#truncate intervals that reach into the update area
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec<myDestInterval$startsec & ann$endsec>myDestInterval$startsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					if (length(myIDs_tiers_timesection) > 0) {
						cut_tiers_start <- paste(unique(ann$tierName[myIDs_tiers_timesection]), collapse = ", ")
						cut_info <- rbind(cut_info, data.frame(time = myDestInterval$startsec, side = "start", tiers = cut_tiers_start, region = region_counter, stringsAsFactors = FALSE))
					}
					ann$endsec[myIDs_tiers_timesection] <- myDestInterval$startsec

					#truncate intervals that reach out of the update area
					myIDs_tiers <- which(ann$tierName %in% transUpdate@tiers$name)
					myIDS_times <- which(ann$startsec<myDestInterval$endsec & ann$endsec>myDestInterval$endsec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					if (length(myIDs_tiers_timesection) > 0) {
						cut_tiers_end <- paste(unique(ann$tierName[myIDs_tiers_timesection]), collapse = ", ")
						cut_info <- rbind(cut_info, data.frame(time = myDestInterval$endsec, side = "end", tiers = cut_tiers_end, region = region_counter, stringsAsFactors = FALSE))
					}
					ann$startsec[myIDs_tiers_timesection] <- myDestInterval$endsec
				}
				
				#merge update and destination transcript
				ann <- dplyr::bind_rows(ann, annUpdate)

				# ===== HEAL CUTS =====
				if (healCuts) {
					healed_items <- character()

					all_tiers <- unique(ann$tierName[!startsWith(ann$tierName, "OLD_") & ann$tierName != "CHECK-update"])
					for (tn in all_tiers) {
						healed_side <- character()

						# Left: annotation ending at start, update annotation starting at start
						left_dest <- which(ann$tierName == tn & abs(ann$endsec - myDestInterval$startsec) < 0.001)
						left_upd  <- which(ann$tierName == tn & abs(ann$startsec - myDestInterval$startsec) < 0.001)
						if (length(left_dest) == 1 && length(left_upd) == 1) {
							if (trimws(ann$content[left_dest]) == trimws(ann$content[left_upd])) {
								ann$endsec[left_dest] <- ann$endsec[left_upd]
								ann <- ann[-left_upd, ]
								healed_side <- c(healed_side, "left")
							}
						}

						# Right: update annotation ending at end, dest annotation starting at end
						right_upd  <- which(ann$tierName == tn & abs(ann$endsec - myDestInterval$endsec) < 0.001)
						right_dest <- which(ann$tierName == tn & abs(ann$startsec - myDestInterval$endsec) < 0.001)
						if (length(right_upd) == 1 && length(right_dest) == 1) {
							if (trimws(ann$content[right_upd]) == trimws(ann$content[right_dest])) {
								ann$endsec[right_upd] <- ann$endsec[right_dest]
								ann <- ann[-right_dest, ]
								healed_side <- c(healed_side, "right")
							}
						}

						if (length(healed_side) > 0) {
							side_label <- paste(healed_side, collapse = "&")
							healed_items <- c(healed_items, paste0(tn, " (", side_label, ")"))
							cli::cli_alert_success("Annotation(s) joined: {tn} ({side_label}) at {round(myDestInterval$startsec, 1)}s-{round(myDestInterval$endsec, 1)}s")

							for (hs in healed_side) {
								ci_side <- if (hs == "left") "start" else "end"
								ci_rows <- which(cut_info$region == region_counter & cut_info$side == ci_side)
								for (cr in ci_rows) {
									remaining <- setdiff(strsplit(cut_info$tiers[cr], ", ")[[1]], tn)
									if (length(remaining) == 0) {
										cut_info <- cut_info[-cr, ]
									} else {
										cut_info$tiers[cr] <- paste(remaining, collapse = ", ")
									}
								}
							}
						}
					}

					if (insertCheckTier && length(healed_items) > 0) {
						heal_content <- paste0("Annotation(s) joined: ", paste(healed_items, collapse = ", "))
						heal_ann <- data.frame(
							annotationID = as.integer(max(as.integer(ann$annotationID), na.rm = TRUE) + 1L),
							tierName = "CHECK-update",
							startsec = myDestInterval$startsec,
							endsec = myDestInterval$endsec,
							content = heal_content,
							content.norm = "",
							char.orig.bytime.start = NA_integer_, char.orig.bytime.end = NA_integer_,
							char.norm.bytime.start = NA_integer_, char.norm.bytime.end = NA_integer_,
							char.orig.bytier.start = NA_integer_, char.orig.bytier.end = NA_integer_,
							char.norm.bytier.start = NA_integer_, char.norm.bytier.end = NA_integer_,
							stringsAsFactors = FALSE
						)
						ann <- dplyr::bind_rows(ann, heal_ann)
					}
				}

			}
		}
	}
	#assign new annotations to transcript
	destinationTranscript@annotations <- ann

	#add CHECK-update and OLD_ tiers at the TOP using helper_tiers_sort_table
	top_tiers <- character()
	if ("CHECK-update" %in% ann$tierName) {
		top_tiers <- c(top_tiers, "CHECK-update")
	}
	old_tier_names <- unique(ann$tierName[startsWith(ann$tierName, "OLD_")])
	if (length(old_tier_names) > 0) {
		top_tiers <- c(top_tiers, old_tier_names)
	}
	if (length(top_tiers) > 0) {
		sortVector <- c(top_tiers, "\\*")
		destinationTranscript@tiers <- act::helper_tiers_sort_table(
			destinationTranscript@tiers, sortVector, tiersAddMissing = TRUE
		)
	}

	#--- CLI warning for cut annotations
	if (nrow(cut_info) > 0) {
		cut_info <- cut_info[order(cut_info$region, cut_info$time), ]
		for (reg in unique(cut_info$region)) {
			reg_rows <- cut_info[cut_info$region == reg, ]
			region_start <- reg_rows$time[reg_rows$side == "start"]
			region_end   <- reg_rows$time[reg_rows$side == "end"]
			time_label <- paste0(
				if (length(region_start) > 0) paste0(round(region_start[1], 1)) else "?",
				"s-",
				if (length(region_end) > 0) paste0(round(region_end[1], 1)) else "?",
				"s"
			)
			all_cut_tiers <- unique(unlist(strsplit(reg_rows$tiers, ", ")))
			for (tn in all_cut_tiers) {
				sides <- character()
				if (any(reg_rows$side == "start" & grepl(tn, reg_rows$tiers, fixed = TRUE))) sides <- c(sides, "left")
				if (any(reg_rows$side == "end" & grepl(tn, reg_rows$tiers, fixed = TRUE))) sides <- c(sides, "right")
				side_label <- paste(sides, collapse = "&")
				cli::cli_alert_warning("Annotation(s) cut: {tn} ({side_label}) at {time_label}")
			}
		}
	}

	#--- insert CHECK-update tier
	if (insertCheckTier && nrow(cut_info) > 0) {
		check_tier_name <- "CHECK-update"
		cut_info <- cut_info[order(cut_info$time), ]
		check_anns <- data.frame(
			startsec = pmax(0, cut_info$time - 1),
			endsec   = cut_info$time + 1,
			content  = paste0("REVISE cut annotation: ", cut_info$tiers),
			stringsAsFactors = FALSE
		)
		for (ci in seq_len(nrow(check_anns))) {
			if (ci > 1 && check_anns$startsec[ci] < check_anns$endsec[ci - 1]) {
				check_anns$startsec[ci] <- check_anns$endsec[ci - 1]
			}
			if (check_anns$startsec[ci] < check_anns$endsec[ci]) {
				destinationTranscript@annotations <- rbind(destinationTranscript@annotations, data.frame(
					annotationID = as.integer(max(as.integer(destinationTranscript@annotations$annotationID), na.rm = TRUE) + 1L),
					tierName = check_tier_name,
					startsec = check_anns$startsec[ci],
					endsec = check_anns$endsec[ci],
					content = check_anns$content[ci],
					content.norm = "",
					char.orig.bytime.start = NA_integer_, char.orig.bytime.end = NA_integer_,
					char.norm.bytime.start = NA_integer_, char.norm.bytime.end = NA_integer_,
					char.orig.bytier.start = NA_integer_, char.orig.bytier.end = NA_integer_,
					char.norm.bytier.start = NA_integer_, char.norm.bytier.end = NA_integer_,
					stringsAsFactors = FALSE
				))
			}
		}
	}

	#--- store cut_info as attribute for Excel export
	attr(destinationTranscript, "cut_info") <- cut_info

	#change name & path
	destinationTranscript@name <- paste(destinationTranscript@name, "_UPDATED",sep="")
	destinationTranscript@file.path <-""
	
	#HISTORY transcript
	destinationTranscript@modification.systime <- Sys.time()
	destinationTranscript@history[[length(destinationTranscript@history)+1]] <-	list( 
		modification               = "transcripts_merge",
		systime                    = Sys.time(),
		destinationTranscript      = destinationTranscript@name,
		sourceTranscripts          = c(destinationTranscript, updateTranscripts)
	)
	

	return(destinationTranscript)
}



