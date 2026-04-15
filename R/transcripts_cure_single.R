#' Cure a single transcript
#'
#' Transcript object may contain errors, e.g. because of defect annotation input files or user modifications.
#' This function may cure some of these errors.
#' - Annotations with reversed times: annotations with \code{endsec} lower than \code{startsec} will be deleted.
#' - Overlapping annotations: earlier annotations will end where the next annotation starts.
#' - Annotations below 0 sec: Annotations that are starting and ending before 0 sec will be deleted; Annotations starting before but ending after 0 sec will be truncated.
#' - Transcript length zero: If \code{@length.sec} is missing or \code{<= 0} it will be set to the largest \code{endsec} of any annotation (or a small default).
#' - Zero-duration annotations: In IntervalTiers, annotations with \code{startsec == endsec} will be cured. Multiple zero-duration annotations at the same time point are merged by concatenating their content with a single space. The resulting annotation is then extended to the start of the next annotation (or to \code{@length.sec} if it is the last one). If no room is available (next annotation starts at the same time), the annotation is deleted.
#' - Missing tiers: Tiers that are present in the annotations but missing in the list of tiers in \code{@tiers} of the transcript object will be added.
#'
#' @param t Transcript object.
#' @param annotationsTimesReversed Logical; If \code{TRUE} annotations with reversed times will be deleted
#' @param annotationsTimesBelowZero Logical; If \code{TRUE} annotations before 0 sec will be corrected.
#' @param annotationsOverlap Logical; If \code{TRUE} overlapping annotations will be corrected.
#' @param transcriptLengthZero Logical; If \code{TRUE} a transcript with \code{@length.sec <= 0} will be corrected to the largest annotation endsec.
#' @param annotationsZeroDuration Logical; If \code{TRUE} zero-duration annotations in IntervalTiers will be merged/extended/deleted.
#' @param tiersMissing Logical; If \code{TRUE} tiers missing in \code{@tiers} slot of the transcript object will be added.
#' @param warning Logical; If \code{TRUE} a warning notice will be shown upon correction.
#'
#' @return Transcript object;
#'
#' @seealso \link{transcripts_cure}
#'
#' @export
#'
#' @example inst/examples/transcripts_cure_single.R
#'
transcripts_cure_single <- function (t,
									 annotationsTimesReversed  = TRUE,
									 annotationsOverlap        = TRUE,
									 annotationsTimesBelowZero = TRUE,
									 transcriptLengthZero      = TRUE,
									 annotationsZeroDuration   = TRUE,
									 tiersMissing              = TRUE,
									 warning                   = FALSE) {

	if (missing(t)) 	{cli::cli_abort("Transcript object in parameter {.arg t} is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{cli::cli_abort("Parameter {.arg t} needs to be a {.cls transcript} object.") 	} }

	#--- annotationsTimesReversed
	annotationsTimesReversed.deleted.count <- 0
	if (annotationsTimesReversed) {
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$endsec<t@annotations$startsec )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsTimesReversed.deleted.count <- length(ids)
			}
		}
	}

	#--- annotationsOverlap
	annotationsOverlap.corrected.count <- 0
	if (annotationsOverlap) {
		if (nrow( t@annotations)>1) {
			t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]
			tiers <- unique(t@annotations$tierName)
			for (tier in tiers) {
				ids <- which(t@annotations$tierName==tier)
				if (length(ids)>1) {
					for (i in 1:(length(ids)-1)) {
						if 	(t@annotations$endsec[ids[i]]>t@annotations$startsec[ids[i]+1]) {
							t@annotations$endsec[ids[i]] <- t@annotations$startsec[ids[i]+1]
							annotationsOverlap.corrected.count <- annotationsOverlap.corrected.count +1
						}
					}
				}
			}
		}
	}

	#--- below 0
	annotationsTimesBelowZero.deleted.count <- 0
	annotationsTimesBelowZero.corrected.count <- 0
	if (annotationsTimesBelowZero) {
		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$endsec<0 & t@annotations$startsec<0 )
			if (length(ids)>0) {
				t@annotations <- t@annotations[-ids,]
				annotationsTimesBelowZero.deleted.count <- length(ids)
			}
		}

		if (nrow( t@annotations)>0) {
			ids <- which ( t@annotations$startsec<0 )
			if (length(ids)>0) {
				t@annotations$startsec[ids] <- 0
				annotationsTimesBelowZero.corrected.count <- length(ids)
			}
		}
	}

	#--- transcriptLengthZero
	transcriptLengthZero.corrected <- FALSE
	transcriptLengthZero.new.value <- NA_real_
	if (transcriptLengthZero) {
		current.length <- t@length.sec
		if (is.null(current.length) || length(current.length) == 0L || !is.finite(current.length) || current.length <= 0) {
			new.length <- 0
			if (nrow(t@annotations) > 0) {
				max.endsec <- suppressWarnings(max(t@annotations$endsec, na.rm = TRUE))
				if (is.finite(max.endsec) && max.endsec > 0) {
					new.length <- max.endsec
				}
			}
			if (new.length <= 0) new.length <- 0.001
			t@length.sec <- new.length
			transcriptLengthZero.corrected <- TRUE
			transcriptLengthZero.new.value <- new.length
		}
	}

	#--- annotationsZeroDuration (IntervalTiers only)
	annotationsZeroDuration.merged.count   <- 0
	annotationsZeroDuration.extended.count <- 0
	annotationsZeroDuration.deleted.count  <- 0
	if (annotationsZeroDuration) {
		if (nrow(t@annotations) > 0 && nrow(t@tiers) > 0) {
			interval.tier.names <- t@tiers$name[t@tiers$type == "IntervalTier"]

			if (length(interval.tier.names) > 0) {
				t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]

				keep <- rep(TRUE, nrow(t@annotations))

				for (tier.name in interval.tier.names) {
					tier.row.ids <- which(t@annotations$tierName == tier.name)
					if (length(tier.row.ids) == 0L) next

					# Step 1: merge zero-duration annotations at the same time point
					zero.ids.in.tier <- tier.row.ids[
						t@annotations$startsec[tier.row.ids] == t@annotations$endsec[tier.row.ids]
					]

					if (length(zero.ids.in.tier) > 1L) {
						# group by startsec
						zero.times <- t@annotations$startsec[zero.ids.in.tier]
						unique.times <- unique(zero.times)
						for (u.time in unique.times) {
							group.ids <- zero.ids.in.tier[zero.times == u.time]
							if (length(group.ids) > 1L) {
								# merge content into first, mark rest for deletion
								merged.content <- paste(
									t@annotations$content[group.ids],
									collapse = " "
								)
								t@annotations$content[group.ids[1]] <- merged.content
								keep[group.ids[-1]] <- FALSE
								annotationsZeroDuration.merged.count <-
									annotationsZeroDuration.merged.count + (length(group.ids) - 1L)
							}
						}
					}
				}

				# apply deletions from merge step before extend/delete step
				if (any(!keep)) {
					t@annotations <- t@annotations[keep, ]
				}

				# Step 2: extend or delete the remaining zero-duration annotations
				keep2 <- rep(TRUE, nrow(t@annotations))

				for (tier.name in interval.tier.names) {
					tier.row.ids <- which(t@annotations$tierName == tier.name)
					if (length(tier.row.ids) == 0L) next

					# sort by startsec within tier
					tier.row.ids <- tier.row.ids[order(t@annotations$startsec[tier.row.ids])]

					for (k in seq_along(tier.row.ids)) {
						row.id <- tier.row.ids[k]
						if (t@annotations$startsec[row.id] != t@annotations$endsec[row.id]) next

						# determine target endsec
						target <- if (k < length(tier.row.ids)) {
							t@annotations$startsec[tier.row.ids[k + 1L]]
						} else {
							t@length.sec
						}

						if (is.finite(target) && target > t@annotations$startsec[row.id]) {
							t@annotations$endsec[row.id] <- target
							annotationsZeroDuration.extended.count <-
								annotationsZeroDuration.extended.count + 1L
						} else {
							keep2[row.id] <- FALSE
							annotationsZeroDuration.deleted.count <-
								annotationsZeroDuration.deleted.count + 1L
						}
					}
				}

				if (any(!keep2)) {
					t@annotations <- t@annotations[keep2, ]
				}
			}
		}
	}

	#--- @tiers in transcript object
	tiersMissing.added.count <- 0
	if (tiersMissing) {
		if (nrow( t@annotations)>0) {
			tierNames <- unique(t@annotations$tierName)

			if (is.null(t@tiers)) {
				t@tiers <- .emptyTiers
			}
			if (length(t@tiers)==0) {
				t@tiers <- .emptyTiers
			}
			tierNamesInList <- 	t@tiers$name

			if (length(setdiff(tierNames, tierNamesInList))>0) {
				tiersMissing.added.count <- length(setdiff(tierNames, tierNamesInList))
				if (!is.null(tierNamesInList)) {
					tierNames 		<- union(tierNamesInList, tierNames)
				}

				tierTypesInList <- t@tiers$type
				names(tierTypesInList) <- NULL
				if (is.null(tierTypesInList))	{
					tierTypes <- rep("IntervalTier", length(tierNames))
				} else	{
					missing <- length(tierNames)-length(tierTypesInList)
					if (missing>0) {
						tierTypesInList <- c(tierTypesInList,rep("IntervalTier", missing))
					}
					tierTypes <- tierTypesInList
				}

				t@tiers <- act::helper_tiers_new_table(tierNames=tierNames, tierTypes=tierTypes)
			}
		}
	}

	#HISTORY transcript
	t@modification.systime <- Sys.time()
	t@history[[length(t@history)+1]] <-	list(
		modification                                  = "transcripts_cure_single",
		systime                                       = Sys.time(),
		annotationsTimesReversed.deleted.count        = annotationsTimesReversed.deleted.count,
		annotationsTimesBelowZero.deleted.count       = annotationsTimesBelowZero.deleted.count,
		annotationsTimesBelowZero.corrected.count     = annotationsTimesBelowZero.corrected.count,
		annotationsOverlap.corrected.count		      = annotationsOverlap.corrected.count,
		transcriptLengthZero.corrected                = transcriptLengthZero.corrected,
		transcriptLengthZero.new.value                = transcriptLengthZero.new.value,
		annotationsZeroDuration.merged.count          = annotationsZeroDuration.merged.count,
		annotationsZeroDuration.extended.count        = annotationsZeroDuration.extended.count,
		annotationsZeroDuration.deleted.count         = annotationsZeroDuration.deleted.count,
		tiersMissing.added.count				      = tiersMissing.added.count
	)


	if (warning) {
		anyChange <- (annotationsTimesReversed.deleted.count > 0
			| annotationsTimesBelowZero.deleted.count > 0
			| annotationsTimesBelowZero.corrected.count > 0
			| annotationsOverlap.corrected.count > 0
			| transcriptLengthZero.corrected
			| annotationsZeroDuration.merged.count > 0
			| annotationsZeroDuration.extended.count > 0
			| annotationsZeroDuration.deleted.count > 0
			| tiersMissing.added.count > 0)
		if (anyChange) {
			bullets <- character()
			if (annotationsTimesReversed.deleted.count > 0)
				bullets <- c(bullets, "*" = "{annotationsTimesReversed.deleted.count} annotation(s) with reversed times deleted")
			if (annotationsTimesBelowZero.deleted.count > 0)
				bullets <- c(bullets, "*" = "{annotationsTimesBelowZero.deleted.count} annotation(s) below 0 sec deleted")
			if (annotationsTimesBelowZero.corrected.count > 0)
				bullets <- c(bullets, "*" = "{annotationsTimesBelowZero.corrected.count} annotation(s) starting before but ending after 0 sec truncated")
			if (annotationsOverlap.corrected.count > 0)
				bullets <- c(bullets, "*" = "{annotationsOverlap.corrected.count} overlapping annotation(s) corrected")
			if (transcriptLengthZero.corrected)
				bullets <- c(bullets, "*" = "length.sec was <= 0, set to {transcriptLengthZero.new.value}")
			if (annotationsZeroDuration.merged.count > 0)
				bullets <- c(bullets, "*" = "{annotationsZeroDuration.merged.count} zero-duration annotation(s) at same time merged")
			if (annotationsZeroDuration.extended.count > 0)
				bullets <- c(bullets, "*" = "{annotationsZeroDuration.extended.count} zero-duration annotation(s) extended")
			if (annotationsZeroDuration.deleted.count > 0)
				bullets <- c(bullets, "*" = "{annotationsZeroDuration.deleted.count} zero-duration annotation(s) deleted (no room to extend)")
			if (tiersMissing.added.count > 0)
				bullets <- c(bullets, "*" = "{tiersMissing.added.count} missing tier(s) added")
			cli::cli_warn(c("Transcript {.val {t@name}} cured:", bullets))
		}
	}
	return(t)
}
