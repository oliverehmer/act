% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiers_add.R
\name{tiers_add}
\alias{tiers_add}
\title{Add tiers}
\usage{
tiers_add(
  x,
  tierName,
  tierType = c("IntervalTier", "TextTier"),
  absolutePosition = NULL,
  destinationTier = NULL,
  relativePositionToDestinationTier = 0,
  insertOnlyIfDestinationExists = FALSE,
  filterTranscriptNames = NULL,
  skipIfTierAlreadyExists = TRUE
)
}
\arguments{
\item{x}{Corpus object.}

\item{tierName}{Character string; names of the tier to be added.}

\item{tierType}{Character string; type of the tier to be added.}

\item{absolutePosition}{Integer; Absolute position where the tier will be inserted. Value 1 and values beloe 1 will insert the tier in the first position; To insert the tier at the end, leave 'absolutePosition' and 'destinationTier' open.}

\item{destinationTier}{Character string; insert the tier relative to this tier.}

\item{relativePositionToDestinationTier}{Integer; position relative to the destination tier; 1=immediately after; 0 and -1=immediately before;  bigger numbers are also allowed.}

\item{insertOnlyIfDestinationExists}{Logical; if \code{TRUE} the new tier will only be added if the destination tier 'destinationTier' exists in the transcript object. If \code{FALSE} the new tier will only be added in any case. If the destination tier 'destinationTier' does not exist in the transcript object, the tier will be inserted at the end.}

\item{filterTranscriptNames}{Vector of character strings; names of the transcripts to be modified. If left open, the tier will be added to all transcripts in the corpus.}

\item{skipIfTierAlreadyExists}{Logical; if \code{TRUE} the new tier will be skipped if a tier with this name already exists in the transcript; if \code{FALSE} an error will be raised.}
}
\value{
Corpus object.
}
\description{
Adds a tiers in all transcript objects of a corpus.
If tiers should be added only in certain transcripts, set the parameter \code{filterTranscriptNames}.
In case that you want to select transcripts by using regular expressions use the function \code{act::search_makefilter} first.
}
\details{
You can either insert the new tier at a specific position (e.g. 'absolutePosition=1') or in relation to a existing tier (e.g. destinationTier='speaker1').
To insert a tier at the end, leave 'absolutePosition' and 'destinationTier' open.

Results will be reported in \code{@history} of the transcript objects.
}
\examples{
library(act)

# --- Add new interval tier.
# Since not position is set it will be inserted in the end, by default.
x <- act::tiers_add(examplecorpus,
					tierName="TEST")
#check results
x@history[length(x@history)]
#have a look at the first transcript
x@transcripts[[1]]@tiers
#--> New tier is inserted in the end.

# --- Add new interval tier in position 2
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					absolutePosition=2)
#check results
x@history[length(x@history)]
#have a look at the first transcript
x@transcripts[[1]]@tiers
#--> New tier is inserted as second tier.


# --- Add new interval tier at the position of "Entrevistador", only if this tier exists,
# If the destination tier does not exist, the new tier will NOT be inserted.

#Have a look at the first and the second transcript. 
examplecorpus@transcripts[[1]]@tiers
#Transcript 1 does contain a tier "Entrevistador" in the first position.
examplecorpus@transcripts[[2]]@tiers
#Transcript 2 does contain a tier "Entrevistador" in the first position.

#Insert new tier
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					destinationTier="Entrevistador",
					relativePositionToDestinationTier=0,
					insertOnlyIfDestinationExists=TRUE)

#Check results
x@history[length(x@history)]
#Have a look at the transcript 1:
# Tier 'TEST' was in first position (e.g. where 'Entrevistador' was before).
x@transcripts[[1]]@tiers
#Have a look at the transcript 2:
#Tier 'TEST' was not inserted, since there was no destination tier 'Entrevistador'.
x@transcripts[[2]]@tiers

# --- Add new interval tier AFTER tier="Entrevistador"
# If the destination tier does not exist, the new tier will be inserted at the end in any case.
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					destinationTier="Entrevistador",
					relativePositionToDestinationTier=1,
					insertOnlyIfDestinationExists=FALSE)
#check results
x@history[length(x@history)]
#Have a look at the transcript 1:
# Tier 'TEST' was inserted after the tier 'Entrevistador'.
x@transcripts[[1]]@tiers
#Have a look at the transcript 2:
#Tier 'TEST' was insertedat the end.
x@transcripts[[2]]@tiers

}
\seealso{
\link{tiers_delete}, \link{tiers_rename}, \link{tiers_convert}, \link{tiers_sort}
}
