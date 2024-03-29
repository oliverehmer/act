% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_openresult_inquicktime.R
\name{search_openresult_inquicktime}
\alias{search_openresult_inquicktime}
\title{Open a search result in 'Quicktime' (and play it)}
\usage{
search_openresult_inquicktime(
  x,
  s,
  resultNr,
  play = TRUE,
  closeAfterPlaying = FALSE,
  bringQuicktimeToFront = TRUE,
  filterFile = c(".*\\\\.(mp4|mov)", ".*\\\\.(aiff|aif|wav)", ".*\\\\.mp3")
)
}
\arguments{
\item{x}{Corpus object.}

\item{s}{Search object.}

\item{resultNr}{Integer; Number of the search result (row in the data frame \code{s@results}) to be played.}

\item{play}{Logical; If \code{TRUE} selection will be played.}

\item{closeAfterPlaying}{Logical; if \code{TRUE} the Quicktime player will be closed after playing the cuts.}

\item{bringQuicktimeToFront}{Logical; if \code{TRUE} the Quicktime player will be activated and placed before all other windows.}

\item{filterFile}{Vector of character strings; Each element of the vector is a regular expression. Expressions will be checked consecutively. The first match with an existing media file will be used for playing. The default checking order is video > uncompressed audio > compressed audio.}
}
\value{
Logical; \code{TRUE} if media file has been played, or \code{FALSE} if not.
}
\description{
The function remote controls 'Quicktime' by using an Apple Script.
It opens a search result in 'Quicktime' and plays it.
}
\details{
Note: You need to be on a Mac to use this function.

\emph{Span} \cr
If you want to extend the cut before or after each search result, you can modify \code{@cuts.span.beforesec} and \code{@cuts.span.aftersec} in your search object.
}
\examples{
library(act)

mysearch <- act::search_new(x=examplecorpus, pattern = "pero")

# You can only use this function if you are on a Mac.
# In addition, you need to have downloaded the example media. 
\dontrun{

# Assign media files
examplecorpus@paths.media.files <- c("FOLDERWHEREMEDIAFILESARELOCATED")
examplecorpus <- act::media_assign(examplecorpus)
	
# Play the media for the first search result
act::search_openresult_inquicktime(x=examplecorpus, 
s=mysearch, 
resultNr = 1,
play=TRUE,
closeAfterPlaying=TRUE)

# Play all search results after one another.
	for (i in 1:nrow(mysearch@results)) {
		print(mysearch@results$content[i])
		act::search_openresult_inquicktime(x=examplecorpus, 
s=mysearch, 
resultNr = i, 
play=TRUE,
closeAfterPlaying=TRUE)
	}
}
}
