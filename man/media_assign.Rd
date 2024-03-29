% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/media_assign.R
\name{media_assign}
\alias{media_assign}
\title{Assign media file links to transcript objects}
\usage{
media_assign(
  x,
  searchPaths = NULL,
  searchInSubfolders = TRUE,
  filterFile = "",
  transcriptNames = NULL,
  deleteExistingMedia = TRUE,
  onlyUniqueFiles = TRUE
)
}
\arguments{
\item{x}{Corpus object.}

\item{searchPaths}{Vector of character strings; paths where media files should be searched; if path is not defined, the paths given in \code{x@paths.media.files} will be used).}

\item{searchInSubfolders}{Logical; if \code{FALSE} only the main level of the directory will be scanned for media, if \code{TRUE} sub folders will be scanned for media, too.}

\item{filterFile}{Character string; Regular expression of files to look for.}

\item{transcriptNames}{Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.}

\item{deleteExistingMedia}{Logical; if \code{TRUE} existing media links will be deleted, if \code{FALSE} existing media links will be preserved and new links will be added.}

\item{onlyUniqueFiles}{Logical; if \code{TRUE} media files with the same name (in different locations) will only be added once; if \code{FALSE} all media files found will be added, irrespective of possible doublets.}
}
\value{
Corpus object.
}
\description{
Searches for media files in folders and assigns the links to transcript objects in a corpus.
The function uses the name of the transcript to find the media files,
e.g. the function assumes that the annotation files have the same name as the media files, except from the suffix/the file type.
}
\details{
Only the the file types set in \code{options()$act.fileformats.audio} and \code{options()$act.fileformats.video} will be recognized.
You can modify these options to recognize other media types.
}
\examples{
library(act)

# Set the folder(s) where your media files are located in the corpus object
# Please be aware that that the example corpus that comes with the package
# does NOT contain media files. Please download the entire example corpus
# with media files if you want to use this function reasonably.
examplecorpus@paths.media.files <- c("", "")

examplecorpus <- act::media_assign(examplecorpus)

}
\seealso{
\link{media_delete}, \link{media_getPathToExistingFile}
}
