library(act)

# The example files that come with the act library are located here:
path <- system.file("extdata", "examplecorpus", package="act")

# The example corpus comes without media files.
# It is recommended to download a full example corpus also including the media files.
# You can use the following commands.
\dontrun{
   path <- "EXISTING_FOLDER_ON_YOUR_COMPUTER/examplecorpus"
   temp <- tempfile()
   download.file(options()$act.examplecorpusURL, temp)
   unzip(zipfile=temp, exdir=path)
}

# The following command creates a new corpus object
mycorpus <- act::corpus_new(name = "mycorpus",
	pathsAnnotationFiles = path,
	pathsMediaFiles = path)

# Get a summary
mycorpus

