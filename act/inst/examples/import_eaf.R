library(act)

# Path to an .eaf file that you want to read
path <- system.file("extdata", "examplecorpus", "SYNC", 
					"SYNC_rotar_y_flexionar.eaf", package="act")


# To import a .eaf file of your choice:
\dontrun{
path <- "PATH_TO_AN_EXISTING_EAF_ON_YOUR_COMPUTER"
}


t <- act::import_eaf(filePath=path)
t


# Content of an .eaf file (already read by \code{readLines}), 
# e.g. from an existing transcript object:
mycontent <-examplecorpus@transcripts[['SYNC_rotar_y_flexionar']]@file.content
t <- act::import_eaf(fileContent=mycontent, transcriptName="test")
t

