library(act)

# Path to the .TextGrid file that you want to read
path <- system.file("extdata", "examplecorpus", "GAT", 
					"ARG_I_PAR_Beto.TextGrid", package="act")

# To import a .TextGrid file of your choice:
\dontrun{
path <- "PATH_TO_AN_EXISTING_TEXTGRID_ON_YOUR_COMPUTER"
}


t <- act::import_textgrid(filePath=path)
t


# Content of a .TextGrid (already read by \code{readLines}), 
# e.g. from an existing transcript object:
mycontent <-examplecorpus@transcripts[[1]]@file.content
t <- act::import_textgrid(fileContent=mycontent, transcriptName="test")
t

