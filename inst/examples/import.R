library(act)

# To import an annotation file of your choice:
\dontrun{
	path <- "PATH_TO_AN_EXISTING_FILE_ON_YOUR_COMPUTER"
}

# Path to a .TextGrid file that you want to read
filePath <- system.file("extdata", "examplecorpus", "GAT", 
						"ARG_I_PAR_Beto.TextGrid", package="act")
t <- act::import(filePath=filePath)
t

# Path to an .eaf file that you want to read
filePath <- system.file("extdata", "examplecorpus", "SYNC", 
						"SYNC_rotar_y_flexionar.eaf", package="act")
t <- act::import(filePath=filePath)
t

# Content of a .TextGrid file, e.g. as stored in \code{@file.content} 
# of a transcript object.
fileContent <- examplecorpus@transcripts[['ARG_I_CHI_Santi']]@file.content
t <- act::import(fileContent=fileContent)
t

# Content of an .eaf file, e.g. as stored in \code{@file.content} 
# of a transcript object.
fileContent <- examplecorpus@transcripts[['SYNC_rotar_y_flexionar']]@file.content
t <- act::import(fileContent=fileContent)
t

