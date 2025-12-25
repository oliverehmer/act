library(act)

# Set destination folder
folderOutput <- tempdir()

# It makes more sense, however, to you define a folder
# that is easier to access on your computer
\dontrun{
folderOutput <- "PATH_TO_AN_EMPTY_FOLDER_ON_YOUR_COMPUTER"
}

# Exports all transcript objects in all supported formats
act::corpus_export(x=examplecorpus,
							   folderOutput=folderOutput)

# Exports all transcript objects in 'Praat' .TextGrid format
act::corpus_export(x=examplecorpus,
                              folderOutput=folderOutput,
                              formats="textgrid")

# Exports all transcript objects in 'ELAN' .eaf format.
# By default WITH media links
act::corpus_export(x=examplecorpus,
						folderOutput=folderOutput,
						formats="eaf")


# Same same, but now WITHOUT media links.
# Only Media links are only exported that are in
# the '@media.path' attribute in the transcript object(s))
act::corpus_export(x=examplecorpus,
						folderOutput=folderOutput,
						formats="eaf",
						createMediaLinks=FALSE)

# Exports in 'ELAN' .eaf and Praat' .TextGrid format
act::corpus_export(x=examplecorpus,
                                 folderOutput=folderOutput,
                                 formats=c("eaf", "textgrid"))

