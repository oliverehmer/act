library(act)

# Set destination folder
outputFolder <- tempdir()

# It makes more sense, however, to you define a folder
# that is easier to access on your computer
\dontrun{
outputFolder <- "PATH_TO_AN_EMPTY_FOLDER_ON_YOUR_COMPUTER"
}

# Exports all transcript objects in all supported formats
act::corpus_export(x=examplecorpus,
							   outputFolder=outputFolder)

# Exports all transcript objects in 'Praat' .TextGrid format
act::corpus_export(x=examplecorpus,
                              outputFolder=outputFolder,
                              formats="textgrid")

# Exports all transcript objects in 'ELAN' .eaf format.
# By default WITH media links
act::corpus_export(x=examplecorpus,
						outputFolder=outputFolder,
						formats="eaf")


# Same same, but now WITHOUT media links.
# Only Media links are only exported that are in
# the '@media.path' attribute in the transcript object(s))
act::corpus_export(x=examplecorpus,
						outputFolder=outputFolder,
						formats="eaf",
						createMediaLinks=FALSE)

# Exports in 'ELAN' .eaf and Praat' .TextGrid format
act::corpus_export(x=examplecorpus,
                                 outputFolder=outputFolder,
                                 formats=c("eaf", "textgrid"))

