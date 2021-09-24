library(act)

# Summary of the data in the corpus
examplecorpus

# Summary of the data in th second transcripts in the corpus
examplecorpus@transcripts[[2]]

\dontrun{
# Download example corpus with media files
destinationpath <- "/EXISTING_FOLDERON_YOUR_COMPUTER/examplecorpus"
temp <- tempfile()
download.file(options()$act.examplecorpusURL, temp)
unzip(zipfile=temp, exdir=destinationpath)
}
