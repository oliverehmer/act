library(act)

# Summary of the data in the corpus
examplecorpus

# Summary of the data in the second transcript in the corpus
examplecorpus@transcripts[[2]]

\dontrun{
# The example corpus with its media files (downloaded once, about 64 MB)
x <- act::examplecorpus_media()
x@transcripts[[1]]@media
}
