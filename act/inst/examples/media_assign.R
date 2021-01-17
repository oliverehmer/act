library(act)

# Set the folder(s) where your media files are located in the corpus object
# Please be aware that that the example corpus that comes with the package
# does NOT contain media files. Please download the entire example corpus
# with media files if you want to use this function reasonably.
examplecorpus@paths.media.files <- c("", "")

examplecorpus <- act::media_assign(examplecorpus)

