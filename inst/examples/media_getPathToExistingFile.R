library(act)

# Please be aware that that the example corpus that comes with the package
# does NOT contain media files. Please download the entire example corpus
# with media files if you want to use this function reasonably.

# You can access the media files linked to a transcript directly using
# the object properties.
examplecorpus@transcripts[["SYNC_rotar_y_flexionar"]]@media.path

# Get only media files of a certain type, e.g. a wav file, and return only the first match:
act::media_getPathToExistingFile(examplecorpus@transcripts[["SYNC_rotar_y_flexionar"]],
 filterMediaFile=".*\\.wav")
