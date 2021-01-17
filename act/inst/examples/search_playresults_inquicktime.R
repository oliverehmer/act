library(act)

mysearch <- act::search_new(x=examplecorpus, pattern = "pero")

# You can only use this function if you are on a Mac.
# In addition, you need to have downloaded the example media files. 
\dontrun{
	# Assign media files
	examplecorpus@paths.media.files <- c("FOLDERWHEREMEDIAFILESARELOCATED")
	examplecorpus <- act::media_assign(examplecorpus)
	
	# Create print transcripts. This is not necessary.
	# But its nice to see them when playing all results.
	mysearch <- act::search_cuts_printtranscript (x=examplecorpus, s=mysearch)
	
	# Play all search results
	act::search_playresults_inquicktime(x=examplecorpus, s=mysearch)
}
