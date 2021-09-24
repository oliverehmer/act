library(act)

mysearch <- act::search_new(x=examplecorpus, pattern = "pero")

# You can only use this function if you are on a Mac.
# In addition, you need to have downloaded the example media. 
\dontrun{

# Assign media files
examplecorpus@paths.media.files <- c("FOLDERWHEREMEDIAFILESARELOCATED")
examplecorpus <- act::media_assign(examplecorpus)
	
# Play the media for the first search result
act::search_openresult_inquicktime(x=examplecorpus, 
s=mysearch, 
resultNr = 1,
play=TRUE,
closeAfterPlaying=TRUE)

# Play all search results after one another.
	for (i in 1:nrow(mysearch@results)) {
		print(mysearch@results$content[i])
		act::search_openresult_inquicktime(x=examplecorpus, 
s=mysearch, 
resultNr = i, 
play=TRUE,
closeAfterPlaying=TRUE)
	}
}
