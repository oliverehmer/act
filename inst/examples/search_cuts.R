library(act)

# IMPORTANT: In the example corpus all transcripts are assigned media links.
# The actual media files are, however, not included in when installing the package 
# due to size limitations of CRAN.
# But you may download the media files separately.
# Please see the section 'examplecorpus' for instructions. 
# --> You will need the media files to execute the following example code.

\dontrun{
	# Search
	mysearch <- act::search_new(examplecorpus, pattern="yo")
	
	# Create print transcripts, media cutlists and .srt subtitles 
	# for all search results
	test <- act::search_cuts(x=examplecorpus, s=mysearch)
	
	# Display all print transcripts on screen from @cuts.printtranscripts
	cat(test@cuts.printtranscripts)
	
	# Display cutlist on screen from @cuts.cutlist.mac
	cat(test@cuts.cutlist.mac)
	
	# Display .srt subtitles
	cat(test@results[, mysearch@cuts.column.srt])
}

