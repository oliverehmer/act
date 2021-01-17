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
	
	# Create cut lists 
	mysearch <- act::search_cuts_media (x=examplecorpus, s=mysearch)
	
	# Check results for Mac:
	# Get entire cut list for Mac and display on screen, 
	# so you can copy&paste this into the Terminal
	mycutlist <-mysearch@cuts.cutlist.mac 
	cat(stringr::str_c(mycutlist, sep="\n", collapse = "\n"))
	# Cut list for first search result
	mycutlist <- mysearch@results$cuts.cutlist.mac[[1]]
	cat(stringr::str_c(mycutlist, sep="\n", collapse = "\n"))
	
	# Check results for Windows:
	# Get entire cut list for Mac and display on screen, 
	# so you can copy&paste this into the CLI
	mycutlist <-mysearch@cuts.cutlist.win 
	cat(stringr::str_c(mycutlist, sep="\n", collapse = "\n"))
	# Cut list for first search result
	mycutlist <- mysearch@results$cuts.cutlist.win[[1]]
	cat(stringr::str_c(mycutlist, sep="\n", collapse = "\n"))
	
	# It is, however, more convenient to specify the argument 'outputFolder' in order to get
	# the cut list as a (executable) file/batch list.
}