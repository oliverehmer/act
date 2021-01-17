library(act)

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")
nrow(mysearch@results)

# Create temporary file path
path <- tempfile(pattern = "searchresults", tmpdir = tempdir(),
    			 fileext = ".xlsx")

# It makes more sense, however, to you define a destination folder
# that is easier to access on your computer:
\dontrun{
	path <- tempfile(pattern = "searchresults",
 					 tmpdir = "PATH_TO_AN_EXISTING_FOLDER_ON_YOUR_COMPUTER",
 					 fileext = ".xlsx")
}

# Save search results
act::search_results_export(s=mysearch, path=path)

# Do your coding of the search results somewhere outside of act
# ...

# Load search results
mysearch.import <- act::search_results_import(path=path)
nrow(mysearch.import@results)
