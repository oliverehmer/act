library(act)

# ========== Example data 
# The act package comes with some example data. 
# The data is stored at the following location:
path <- system.file("extdata", "examplecorpus", package="act")

# Since this folder is quite difficult to access, you might consider copying the 
# contents of this folder to a more convenient location.
# The following commands will create a new folder called 'examplecorpus' in the
# folder 'path'.
# You will find the data there.
\dontrun{
path <- "EXISTING_FOLDER_ON_YOUR_COMPUTER"
sourcepath <- system.file("extdata", "examplecorpus", package="act")
if (!dir.exists(path)) {dir.create(path)}
file.copy(sourcepath, dirname(path), recursive=TRUE)
}

# The example files that come with the package do only contain annotation files.
# Media files are not included.
# The following lines will download the data and create a new folder called 
# 'examplecorpus' in the folder 'path'.  
# You will find the data there.
\dontrun{
path <- "EXISTING_FOLDER_ON_YOUR_COMPUTER"
sourceurl <- 
"http://www.romanistik.uni-freiburg.de/ehmer/files/digitalhumanities/act_examplecorpus.zip"
temp <- tempfile()
download.file(sourceurl, temp)
unzip(zipfile=temp, exdir=path)
}

# ========== Create a corpus object and load data
# Now that we have the example data accessible, we can create a corpus object.
# The corpus object is a structured collection of all the information that you can 
# work with using act.
# It will contain the information of each transcript, links to media files and further 
# meta data.

# --- Locate folder with annotation files
# When creating a corpus object you will need to specify where your annotation 
# files ('Praat' '.TextGrids' or 'ELAN' .eaf) are located.
# We will use the example data, that we have just located in 'path'.
path

# In case that you want to use your own data, you can set the path here:
\dontrun{
	path <- "EXISTING_FOLDER_ON_YOUR_COMPUTER"
}

# --- Create corpus object and load annotation files
# The following command will create a corpus object, with the name 'examplecorpus'.
examplecorpus <- act::corpus_new(
	pathsAnnotationFiles = path,
	pathsMediaFiles = path,
	name = "examplecorpus"
)

# The act package assumes, that annotation files and media files have the same base  
# name and differ only in the suffix (e.g. 'filename.TextGrid' and 'filename.wav'/
# 'filename.mp4').
# This allows act to automatically link media files to the transcripts.

# --- Information about your corpus
# The following command will give you a summary of the data contained in your corpus object.
examplecorpus
# More detailed information about the transcripts in your corpus object is available by 
# calling the function act::info()
act::info(examplecorpus)
# If you are working in R studio, a nice way of inspecting this information is the following:
\dontrun{
	View(act::info(examplecorpus)$transcripts)
	View(act::info(examplecorpus)$tiers)
}

# ========== all data
# You can also get all data that is in the loaded annotation files in a data frame:
all_annotations <- act::annotations_all(examplecorpus)
\dontrun{
	View(all_annotations)
}

# ========== Search
# Let's do some searches in the data.
# Search for the 1. Person Singular Pronoun in Spanish 'yo' in the examplecorpus
mysearch <- act::search_new(x=examplecorpus, 
							pattern= "yo")
# Have a look at the result:
mysearch

# Directly view all search results in the viewer
\dontrun{
	View(mysearch@results)
}

# --- Search original vs. normalized content
# You can either search in the original 'content' of the annotations,
# or you can search in a 'normalized' version of the annotations.
# Let's compare the two modes.
mysearch.norm  <- act::search_new(examplecorpus, pattern="yo", searchNormalized=TRUE)
mysearch.org   <- act::search_new(examplecorpus, pattern="yo", searchNormalized=FALSE)
# There is a difference in the number of results.
mysearch.norm@results.nr
mysearch.org@results.nr

# The difference is because during in the normalized version, for instance, capital letters 
# will be converted to small letters. 
# In our case, one annotation in the example corpus contains a "yO" with a
# capital letter:
mysearch <- act::search_new(examplecorpus, pattern="yO", searchNormalized=FALSE)
mysearch@results$hit

# During normalization a range of normalization procedures will be applied, using a 
# replacement matrix. This matrix searches and replaces certain patterns, that you want to 
# exclude from the normalized content.
# By default, normalization gets rid of all transcription conventions of GAT. 
# You may, in addition, also customize the replacement matrix to your own needs/transcription
# conventions.

# --- Search original content vs. full text
# There are two search modes.
# The 'fulltext' mode will will find matches across annotations.
# The 'content' mode will will respect the temporal boundaries of the original annotations.

# Let's define a search pattern with a certain span.
myRegEx <- "\\bno\\b.{1,20}pero"
# This regular expression matches the Spanish word "no" 'no' followed by a "pero" 'but'
# in a distance ranging from 1 to 20 characters.

# The 'content' search mode will not find any hit.
mysearch <- act::search_new(examplecorpus, pattern=myRegEx, searchMode="content")
mysearch@results.nr

# The 'fulltext' search mode will not find two hits that extend over several annotations.
mysearch <- act::search_new(examplecorpus, pattern=myRegEx, searchMode="fulltext")
mysearch@results.nr
cat(mysearch@results$hit[1])
cat(mysearch@results$hit[2])

