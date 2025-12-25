library(act)

# Search all transcripts that have "ARG" (ignoring case sensitivity) in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTranscriptIncludeRegex="(?i)arg")
myfilter$transcriptNames

# Search all transcripts that don't have "ARG" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTranscriptExcludeRegex="ARG")
myfilter$transcriptNames

# Search all tiers that have an "A" or an "a" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTierIncludeRegex="(?i)A")
myfilter$tierNames

# Search all tiers that have a capital "A" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTierIncludeRegex="A")
myfilter$tierNames

# In which transcripts do these tiers occur?
myfilter$transcriptNames

# Let's check the first of the transcripts, if this is really the case...
examplecorpus@transcripts[[myfilter$transcriptNames[1]]]@tiers

