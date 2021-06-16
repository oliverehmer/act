library(act)

# Search all transcripts that have "ARG" (ignoring case sensitivity) in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTranscriptIncludeRegEx="(?i)arg")
myfilter$transcript.names

# Search all transcripts that don't have "ARG" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTranscriptExcludeRegEx="ARG")
myfilter$transcript.names

# Search all tiers that have an "A" or an "a" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTierIncludeRegEx="(?i)A")
myfilter$tier.names

# Search all tiers that have a capital "A" in their name
myfilter <- act::search_makefilter(x=examplecorpus, filterTierIncludeRegEx="A")
myfilter$tier.names

# In which transcripts do these tiers occur?
myfilter$transcript.names

# Let's check the first of the transcripts, if this is really the case...
examplecorpus@transcripts[[myfilter$transcript.names[1]]]@tiers

