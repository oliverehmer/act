library(act)

# Search all transcripts that have "ARG" (ignoring case sensitivity) in their name
mymeta <- act::search_meta(x=examplecorpus, filterTranscriptIncludeRegEx="(?i)arg")
mymeta$transcripts.names

# Search all transcripts that don't have "ARG" in their name
mymeta <- act::search_meta(x=examplecorpus, filterTranscriptExcludeRegEx="ARG")
mymeta$transcripts.names

# Search all tiers that have an "A" or an "a" in their name
mymeta <- act::search_meta(x=examplecorpus, filterTierIncludeRegEx="(?i)A")
mymeta$tiers.names

# Search all tiers that have a capital "A" in their name
mymeta <- act::search_meta(x=examplecorpus, filterTierIncludeRegEx="A")
mymeta$tiers.names

# In which transcripts do these tiers occur?
mymeta$transcripts.names

# Let's check the first of the transcripts, if this is really the case...
examplecorpus@transcripts[[mymeta$transcripts.names[1]]]@tiers

