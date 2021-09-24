library(act)

# delete two transcripts by their name
test <- act::transcripts_delete(examplecorpus, 
c("BOL_CCBA_SP_MeryGaby1", 
"BOL_CCBA_SP_MeryGaby2"))

# compare the the original and modified corpus object
length(examplecorpus@transcripts)
length(test@transcripts)
setdiff(names(examplecorpus@transcripts), names(test@transcripts))
test@history[length(test@history)]

# delete transcripts that match a filter, e.g. all transcripts from Bolivia "BOL_"
myfilter <- act::search_makefilter(examplecorpus, filterTranscriptIncludeRegEx = "BOL_")
test <- act::transcripts_delete(examplecorpus, 
myfilter$transcript.names)

# compare the the original and modified corpus object
length(examplecorpus@transcripts)
length(test@transcripts)
setdiff(names(examplecorpus@transcripts), names(test@transcripts))
