library(act)

# get current names of the transcripts
names.old <- act::helper_transcriptNames_get(examplecorpus)

# rename giving numbers as names
names.test <- as.character(seq(1:length(names.old)))
test <-  act::helper_transcriptNames_set(examplecorpus, names.test)
names(test@transcripts)

# create an error: empty name
\dontrun{
names.test <- names.old
names.test[2] <- " "
test <-  act::helper_transcriptNames_set(examplecorpus, names.test)
}

# create an error: double names
\dontrun{
names.test <- names.old
names.test[2] <- names.test[1]
test <-  act::helper_transcriptNames_set(examplecorpus, names.test)
}