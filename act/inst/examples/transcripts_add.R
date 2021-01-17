library(act)

# get one of the already existing transcript in the examplecorpus
newtrans <- examplecorpus@transcripts[[1]]

# add this transcript to the examplecorpus
newcorpus <- act::transcripts_add(examplecorpus, newtrans)

# compare the two corpus objects
length(examplecorpus@transcripts)
length(newcorpus@transcripts)

names(examplecorpus@transcripts)
names(newcorpus@transcripts)
