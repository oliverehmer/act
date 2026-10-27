library(act)

# The example corpus does not contain any errors.
# But let's use the function anyway.
x<- act::transcripts_cure(examplecorpus)
tail(x@transcripts[[1]]@history, 1)

# See \code{act::cure_transcript} for actual examples.

