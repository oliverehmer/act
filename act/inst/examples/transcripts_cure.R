library(act)

# The example corpus does not contain any errors.
# But let's use the function anyway.
x<-act::transcripts_cure(examplecorpus)
x@history[[length(x@history)]]

# See \code{act::cure_transcript} for actual examples.

