library(act)

# Search for the 1. Person Singular Pronoun in Spanish.
# Only create the search object without running the search.
mysearch <- act::search_new(x=examplecorpus, pattern= "yo", runSearch=FALSE)

# Run the search

df <- act::search_transcript_content(t=examplecorpus@transcripts[[3]],
s=mysearch)
nrow(df)
