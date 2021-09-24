library(act)

# Search for the 1. Person Singular Pronoun in Spanish.
# Only create the search object without running the search.
mysearch <- act::search_new(x=examplecorpus, pattern= "yo", runSearch=FALSE)

# Run the search
mysearch <- act::search_run(x=examplecorpus, s=mysearch)
mysearch
mysearch@results$hit

# Search Only in tiers called "A", in any transcript
mysearch@filter.tier.names <-"A"
mysearch@filter.transcript.names <-""
mysearch <- act::search_run(x=examplecorpus, s=mysearch)
cbind(mysearch@results$transcript.name, mysearch@results$tier.name, mysearch@results$hit)

# Search Only in tiers called "A", only in transcript "ARG_I_PER_Alejo"
mysearch@filter.tier.names <-"A"
mysearch@filter.transcript.names <-"ARG_I_PER_Alejo"
mysearch <- act::search_run(x=examplecorpus, s=mysearch)
cbind(mysearch@results$transcript.name, mysearch@results$tier.name, mysearch@results$hit)


