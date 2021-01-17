library(act)

# Search for the 1. Person Singular Pronoun in Spanish
# Search without creating the concordance immediately.
# This is for example useful if you are working with a large corpus, since
# making the concordance may take a while.
mysearch <- act::search_new(examplecorpus, pattern="yo", concordanceMake=FALSE)
mysearch@results[1,]


# The results do not contain the concordance, it is only 15 columns
ncol(mysearch@results)

# Make the concordance
mysearch.new <- act::search_concordance(x=examplecorpus, s=mysearch)
ncol(mysearch.new@results)
