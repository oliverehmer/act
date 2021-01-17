library(act)

# Search for the 1. Person Singular Pronoun in Spanish.
mysearch <- act::search_new(examplecorpus, pattern= "yo")
mysearch
# Search in normalized content vs. original content
mysearch.norm  <- act::search_new(examplecorpus, pattern="yo", searchNormalized=TRUE)
mysearch.org   <- act::search_new(examplecorpus, pattern="yo", searchNormalized=FALSE)
mysearch.norm@results.nr
mysearch.org@results.nr

# The difference is because during normalization capital letters will be converted
# to small letters. One annotation in the example corpus contains a "yo" with a
# capital letter:
mysearch <- act::search_new(examplecorpus, pattern="yO", searchNormalized=FALSE)
mysearch@results$hit

# Search in full text vs. original content.
# Full text search will find matches across annotations.
# Let's define a regular expression with a certain span.
# Search for the word "no" 'no' followed by a "pero" 'but'
# in a distance ranging from 1 to 20 characters.
myRegEx <- "\\bno\\b.{1,20}pero"
mysearch <- act::search_new(examplecorpus, pattern=myRegEx, searchMode="fulltext")
mysearch
mysearch@results$hit

