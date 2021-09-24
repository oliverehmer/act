library(act)

# Lets search for instances where participants laugh together
# First search for annotations that contain laughter (in original content)
myRegEx <- "(\\brie\\b|\\briendo\\b)"
mysearch <- act::search_new(x=examplecorpus,
							pattern=myRegEx,
							searchNormalized = FALSE)
mysearch@results.nr

# Now perform sub search, also on laughs/laughing
test <- act::search_sub(x=examplecorpus,
						s=mysearch,
						pattern=myRegEx)

# Check the co-occurring search hits
test@results$subsearch
