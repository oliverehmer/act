library(act)

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")

# Create srt subtitles for all search results
test <- act::search_cuts_srt (x=examplecorpus, s=mysearch)

# Display srt subtitle of first three results
cat(test@results[1:3, mysearch@cuts.column.srt])

# Create srt subtitle including 1 sec before and 5 sec after
mysearch@cuts.span.beforesec = 1
mysearch@cuts.span.aftersec = 5
test <- act::search_cuts_srt (x=examplecorpus,
								s=mysearch)

# Display srt subtitle of first  results
cat(test@results[1,mysearch@cuts.column.srt])
