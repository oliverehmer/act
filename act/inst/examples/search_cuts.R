library(act)

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")

# Create print transcripts, media cutlists and .srt subtitles 
# for all search results
test <- act::search_cuts(x=examplecorpus, s=mysearch)

# Display all print transcripts on screen from @cuts.printtranscripts
cat(test@cuts.printtranscripts)

# Display cutlist on screen from @cuts.cutlist.mac
cat(test@cuts.cutlist.mac)

# Display .srt subtitles
cat(test@results[, mysearch@cuts.column.srt])

