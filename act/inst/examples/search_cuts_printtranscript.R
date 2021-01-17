library(act)

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")

# Create print transcripts for all search results
test <- act::search_cuts_printtranscript (x=examplecorpus, s=mysearch)

# Display all print transcripts on screen from @cuts.printtranscripts
cat(test@cuts.printtranscripts)

# Display all print transcripts from results data frame
cat(test@results[,mysearch@cuts.column.printtranscript])
cat(stringr::str_c(test@results[,mysearch@cuts.column.printtranscript], sep="\n", collapse = "\n"))

# Only single print transcript from results data frame
cat(test@results[1,mysearch@cuts.column.printtranscript])

# Create print transcript snippets including 1 sec before and 5 sec after
mysearch@cuts.span.beforesec =1
mysearch@cuts.span.aftersec = 5
test <- act::search_cuts_printtranscript (x=examplecorpus,
s=mysearch)

# Display all transcript snippets on screen
cat(stringr::str_c(test@results[,mysearch@cuts.column.printtranscript], sep="\n", collapse = "\n"))
