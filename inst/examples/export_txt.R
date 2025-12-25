library(act)

# Get a transcript
t <- examplecorpus@transcripts[[1]]

# Create print transcript
printtranscript <- act::export_txt (t=t)

# Display on screen
cat(printtranscript)

