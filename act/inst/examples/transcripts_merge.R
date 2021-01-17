library(act)

# We have three transcripts that to demonstrate the function \code{transcripts_merge}:
# - the destination transcript
trans_destination <- 	examplecorpus@transcripts[["update_destination"]]
# - two transcripts that contain updates
trans_updates <- 		c(examplecorpus@transcripts[["update_update1" ]],
                           examplecorpus@transcripts[["update_update2" ]])

# Run the function
test <- transcripts_merge(trans_destination, trans_updates)

# Save the transcript to a TextGrid file.
# Set the destination file path
path <- tempfile(pattern = "merge_test", tmpdir = tempdir(),
                 fileext = ".TextGrid")

# It makes more sense, however, to you define a destination folder
# that is easier to access on your computer:
\dontrun{
path <- file.path("PATH_TO_AN_EXISTING_FOLDER_ON_YOUR_COMPUTER",
                    paste(t@name, ".TextGrid", sep=""))
}

# Export
act::export_textgrid( t=test, outputPath=path)

