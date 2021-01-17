library(act)

# Get the transcript you want to export
t <- examplecorpus@transcripts[[1]]

# Create temporary file path
path <- tempfile(pattern = t@name, tmpdir = tempdir(), fileext = ".exb")

# It makes more sense, however, to you define a destination folder
# that is easier to access on your computer
\dontrun{
path <- file.path("PATH_TO_AN_EXISTING_FOLDER_ON_YOUR_COMPUTER",
                   paste(t@name, ".exb", sep=""))
}

# Export WITH media links
act::export_exb(t=t, outputPath=path)

# Export WITHOUT media links
act::export_exb(t=t, outputPath=path, createMediaLinks = FALSE)

