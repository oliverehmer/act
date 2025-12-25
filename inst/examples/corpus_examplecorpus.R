library(act)

# Summary of the data in the corpus
examplecorpus

# Summary of the data in th second transcripts in the corpus
examplecorpus@transcripts[[2]]

\dontrun{
# Download example corpus with media files
destinationpath <- 
temp <- tempfile()
download.file(options()$act.examplecorpusURL, temp)
unzip(zipfile=temp, exdir=destinationpath)

# Set the URL for the ZIP archive
url <- options()$act.examplecorpusURL

# Define output path
output_dir <- "/EXISTING_FOLDERON_YOUR_COMPUTER/examplecorpus"
output_path <- file.path(output_dir, "act_examplecorpus.zip")

# Download the ZIP file
download.file(url, output_path, mode = "wb")

# Unzip it to the output directory
unzip(output_path, exdir = output_dir)

# Rename the extracted folder
folder_old <- file.path(output_dir, "act_examplecorpus-main")
folder_new <- file.path(output_dir, "act_examplecorpus")
if (dir.exists(folder_new)) unlink(folder_new, recursive = TRUE)
file.rename(folder_old, folder_new)

}
