library(act)

# The example files that come with the act library are located here:
path <- system.file("extdata", "examplecorpus", package="act")

# This is the examplecorpus object that comes with the library
examplecorpus

# Make sure that the input folder of the example corpus object is set correctly
examplecorpus@paths.annotation.files <- path
examplecorpus@paths.media.files <- path

# Load annotation files into the corpus object (again)
examplecorpus <- act::corpus_import(x=examplecorpus)

# Creating the full texts may take a long time.
# If you do NOT want to create the full texts immediately use the following command:
examplecorpus <- act::corpus_import(x=examplecorpus, createFullText=FALSE )
