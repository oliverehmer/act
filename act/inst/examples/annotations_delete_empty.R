library(act)

# In the example corpus are no empty annotations.
# Empty annotations are deleted by default when annotation files are loaded.
# So let's first make an empty annotation.

# Check the first annotation in the first transcript
examplecorpus@transcripts[[1]]@annotations$content[[1]]

# Empty the contents of this annotation
examplecorpus@transcripts[[1]]@annotations$content[[1]] <- ""

# Run the function
test <- act::annotations_delete_empty (x=examplecorpus)

# Compare how many data rows are in the first transcript in
# the example corpus and in the newly created test corpus:
nrow(examplecorpus@transcripts[[1]]@annotations)
nrow(test@transcripts[[1]]@annotations)

