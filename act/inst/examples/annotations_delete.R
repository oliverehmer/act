library(act)

# Set the regular expression which annotations should be deleted.
# In this case: all annotations that contain the letter "a"
myRegEx <- "a"

# Have a look at all annotations in the first transcript
examplecorpus@transcripts[[1]]@annotations$content

# Some of them match to the regular expression
hits <- grep(pattern=myRegEx, x=examplecorpus@transcripts[[1]]@annotations$content)
examplecorpus@transcripts[[1]]@annotations$content[hits]
# Others don't match the regular expression
examplecorpus@transcripts[[1]]@annotations$content[-hits]

# Run the function and delete the annotations that match the regular expression
test <- act::annotations_delete (x=examplecorpus, filterContent=myRegEx)

# Compare how many data rows are in the first transcript in 
# the example corpus and in the newly created test corpus:
nrow(examplecorpus@transcripts[[1]]@annotations)
nrow(test@transcripts[[1]]@annotations)

# Only the annotations are left, that did not match the regular expression:
test@transcripts[[1]]@annotations$content