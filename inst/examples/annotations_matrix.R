library(act)

# An example replacement matrix comes with the package.
# It replaces most of the GAT conventions.
path <- system.file("extdata", "normalization", "normalizationMatrix.csv", package="act")

# Have a look at the matrix
mymatrix <- act::matrix_load(path)
mymatrix

# Apply matrix to examplecorpus
test <- act::annotations_matrix(x=examplecorpus, path_replacementMatrixCSV=path)

# Compare some annotations in the original examplecorpus object and
# in the modified corpus object test
examplecorpus@transcripts[[1]]@annotations$content[[1]]
test@transcripts[[1]]@annotations$content[[1]]

examplecorpus@transcripts[[2]]@annotations$content[[3]]
test@transcripts[[2]]@annotations$content[[3]]
