library(act)

# An example replacement matrix comes with the package.
path <- system.file("extdata", "normalization", "normalizationMatrix.csv", package="act")

# Load the matrix
mymatrix <- act::matrix_load(path)

# Have a look at the matrix
colnames(mymatrix)
mymatrix

#the original path of the matrix is stored in the attributes
attr(mymatrix, 'path') 
