library(act)

# An example replacement matrix comes with the package.
path <- system.file("extdata", "normalization", "normalizationMatrix.csv", package="act")

# Load the matrix
mymatrix <- act::matrix_load(path)

# ' # Create temporary file path
path <- tempfile(pattern = "mymatrix", tmpdir=tempdir(),  fileext = ".csv")

# It makes more sense, however, to you define a destination folder
# that is easier to access on your computer:
\dontrun{
path <- file.path("PATH_TO_AN_EXISTING_FOLDER_ON_YOUR_COMPUTER",
                  "mymatrix.csv")
}

# Save the matrix
act::matrix_save(mymatrix, path=path)

