# Builds data/examplecorpus.RData from the annotation files in
# inst/extdata/examplecorpus. Run it after every change to the transcript or
# corpus class or to the import functions:
#   Rscript act/data-raw/examplecorpus_build.R        (from the folder above act)
#   source("data-raw/examplecorpus_build.R")          (inside act)
# The corpus is built without media (a link would be a path on one computer;
# act::examplecorpus_media() downloads the media and links them). Every path
# is stored relative to the example folder, so no path of the building
# computer ends up in the package.
root <- if (file.exists("DESCRIPTION") && dir.exists("data-raw")) "." else "act"
pkgload::load_all(root, quiet = TRUE)
source_folder <- normalizePath(file.path(root, "inst", "extdata", "examplecorpus"), winslash = "/")

examplecorpus <- act::corpus_new(pathsAnnotationFiles = source_folder,
                                 pathsMediaFiles      = NULL,
                                 name                 = "examplecorpus",
                                 assignMedia          = FALSE,
                                 verbose              = FALSE)

# every character value that starts with the source folder becomes relative
prefix <- paste0(source_folder, "/")
relative <- function(v) {
  if (is.character(v)) return(ifelse(startsWith(v, prefix), substring(v, nchar(prefix) + 1L), ifelse(v == source_folder, "", v)))
  if (is.list(v) && !is.data.frame(v)) return(lapply(v, relative))
  if (is.data.frame(v)) { for (n in names(v)) v[[n]] <- relative(v[[n]]); return(v) }
  v
}
for (nm in names(examplecorpus@transcripts)) {
  t <- examplecorpus@transcripts[[nm]]
  t@file.path  <- relative(t@file.path)
  t@file.state <- relative(t@file.state)
  t@history    <- relative(t@history)
  examplecorpus@transcripts[[nm]] <- t
}
examplecorpus@paths.annotation.files <- character()
examplecorpus@paths.media.files      <- character()
examplecorpus@import.results         <- relative(examplecorpus@import.results)
# the normalization matrix of the package names its file: keep only the part below extdata/
nm_path <- attr(examplecorpus@normalization.matrix, "path")
if (!is.null(nm_path)) attr(examplecorpus@normalization.matrix, "path") <- sub("^.*/extdata/", "", nm_path)

# nothing of the building computer may remain
package_root <- normalizePath(root, winslash = "/")
leaks <- grep(package_root, capture.output(str(examplecorpus, max.level = 8)), value = TRUE, fixed = TRUE)
if (length(leaks)) stop("absolute paths left in the corpus: ", paste(head(leaks, 3), collapse = " | "))

target <- file.path(root, "data", "examplecorpus.RData")
save(examplecorpus, file = target, compress = "xz")
check <- new.env(); load(target, envir = check)
stopifnot(methods::is(check$examplecorpus, "corpus"), length(check$examplecorpus@transcripts) == 17L,
          all(vapply(check$examplecorpus@transcripts, function(t) length(t@file.state) > 0, logical(1))))
cat("examplecorpus:", length(check$examplecorpus@transcripts), "transcripts ->", target, "\n")
