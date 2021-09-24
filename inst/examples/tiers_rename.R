library(act)

# Check the names of the existing tiers in the first two transcripts
examplecorpus@transcripts[[1]]@tiers$name
examplecorpus@transcripts[[2]]@tiers$name

x <- act::tiers_rename(examplecorpus, "Entrevistador", "E")

x@transcripts[[1]]@tiers$name
x@transcripts[[2]]@tiers$name

