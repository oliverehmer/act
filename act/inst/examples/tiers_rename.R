library(act)

# Check the names of the existing tiers in the first two transcripts
examplecorpus@transcripts[[1]]@tiers$name
examplecorpus@transcripts[[2]]@tiers$name

examplecorpus <- act::tiers_rename(examplecorpus, "Entrevistador", "E")

examplecorpus@transcripts[[1]]@tiers$name
examplecorpus@transcripts[[2]]@tiers$name

