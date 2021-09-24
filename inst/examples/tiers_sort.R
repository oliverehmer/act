library(act)

# Check the order of the existing tiers in the first two transcripts
examplecorpus@transcripts[[1]]@tiers$name[order(examplecorpus@transcripts[[1]]@tiers$position)]
examplecorpus@transcripts[[2]]@tiers$name[order(examplecorpus@transcripts[[2]]@tiers$position)]

# Get tier names to create the sort vector
sortVector <- c(examplecorpus@transcripts[[1]]@tiers$name,
                examplecorpus@transcripts[[2]]@tiers$name)

# Revert the vector for demonstration.
sortVector <- sortVector[length(sortVector):1]

# This will only reorder the tiers.
examplecorpus <- act::tiers_sort(x=examplecorpus, 
sortVector=sortVector)

# Check again the order of the tiers
examplecorpus@transcripts[[1]]@tiers$name[order(examplecorpus@transcripts[[1]]@tiers$position)]
examplecorpus@transcripts[[2]]@tiers$name[order(examplecorpus@transcripts[[2]]@tiers$position)]

# This will reorder the tiers and additionally add tiers that are given  
# in the sort vector but not present in the transcript.
examplecorpus <- act::tiers_sort(x=examplecorpus,
sortVector=sortVector,
addMissingTiers=TRUE)
# Check again the order of the tiers
examplecorpus@transcripts[[1]]@tiers$name[order(examplecorpus@transcripts[[1]]@tiers$position)]
examplecorpus@transcripts[[2]]@tiers$name[order(examplecorpus@transcripts[[2]]@tiers$position)]

# Insert a tier called "newTier" into all transcripts in the corpus:
for (t in examplecorpus@transcripts) {
 sortVector <- c(t@tiers$name, "newTier")
 examplecorpus <- act::tiers_sort(x=examplecorpus,
sortVector=sortVector,
filterTranscriptNames=t@name,
addMissingTiers=TRUE)
}
# Check for example the first transcript: it now contains a tier called "newTier"
examplecorpus@transcripts[[1]]@tiers


# To get more examples and information about sorting see 'helper_tiers_sort_table()'.