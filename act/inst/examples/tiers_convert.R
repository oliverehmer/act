library(act)

# Check the names and types of the existing tiers in the first two transcripts
examplecorpus@transcripts[[1]]@tiers
examplecorpus@transcripts[[2]]@tiers

# Convert interval tiers to point tiers
newcorpus <- act::tiers_convert(examplecorpus, intervalToPoint=TRUE)

# the names and types of the existing tiers
newcorpus@transcripts[[1]]@tiers
newcorpus@transcripts[[2]]@tiers

# Convert point tiers to interval tiers
newcorpus <- act::tiers_convert(newcorpus, pointToInterval=TRUE)

# Note: In this round trip conversion from 'interval > point > interval tier' 
# the original end times of the annotations get lost (when converting from interval > point).
