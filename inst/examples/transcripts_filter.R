library(act)

# Filter corpus to only contain some tiers
all.tierNames <- unique(act::tiers_all(examplecorpus)$name)
some.tierNames <- all.tierNames[1:10]
x <- act::transcripts_filter(examplecorpus, filterTierNames=some.tierNames)
x@history[[length(x@history)]]
