library(act)

# Filter corpus to only contain some tiers
all.tier.names <- unique(act::tiers_all(examplecorpus)$name)
some.tier.names <- all.tier.names[1:10]
x <- act::transcripts_filter(examplecorpus, filterTierNames=some.tier.names)
x@history[[length(x@history)]]
