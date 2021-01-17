library(act)

# get info about all tiers
all.tiers <- act::info(examplecorpus)$tiers

# tiers 'A' and 'B' occur 6 times in 6 transcripts
all.tiers["A", "tiers.count"]
all.tiers["B", "tiers.count"]

# delete tiers
tierNames <- c("A", "B")
x<- examplecorpus
x <- act::tiers_delete(examplecorpus, tierNames=tierNames)
x@history[length(x@history)]

# tiers 'A' and 'B' do not occur anymore
act::info(x)$tiers$tier.name
