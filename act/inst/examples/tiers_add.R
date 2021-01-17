library(act)

# --- Add new interval tier.
# Since not position is set it will be inserted in the end, by default.
x <- act::tiers_add(examplecorpus,
					tierName="TEST")
#check results
x@history[length(x@history)]
#have a look at the first transcript
x@transcripts[[1]]@tiers
#--> New tier is inserted in the end.

# --- Add new interval tier in position 2
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					absolutePosition=2)
#check results
x@history[length(x@history)]
#have a look at the first transcript
x@transcripts[[1]]@tiers
#--> New tier is inserted as second tier.


# --- Add new interval tier at the position of "Entrevistador", only if this tier exists,
# If the destination tier does not exist, the new tier will NOT be inserted.

#Have a look at the first and the second transcript. 
examplecorpus@transcripts[[1]]@tiers
#Transcript 1 does contain a tier "Entrevistador" in the first position.
examplecorpus@transcripts[[2]]@tiers
#Transcript 2 does contain a tier "Entrevistador" in the first position.

#Insert new tier
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					destinationTier="Entrevistador",
					relativePositionToDestinationTier=0,
					insertOnlyIfDestinationExists=TRUE)

#Check results
x@history[length(x@history)]
#Have a look at the transcript 1:
# Tier 'TEST' was in first position (e.g. where 'Entrevistador' was before).
x@transcripts[[1]]@tiers
#Have a look at the transcript 2:
#Tier 'TEST' was not inserted, since there was no destination tier 'Entrevistador'.
x@transcripts[[2]]@tiers

# --- Add new interval tier AFTER tier="Entrevistador"
# If the destination tier does not exist, the new tier will be inserted at the end in any case.
x <- act::tiers_add(examplecorpus,
					tierName="TEST",
					destinationTier="Entrevistador",
					relativePositionToDestinationTier=1,
					insertOnlyIfDestinationExists=FALSE)
#check results
x@history[length(x@history)]
#Have a look at the transcript 1:
# Tier 'TEST' was inserted after the tier 'Entrevistador'.
x@transcripts[[1]]@tiers
#Have a look at the transcript 2:
#Tier 'TEST' was insertedat the end.
x@transcripts[[2]]@tiers

