library(act)

# --- annotationsWithReversedTimes: will be deleted
# get example transcript and reverse the times of an annotation
t <- examplecorpus@transcripts[[1]]
t@annotations$startSec[1] <- 20
t@annotations$endSec[1] <- 10
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- annotationsWithTimesBelowZero: will be deleted or start at 0 sec
t <- examplecorpus@transcripts[[1]]
t@annotations$startSec[1] <- -2
t@annotations$endSec[1]   <- -1
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)

t <- examplecorpus@transcripts[[1]]
t@annotations$startSec[2] <- -5
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- overlappingAnnotations: will end where the next starts
t<-examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tier.name, t@annotations$startSec), ]
t@annotations$endSec[1] <- 8
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- missingTiers: will be added to @tiers in transcript object
t<-examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tier.name, t@annotations$startSec), ]
t@annotations$tier.name[1] <- "NEW"
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)
t2@tiers
# compare with original tiers
t@tiers


# --- several things at once
t<-examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tier.name, t@annotations$startSec), ]
# annotation completely below 0 sec
t@annotations$startSec[1] <- -6
t@annotations$endSec[1]   <- -5
# annotation starts before but ends after 0 sec
t@annotations$startSec[2] <- -3
# annotation with reversed times
t@annotations$startSec[3] <- 6.9
t@annotations$endSec[3]   <- -6.8
# annotation overlaps with next annotation
t@annotations$endSec[6]   <- 9
# new tier, missing tier list
t@annotations$tier.name[8] <- "NEW"
t2 <- act::transcripts_cure_single(t, showWarning=TRUE)
tail(t2@history, n=1)


examplecorpus@transcripts[[1]]@history
