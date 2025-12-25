library(act)

# --- annotationsTimesReversed: will be deleted
# get example transcript and reverse the times of an annotation
t <- examplecorpus@transcripts[[1]]
t@annotations$startsec[1] <- 20
t@annotations$endsec[1] <- 10
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- annotationsTimesBelowZero: will be deleted or start at 0 sec
t <- examplecorpus@transcripts[[1]]
t@annotations$startsec[1] <- -2
t@annotations$endsec[1]   <- -1
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)

t <- examplecorpus@transcripts[[1]]
t@annotations$startsec[2] <- -5
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- annotationsOverlap: will end where the next starts
t<- examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]
t@annotations$endsec[1] <- 8
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)


# --- tiersMissing: will be added to @tiers in transcript object
t<- examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]
t@annotations$tierName[1] <- "NEW"
t2 <- act::transcripts_cure_single(t)
tail(t2@history, n=1)
t2@tiers
# compare with original tiers
t@tiers


# --- several things at once
t<- examplecorpus@transcripts[[1]]
t@annotations <- t@annotations[order(t@annotations$tierName, t@annotations$startsec), ]
# annotation completely below 0 sec
t@annotations$startsec[1] <- -6
t@annotations$endsec[1]   <- -5
# annotation starts before but ends after 0 sec
t@annotations$startsec[2] <- -3
# annotation with reversed times
t@annotations$startsec[3] <- 6.9
t@annotations$endsec[3]   <- -6.8
# annotation overlaps with next annotation
t@annotations$endsec[6]   <- 9
# new tier, missing tier list
t@annotations$tierName[8] <- "NEW"
t2 <- act::transcripts_cure_single(t, warning=TRUE)
tail(t2@history, n=1)


examplecorpus@transcripts[[1]]@history
