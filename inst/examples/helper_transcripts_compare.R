library(act)

x <- examplecorpus@transcripts[[1]]
y <- x
y@annotations$content[1] <- "changed text"
y@annotations$content[5] <- "also changed"

result <- act::helper_transcripts_compare(x = x, y = y, gap = 1, digits = 3)
result$regions
result$tiers
result$media
