library(act)

before <- examplecorpus@transcripts[[1]]
after  <- before
after@annotations$content[1] <- "changed text"
after@annotations <- after@annotations[-2, ]

patch <- act::helper_transcript_patch_make(before = before, after = after, tierNames = NULL)
patch$annotations$removed
patch$annotations$added

undone <- act::helper_transcript_patch_apply(x = after, patch = patch, direction = "backward")
identical(undone@annotations$content, before@annotations$content)

redone <- act::helper_transcript_patch_apply(x = undone, patch = patch, direction = "forward")
identical(redone@annotations$content, after@annotations$content)
