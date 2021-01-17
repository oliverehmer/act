library(act)

# get current names 
old.names <- names(examplecorpus@transcripts)

# make vector of names with the same length
new.names <- paste("transcript", 1:length(old.names), sep="")

# rename the transcripts
test <- act::transcripts_rename(examplecorpus, newTranscriptNames=new.names)

# check
names(test@transcripts)
test@transcripts[[1]]@name
test@history[length(test@history)]

# convert to lower case
test <- act::transcripts_rename(examplecorpus, toLowerCase=TRUE)
test@history[length(test@history)]

# search replace
test <- act::transcripts_rename(examplecorpus, 
 searchPatterns=c("ARG", "BOL"), 
 searchReplacements = c("ARGENTINA", "BOLIVIA")
)
test@history[length(test@history)]

# search replace ignoring upper and lower case
test <- act::transcripts_rename(examplecorpus, 
 searchPatterns=c("(?i)arg", "(?i)bol"), 
 searchReplacements = c("ARGENTINA", "BOLIVIA")
)
test@history[length(test@history)]


# search replace too much
test <- act::transcripts_rename(x=examplecorpus, 
 searchPatterns="ARG_I_CHI_Santi", 
 searchReplacements = "")
names(test@transcripts)[1]

