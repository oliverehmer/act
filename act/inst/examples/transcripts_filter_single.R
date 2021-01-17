library(act)

# get an example transcript
t1 <- examplecorpus@transcripts[[1]]

# --- Filter by tiers
# The example transcript contains two tiers that contain four annotations each.
t1@tiers
table(t1@annotations$tier.name)

# Filter transcript to only contain annotations of the FIRST tier
t2 <- act::transcripts_filter_single(t1, filterTierNames=t1@tiers$name[1])
t2@tiers
table(t2@annotations$tier.name)

# Use act::search_meta() first to get the tier names, 
# in this case search for tiers with a capital 'I',
# which is the second tier, called 'ISanti'
mymeta <- act::search_meta(examplecorpus, 
				filterTranscriptNames=t2@name,
				filterTierIncludeRegEx="I"
				)
t2 <- act::transcripts_filter_single(t1, filterTierNames=mymeta$tier.names)
t2@tiers
table(t2@annotations$tier.name)

# --- Filter by time section
# only set start of section (until the end of the transcript)
t2 <- act::transcripts_filter_single(t1, filterSectionStartsec=6)
cbind(t2@annotations$startSec,t2@annotations$endSec) 
	  
# only set end of section (from the beginning of the transcript)
t2 <- act::transcripts_filter_single(t1, filterSectionEndsec=8)
cbind(t2@annotations$startSec,t2@annotations$endSec) 

# set start and end of section
t2 <- act::transcripts_filter_single(t1, filterSectionStartsec=6, filterSectionEndsec=8)
cbind(t2@annotations$startSec,t2@annotations$endSec) 

# set start and end of section, start new times from 0
t2 <- act::transcripts_filter_single(t1, 
 filterSectionStartsec=6, 
 filterSectionEndsec=8,
 preserveTime=FALSE)
cbind(t2@annotations$startSec,t2@annotations$endSec) 

