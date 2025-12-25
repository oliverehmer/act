library(act)

# get an example transcript
t1 <- examplecorpus@transcripts[[1]]

# --- Filter by tiers
# The example transcript contains two tiers that contain four annotations each.
t1@tiers
table(t1@annotations$tierName)

# Filter transcript to only contain annotations of the FIRST tier
t2 <- act::transcripts_filter_single(t1, filterTierNames=t1@tiers$name[1])
t2@tiers
table(t2@annotations$tierName)

# Use act::search_makefilter() first to get the tier names, 
# in this case search for tiers with a capital 'I',
# which is the second tier, called 'ISanti'
myfilter <- act::search_makefilter(examplecorpus, 
				filterTranscriptNames=t2@name,
				filterTierIncludeRegex="I"
				)
t2 <- act::transcripts_filter_single(t1, filterTierNames=myfilter$tierNames)
t2@tiers
table(t2@annotations$tierName)

# --- Filter by time section
# only set start of section (until the end of the transcript)
t2 <- act::transcripts_filter_single(t1, filterSectionStartsec=6)
cbind(t2@annotations$startsec,t2@annotations$endsec) 
	  
# only set end of section (from the beginning of the transcript)
t2 <- act::transcripts_filter_single(t1, filterSectionEndsec=8)
cbind(t2@annotations$startsec,t2@annotations$endsec) 

# set start and end of section
t2 <- act::transcripts_filter_single(t1, 
  filterSectionStartsec=6,
  filterSectionEndsec=8)
cbind(t2@annotations$startsec,t2@annotations$endsec) 

# set start and end of section, start new times from 0
t2 <- act::transcripts_filter_single(t1, 
 filterSectionStartsec=6, 
 filterSectionEndsec=8)
cbind(t2@annotations$startsec,t2@annotations$endsec) 

