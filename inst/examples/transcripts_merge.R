library(act)

# We need three transcripts  to demonstrate the function \code{transcripts_merge}:
# - the destination transcript: "update_destination"
# - two transcripts that contain updates: "update_update1 and "update_update2"

#Have a look at the annotations in the destination transcript first. 
#It contains 2 annotations:
examplecorpus@transcripts[["update_destination"]]@annotations
#Have a look at the annotations in the update_update1 transcript, too: 
#It contains 3 annotations:
examplecorpus@transcripts[["update_update1"]]@annotations

# Run the function with only one update:
test <- act::transcripts_merge(x=examplecorpus,
   destinationTranscriptName="update_destination", 
   updateTranscriptNames = "update_update1")

#Have a look at the annotations in the destination transcript again.
#It now contains 5 annotations:
test@transcripts[["update_destination"]]@annotations


# Run the function with two transcript objects for updates:
test <- act::transcripts_merge(x=examplecorpus,
	destinationTranscriptName="update_destination", 
	updateTranscriptNames = c("update_update1","update_update2"))

#Have a look at the annotations in the destination transcript again.
#It now contains 8 annotations:
test@transcripts[["update_destination"]]@annotations

# Compare the transcript in the original and in the modified corpus object. 
# The update transcript objects are gone:
act::info_summarized(examplecorpus)$transcript.names
act::info_summarized(test)$transcript.names
	
#Have a look at the history of the corpus object
test@history
