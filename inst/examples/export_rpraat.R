library(act)

# Convert
rpraat.tg <- act::export_rpraat(t=examplecorpus@transcripts[[1]])

# Now you can use the object in the rPraat pachage.
# For instance you can plot the TextGrid
\dontrun{
	rPraat::tg.plot(rpraat.tg)
}