library(act)

# Path to the .TextGrid file that you want to read
path <- system.file("extdata", "examplecorpus", "GAT", 
					"ARG_I_PAR_Beto.TextGrid", package="act")

# To import a .TextGrid file of your choice:
\dontrun{
	path <- "PATH_TO_AN_EXISTING_TEXTGRID_ON_YOUR_COMPUTER"
}

# Make sure to have rPraat installed before you try the following
\dontrun{
	# Read TextGrid file with rPraat
	rPraat.tg <- rPraat::tg.read(path)

	# Convert to an act transcript
	t <- act::import_rpraat(rPraat.tg)
	
	# Change the name and add it to the examplecorpus
	t@name <-"rpraat"
	newcorpus <- act::transcripts_add(examplecorpus, t)
	
	# Have a look
	newcorpus@transcripts[["rpraat"]]
	
	# Alternatively, you can use the general import function
	t <- act::import(rPraat.tg)
}
