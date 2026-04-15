# Suppress R CMD check NOTEs for variables used in debug blocks (if (1==2) { ... })
utils::globalVariables(c(
	"mergefolder",
	"template_path",
	"examplecorpus",
	"mysearch",
	"corpus"
))
