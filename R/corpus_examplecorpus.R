#' Example corpus for the act package
#' 
#' Example corpus with data loaded from the example annotations files that come with the package
#'
#' The corpus comes without media files. To try it with its media (about 64 MB,
#' downloaded once from GitHub, \url{https://github.com/oliverehmer/act_examplecorpus/})
#' use \link{examplecorpus_media}: \code{x <- act::examplecorpus_media()}.
#' The paths in the corpus are relative to the example folder
#' \code{system.file("extdata", "examplecorpus", package = "act")}. It is built by
#' \code{data-raw/examplecorpus_build.R} of the package sources.
#' @docType data
#'
#' @usage data(examplecorpus)
#'
#' @format An object of class \code{"corpus"}
#'
#' @source  
#'  * GAT: Ehmer, Oliver/Satti, Luis Ignacio/Martinez, Angelita/Pfaender, Stefan (2019): Un sistema para transcribir el habla en la interaccion: GAT 2.0 Gespraechsforschung - Online-Zeitschrift zur verbalen Interaktion (www.gespraechsforschung-ozs.de) 20, 64-114. http://www.gespraechsforschung-online.de/2019.html
#'  * SYNC:	Ehmer, Oliver (2020, in press): Synchronization in demonstrations. Multimodal practices for instructing body knowledge. Linguistics Vanguard. https://www.degruyter.com/view/journals/lingvan/lingvan-overview.xml
#' 
#' @seealso , \link{corpus_new}, \link{corpus_import}
#' 
#' @example inst/examples/corpus_examplecorpus.R
#' 
"examplecorpus"