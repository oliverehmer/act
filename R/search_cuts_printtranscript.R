#' Create print transcripts for all search results
#'
#' Print transcripts in the style of conversation analysis will be created for each search result.
#' The transcripts will be inserted into the column defined in \code{s@cuts.column.printtranscript}.
#' All transcripts will be stored in \code{s@cuts.printtranscripts}.
#'
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param l Layout object.
#' @param exportTxt Logical; If \code{TRUE} and \code{folderOutput} is set, .txt transcripts will be written to disk. The print transcripts are always inserted into the column \code{s@cuts.column.printtranscript} of \code{s@results}, independent of this parameter.
#' @param exportDocx Logical; If \code{TRUE} and \code{folderOutput} is set, .docx transcripts will be written to disk.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param folderOutput Character string; path to an existing folder for file export. If \code{NULL} (default), no files will be written to disk; the print transcripts are inserted only into \code{s@results}. If set, .txt and/or .docx files will be written depending on \code{exportTxt} and \code{exportDocx}.
#'
#' @return Search object; 
#' 
#' @export
#'
#' @example inst/examples/search_cuts_printtranscript.R
#' 
search_cuts_printtranscript <- function(x, 
										s,
										l                  = NULL, 
										exportTxt          = TRUE,
										exportDocx         = TRUE,
										headerInsertSource = TRUE,
										cutSpanBeforesec   = 0,
										cutSpanAftersec    = 0,
										folderOutput       = NULL ) {
	if (1==2) {
		x <- examplecorpus
		s <- mysearch
		l<-NULL
		exportTxt <- TRUE
		exportDocx <- TRUE
		cutSpanBeforesec<-0
		cutSpanAftersec <- 0
		folderOutput<-'/Users/oliverehmer/Downloads'
	}
	
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	if (is.null(s@results$transcriptName)) 		{ cli::cli_abort("Data frame s@results does not contain column {.arg transcriptName}") 	}
	
	if (is.null(l)) 	{
		l <- methods::new("layout")
	}	
	
	if (is.null(cutSpanBeforesec)) 	{
		cutSpanBeforesec <- 0
	} else {
		if (length(cutSpanBeforesec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanBeforesec)) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to be a numeric value.")
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	
	if (is.null(cutSpanAftersec)) 	{
		cutSpanAftersec <- 0
	} else {
		if (length(cutSpanAftersec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanAftersec)) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to be a numeric value.")
		}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}
	
	#--- check if output folder is given
	folder.destination <- NULL
	if (!is.null(folderOutput)) {
		if (!exportTxt && !exportDocx) {
			cli::cli_warn("Parameter {.arg folderOutput} is set but both {.arg exportTxt} and {.arg exportDocx} are {.val FALSE} - no files will be written.")
		}
		if (!options()$act.export.filename.fromColumnName %in% colnames(s@results)) {
			cli::cli_abort("The column defined in the option 'options()$act.export.filename.fromColumnName' does not exist in the data.frame with the search results.")
		}
		folder.destination <- normalizePath(folderOutput, winslash = "/", mustWork = FALSE)
		if (dir.exists(folder.destination)==FALSE) 	{
			cli::cli_abort("Output folder does not exist.")
		}
		
		#create a sub folder for the search
		if (s@name!="") 	{
			folder.destination <- file.path(folder.destination, s@name)
			dir.create(folder.destination, showWarnings = FALSE)
			if (file.exists(folder.destination)==FALSE) 		{
				cli::cli_abort("Unable to create output directory")
			}
		}
	}	
	myWarnings <- character(0)
	
	#create sub folders for text
	if(!is.null(folder.destination)) {
		dir.create(file.path(folder.destination, "transcripts"), showWarnings = FALSE)
	}
	helper_progress_set("Creating transcripts",max(1,nrow(s@results)))
	
	trans.txt.all <- c()
	#i<-1
	if (nrow(s@results)>0) {
		for (i in 1:nrow(s@results)) 	{
			helper_progress_tick()
			
			#==== GET TRANSCRIPT ====
			t <- NULL
			if (is.null(s@results$transcriptName[i])) {
				#transcript not found
				myWarnings <- c(myWarnings, sprintf("result %s '%s': transcript '%s' not found in corpus", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]), as.character(s@results$transcriptName[i])))

			} else {
				t <- x@transcripts[[ s@results$transcriptName[i] ]]
				if (is.null(t)) {
					#transcript not found
					myWarnings <- c(myWarnings, sprintf("result %s '%s': transcript '%s' not found in corpus", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]), as.character(s@results$transcriptName[i])))
				}
			}
			#next t is null
			if (is.null(t)) { next}
			
			#==== FILE NAME ====
			filename.col <- options()$act.export.filename.fromColumnName
			if (filename.col %in% colnames(s@results)) {
				filename <- as.character(s@results[i, filename.col])
			} else {
				filename <- as.character(i)
			}

			if (!exists("filename")) {
				filename <- as.character(i)
			} else if (is.na(filename)) {
				filename <- as.character(i)
			} else if (length(filename)==0) {
				filename <- as.character(i)
			} else {
				if (filename == "") {filename <- as.character(i)}
			}
			
			#==== TIME ====
			startSec 	<- max(0, s@results$startsec[i] - s@cuts.span.beforesec)
			endSec 		<- min(s@results$endsec[i] + s@cuts.span.aftersec, t@length.sec)
			
			#==== ARROW ====
			if (!l@arrow.insert) {
				arrowStartsec  <- NA_real_
				arrowEndsec    <- NA_real_
				arrowTierName  <- NA_character_
			} else {
				arrowStartsec  <- s@results$startsec[i]
				arrowEndsec    <- s@results$endsec[i]
				arrowTierName  <- s@results$tierName[i]
			}
			#View(s@results)
			#==== HEADER ====
			headerPreface	 	<- NULL
			headerTitle 		<- NULL
			headerSubtitle	 	<- NULL
			headerDescription 	<- NULL
			if (l@header.insert==TRUE) {
				if ("header.preface" %in% colnames(s@results)) {
					headerPreface <- as.character(s@results$header.preface[i])
				}
				if ("header.title" %in% colnames(s@results)) {
					headerTitle <- as.character(s@results$header.title[i])
				} else {
					#if title is not given, take resultID
					if ("header.title" %in% colnames(s@results)) {
						headerTitle <-  as.character(s@results$resultID[i])
					}
				}
				if ("header.subtitle" %in% colnames(s@results)) {
					headerSubtitle <- as.character(s@results$header.subtitle[i])
				}
				if ("header.description" %in% colnames(s@results)) {
					headerDescription <- as.character(s@results$header.description[i])
				}
			}
			
			#==== CREATE TRANS =====
			#---- .txt ----
			path.file <- NULL
			if (exportTxt && !is.null(folder.destination)) {
				path.file <- file.path(folder.destination, "transcripts", paste(filename, ".txt", sep=""))
			}

			trans.txt <- act::export_txt(   t = x@transcripts[[ s@results$transcriptName[i] ]],
											l = l,
											pathOutput				  = path.file,
											filterSectionStartsec     = startSec,
											filterSectionEndsec       = endSec,
											headerPreface 	 	      = headerPreface,
											headerTitle 	 	      = headerTitle,
											headerSubtitle 	          = headerSubtitle,
											headerDescription 	      = headerDescription,
											headerInsertSource        = headerInsertSource,
											insertArrowStartsec       = arrowStartsec,
											insertArrowEndsec         = arrowEndsec,
											insertArrowTierName       = arrowTierName
			)

			#---- . store values to columns ----
			#add  column if missing
			if (!s@cuts.column.printtranscript %in% colnames(s@results)) {
				s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
				colnames(s@results)[ncol(s@results)] <- s@cuts.column.printtranscript
			}
			#insert transcript into search results
			output <- stringr::str_flatten(trans.txt, collapse="\n")
			s@results[i, s@cuts.column.printtranscript] <- output

			if (exportTxt) {
				#accumulate transcripts
				trans.txt.all <- c(trans.txt.all, trans.txt, "","")
			}
			
			if (exportDocx) {
				#path
				path.file <- NULL
				if (!is.null(folder.destination)) {
					path.file <- file.path(folder.destination, "transcripts", paste(filename, ".docx", sep=""))
				} 
				
				#export
				trans.doxc<- act::export_docx(  t = x@transcripts[[ s@results$transcriptName[i] ]],
												l = l,
												pathOutput				  = path.file,
												filterSectionStartsec     = startSec,
												filterSectionEndsec       = endSec,
												insertArrowStartsec       = arrowStartsec,
												insertArrowEndsec         = arrowEndsec,
												insertArrowTierName       = arrowTierName,
												headerPreface 	 	      = headerPreface,
												headerTitle 	 	      = headerTitle,
												headerSubtitle 	          = headerSubtitle,
												headerDescription 	      = headerDescription,
												headerInsertSource        = headerInsertSource
				)
			}
		} #next i
	}
	s@cuts.printtranscripts <- stringr::str_flatten(trans.txt.all, collapse="\n")
	
	#==== SAVE TO FILES ====
	# if output folder is given
	if (!is.null(folder.destination)) { 
		if (!folder.destination=="NULL") {
			
			#---- . txt ----
			if(exportTxt) {
				path.file 	<- file.path(folder.destination, paste("searchResults_",  s@name, ".txt", sep=""))
				fileConn 	<- file(path.file, open="wb")
				writeLines(enc2utf8(as.character(trans.txt.all)), fileConn, sep="\n", useBytes=TRUE)
				close(fileConn)
			}
			
			#---- . docx ----
			if(exportDocx) {
				# Resolve all templates the layout carries (1..N).
				# Per-result files were written by export_docx with the same suffix scheme,
				# so each template merges only its own per-result files.
				templates <- .resolve_docx_templates(l)
				template_suffixes <- if (length(templates) <= 1) {
					""
				} else {
					paste0("__", names(templates))
				}

				folder.input <- folder.destination

				for (ti in seq_along(templates)) {
					suffix <- template_suffixes[ti]

					# Filter per-result files by suffix. Empty suffix -> match any .docx.
					filterRegex <- if (nzchar(suffix)) {
						paste0(stringr::str_replace_all(suffix, "([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|])", "\\\\\\1"), "\\.docx$")
					} else {
						NULL
					}

					out.path <- file.path(folder.destination,
										  paste0("searchResults_", s@name, suffix, ".docx"))

					result <- merge_docx(folderInput        = folder.input,
										 pathTemplateInput  = templates[ti],
										 pathOutput         = out.path,
										 recursive          = TRUE,
										 filterRegex        = filterRegex)
				}
			}
			
			#---- . R ----
			path_R 	    <-	file.path(folder.destination, 	 paste("searchResults_", s@name, ".RData", sep=""))
			save(s, file = path_R)
			
			#---- . csv ----
			path_CSV 		<- file.path(folder.destination, 	paste("searchResults_", s@name, ".csv", sep=""))
			act::search_results_export(s, path_CSV, saveAsCSV = TRUE)
			
			#---- . xlsx ----
			path_XLSX  <- file.path(folder.destination, paste("searchResults_", s@name, ".xlsx", sep=""))
			act::search_results_export(s, path_XLSX)
		}
		
	}
	#=== print warnings
	if (length(myWarnings) > 0) {
		w <- unique(myWarnings)
		cli::cli_warn(c("{length(w)} transcript(s) not found in corpus:", stats::setNames(w, rep("-", length(w)))))
	}
	
	#=== give modified results back
	return(s)
}
