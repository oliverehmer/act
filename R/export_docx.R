#' Export print transcript in .docx format
#' 
#' LAYOUT
#' If you want to modify the layout of the print transcripts, create a new layout object with \code{mylayout <- methods::new("layout")}, modify the settings and pass it as argument \code{l}.
#' Using the layout object you may
#' - Adjust with, abbreviation of speakers, etc.
#' - set filters to include/exclude tiers matching regular expressions.
#' - assign template files for .docx formatting using format templates
#'
#' FORMATING
# To format the transcript you can 
#' - adjust the the defaults format templates in the default .docx template.
#' - define further templates and add them to a styles matrix.
#' The paths to both files need to be set in your l layout object. Please check the slots l@docx.template.path and l@docx.styles.base.
#' You can see the structure of the default styles matrix in each new layout object in l@docx.styles.base. Use l@docx.styles.base <- act::export_styles_base_load(...) to assign a custom styles matrix.
#' The default format templates are
#' * Header: 
#' - header.preface (formats: s@results$header.description) 
#' - header.title (formats: s@results$header.description) 
#' - header.subtitle (formats: s@results$header.description) 
#' - header.description (formats: s@results$header.description) 
#' * Transcript body
#' - transcript.default (formats: any annotation in "t@annotations"
#' 
#'
#' @param t Transcript object.
#' @param l Layout object.
#' @param pathOutput Character string; path where to save the transcript.
#' @param filterTierNames Vector of character strings; names of tiers to be included. If left unspecified, all tiers will be exported.
#' @param filterSectionStartsec Double; start of selection in seconds.
#' @param filterSectionEndsec Double; end of selection in seconds.
#' @param insertArrowAnnotationID Integer; ID of the annotation in front of which the arrow will be placed.
#' @param headerPreface Character string; text used as preface before title.
#' @param headerTitle Character string; text used as title.
#' @param headerSubtitle Character string; text  used as sub title.
#' @param headerDescription Character string; text used as description after sub title.
#' @param headerInsertSource Logical; if \code{TRUE} standard information about the source and location of the sequence will be inserted after the heading.

#' 
#' @return Officer doc; transcript as object from library officer.
#' 
#' @seealso \link{corpus_export}, \link{export_eaf}, \link{export_exb}, \link{export_rpraat}, \link{export_srt}, \link{export_textgrid}, \code{vignette("export_docx_styles", package = "act")}
#' 
#' @export
#'
#' @example inst/examples/export_docx.R
#' 
export_docx <- function (   t,
							l                            = NULL,
							pathOutput                   = NULL,
							filterTierNames              = NULL,
							filterSectionStartsec        = NULL,
							filterSectionEndsec          = NULL,
							insertArrowAnnotationID      = "",
							headerPreface                = NULL,
							headerTitle                  = NULL,
							headerSubtitle               = NULL,
							headerDescription            = NULL,
							headerInsertSource           = TRUE
) {
	# 
	
	if (1==2) {
	#	#t <- corpus@transcripts[[1]]
	#	i<-1
	#	t <- corpus@transcripts[[s@results$transcriptName[i ]]]
	#	l <- l
	#	pathOutput             <- '/Users/oliverehmer/Downloads/text.docx'
	#	filterTierNames        <- NULL
	#	filterSectionStartsec  <- s@results$startsec[i]
	#	filterSectionEndsec    <- s@results$endsec[i]
	#	insertArrowAnnotationID<- s@results$annotationID[i]
	#	headerTitle            <- s@results$header.title[i]
	#	headerSubtitle         <- s@results$header.subtitle[i]
	#	headerInsertSource     <- FALSE
	}
	
	if (missing(t)) 	{cli::cli_abort("Transcript object in parameter {.arg t} is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{cli::cli_abort("Parameter {.arg t} needs to be a transcript object.") 	} }
	if (missing(l)) 	{
		l <- methods::new("layout")
		l@docx.template.path <- "" 
	} else {
		if (is.null(l)) 	{
			l <- methods::new("layout")
			l@docx.template.path <- "" 
		}
	}
	
	# Required packages
	if (!requireNamespace("officer", quietly = TRUE)) cli::cli_abort("Please install the {.pkg officer} package.")
	
	# === Define template(s)
	templates <- l@docx.template.path
	templates <- templates[!is.na(templates) & templates != ""]
	if (length(templates) == 0) {
		templates <- system.file("extdata", "docx", "template_transcript.docx", package="act")
		if (!file.exists(templates)) {
			cli::cli_abort("Unable to find {.arg template_transcript.docx}. Please install the act package again.")
		}
	} else {
		missing_templates <- templates[!file.exists(templates)]
		if (length(missing_templates) > 0) {
			cli::cli_abort("Template file(s) not found: {.path {missing_templates}}")
		}
	}

	# === Template suffixes
	template_suffixes <- rep("", length(templates))
	if (length(templates) > 1) {
		base_names <- tools::file_path_sans_ext(basename(templates))
		chars <- strsplit(base_names, "")
		min_len <- min(sapply(chars, length))
		prefix_len <- 0
		for (ci in seq_len(min_len)) {
			if (length(unique(sapply(chars, `[`, ci))) == 1) prefix_len <- ci else break
		}
		for (i in seq_along(base_names)) {
			diff_part <- substr(base_names[i], prefix_len + 1, nchar(base_names[i]))
			diff_part <- gsub("^_+|_+$", "", diff_part)
			if (nchar(diff_part) > 15) diff_part <- paste0("template", formatC(i, width = 2, flag = "0"))
			template_suffixes[i] <- paste0("__", diff_part)
		}
	}

	# === Multi-template loop
	results <- list()
	for (template_idx in seq_along(templates)) {
	inputTemplate <- templates[template_idx]
	
	# === Input validation
	#transcript
	if (missing(t)) 	{cli::cli_abort("ERROR: Transcript object in parameter {.arg t} is missing.") 	}	else { if (!methods::is(t, "transcript")) 	{cli::cli_abort("Parameter {.arg t} needs to be a {.cls transcript} object.") 	} }
	#layout
	if (is.null(l)) 	{
		l <- methods::new("layout")
	}
	#folder output: check if output folder exists
	if (!is.null(pathOutput)) {
		if (!dir.exists(dirname(pathOutput))) {
			cli::cli_abort("Output folder does not exist. Modify parameter {.arg pathOutput}.")
		}
	}
	
	#==== CHECK user inputs ====
	#--- exit if transcript width is too short or too long
	if (l@transcript.width == -1) {
	} else if (l@transcript.width < 40) {
		cli::cli_abort("The width of the transcript is to low. Minimum is 40. Check option {.code l@transcript.width}")
	}
	
	#--- exit if tier length is too short or too long
	if (l@speaker.width == -1) {
	} else if (l@speaker.width==0) {
		cli::cli_abort("Length of tier names is to short. Minimum is 1. Check option {.code l@speaker.width}.")
	} else if (l@speaker.width < -1) {
		cli::cli_abort("Length of tier names is to short. Minimum is 1. Check option {.code l@speaker.width}.")
	} else if (l@speaker.width > 25) {
		cli::cli_abort("Length of tier names is to long. Maximum is 25. Check option {.code l@speaker.width}.")
	}
	
	#--- increase
	options(act.transcript.spacesbefore= max(0, l@spacesbefore))
	
	#==== GET data ====
	#---- . tier names ----
	if (is.null(filterTierNames)) {
		filterTierNames<- t@tiers$name
	}
	filterTierNames <- helper_tiers_filter_create(tierNames=filterTierNames, 
												  filterTierIncludeRegEx=l@filter.tier.includeRegEx,
												  filterTierExcludeRegEx=l@filter.tier.excludeRegEx)
	
	#---- . filter ----
	t <- act::transcripts_filter_single(t, 
										filterTierNames       = filterTierNames, 
										filterSectionStartsec = filterSectionStartsec, 
										filterSectionEndsec   = filterSectionEndsec)
	
	#---- . cure ----
	t <- act::transcripts_cure_single(t,
									  annotationsTimesReversed=TRUE,
									  annotationsOverlap=TRUE,
									  annotationsTimesBelowZero=FALSE,
									  transcriptLengthZero=TRUE,
									  annotationsZeroDuration=TRUE,
									  tiersMissing=FALSE,
									  warning=TRUE)
	
	#---- . annotations  ----
	ann <- t@annotations
	#exit - no annotations: export header only
	if (nrow(ann)==0) {
		label <- export_docx_make_label(t@name, headerTitle, filterSectionStartsec, filterSectionEndsec)
		cli::cli_warn("No annotations found for {.val {label}}. Exporting header only.")
		doc <- officer::read_docx(path = inputTemplate)
		if (l@header.insert==TRUE) {
			if (!is.null(headerPreface)) {
				if (!is.na(headerPreface)) {
					lines <- unlist(stringr::str_split(headerPreface, "\n"))
					style <- get_style_base(l,"header.preface")$docx.template.name
					for (line in lines) { doc <- officer::body_add_par(doc, value = line, style) }
				}
			}
			if (!is.null(headerTitle)) {
				if (!is.na(headerTitle)) {
					lines <- unlist(stringr::str_split(headerTitle, "\n"))
					style <- get_style_base(l,"header.title")$docx.template.name
					for (line in lines) { doc <- officer::body_add_par(doc, value = line, style) }
				}
			}
			if (!is.null(headerSubtitle)) {
				if (!is.na(headerSubtitle)) {
					lines <- unlist(stringr::str_split(headerSubtitle, "\n"))
					style <- get_style_base(l,"header.subtitle")$docx.template.name
					for (line in lines) { doc <- officer::body_add_par(doc, value = line, style) }
				}
			}
			if (!is.null(headerDescription)) {
				if (!is.na(headerDescription)) {
					lines <- unlist(stringr::str_split(headerDescription, "\n"))
					style <- get_style_base(l,"header.description")$docx.template.name
					for (line in lines) { doc <- officer::body_add_par(doc, value = line, style) }
				}
			}
		}
		if (!is.null(pathOutput)) {
			print(x=doc, target = pathOutput)
		}
		return(doc)
	}
	
	#check if all relevant cols are present 
	myCols <- c("tierName", "startsec","endsec","content")
	if (!all(myCols %in% colnames(ann))) {
		cli::cli_abort("Missing columns. Annotations need to contain: {.val {myCols}}")
	}
	
	#add annotationID if not present
	#if ("annotationID" %in% colnames(ann)) {
	#	ann <- ann[, c("annotationID", myCols)]
	#} else {
	#		ann <- cbind(annotationID=NA, ann[,myCols])
	#}
	if (!"annotationID" %in% colnames(ann)) {
		ann$annotationID<-NA
	}
	#convert col tierName to character
	ann$tierName <- as.character(ann$tierName)
	
	#exit - no annotations after filtering
	if (nrow(ann)==0) {
		label <- export_docx_make_label(t@name, headerTitle, filterSectionStartsec, filterSectionEndsec)
		cli::cli_warn("No annotations found for {.val {label}} after filtering.")
		doc <- officer::read_docx(path = inputTemplate)
		if (!is.null(pathOutput)) {
			print(x=doc, target = pathOutput)
		}
		return(doc)
	}

	#sort annotations by start time and tier order
	tier_order <- match(ann$tierName, t@tiers$name)
	ann <- ann[order(ann$startsec, tier_order), ]

	# ===== STYLE MATCHING =====
	ann$format.is.main <- FALSE
	if (!is.null(l@docx.styles.user) && nrow(l@docx.styles.user) > 0) {
		ann$format.style.matched <- FALSE
		for (row_idx in seq_len(nrow(l@docx.styles.user))) {
			style_row <- l@docx.styles.user[row_idx, ]
			matched <- rep(FALSE, nrow(ann))
			if (!is.na(style_row$match.regex)) {
				matched <- matched | grepl(style_row$match.regex, ann$tierName, perl = TRUE)
			}
			unset <- !ann$format.style.matched & matched
			ann$format.style.matched[unset] <- TRUE
			if (!is.null(style_row$is.main.tier) && !is.na(style_row$is.main.tier) && style_row$is.main.tier) {
				ann$format.is.main[unset] <- TRUE
			}
		}
		ann$format.style.matched <- NULL
	}

	# ===== SORT: time + tier order =====
	tier_order <- match(ann$tierName, t@tiers$name)
	tier_order[is.na(tier_order)] <- 999L
	ann <- ann[order(ann$startsec, tier_order), ]

	#==== PREFIXES ====
	#---- . spaces before ----
	ann$spacebefore <- stringr::str_pad("", width=l@spacesbefore, side="left", pad=" ")
	#insert arrow
	if (!is.na(insertArrowAnnotationID)) {
		if (length(which(ann$annotationID==insertArrowAnnotationID))>0) {
			ann$spacebefore[which(ann$annotationID==insertArrowAnnotationID)[1]] <- stringr::str_pad(l@arrow.shape, width=l@spacesbefore, side="right", pad=" ")
		}
	}
	
	#---- . line numbers (placeholder, recalculated after style matching) ----
	ann$line <- ""
	
	# ===== PRE-COMPUTE TIER FORMATTING =====
	style_default_name <- get_style_base(l, "transcript.default")$docx.template.name
	ann$format.show            <- TRUE
	ann$format.style           <- style_default_name
	ann$format.line.nr.show    <- TRUE
	ann$format.acronym.show    <- TRUE
	ann$format.acronym.case    <- NA_character_
	ann$format.acronym.search  <- NA_character_
	ann$format.acronym.replace <- NA_character_
	ann$format.acronym.width   <- 0L
	ann$format.acronym.ending  <- NA_character_
	ann$format.space.after     <- NA_character_
	ann$format.content.wrap    <- TRUE
	ann$format.content.indent.align.filler.inside <- " "
	ann$format.content.indent.align.char <- NA_character_
	ann$format.content.indent.align.mode <- NA_character_

	for (tier_name in unique(ann$tierName)) {
		format_tier <- get_style_user(l, name = tier_name)
		rows <- which(ann$tierName == tier_name)
		if (!is.na(format_tier$show))               ann$format.show[rows]            <- format_tier$show
		if (!is.na(format_tier$line.nr.show))       ann$format.line.nr.show[rows]    <- format_tier$line.nr.show
		if (!is.na(format_tier$acronym.show))       ann$format.acronym.show[rows]    <- format_tier$acronym.show
		if (!is.na(format_tier$docx.template.name)) ann$format.style[rows]           <- format_tier$docx.template.name
		if (!is.na(format_tier$acronym.case))       ann$format.acronym.case[rows]    <- format_tier$acronym.case
		if (!is.na(format_tier$acronym.search))     ann$format.acronym.search[rows]  <- format_tier$acronym.search
		if (!is.na(format_tier$acronym.replace))    ann$format.acronym.replace[rows] <- format_tier$acronym.replace
		if (!is.na(format_tier$acronym.width))      ann$format.acronym.width[rows]   <- format_tier$acronym.width
		if (!is.na(format_tier$acronym.ending))     ann$format.acronym.ending[rows]  <- format_tier$acronym.ending
		if (!is.na(format_tier$space.after))        ann$format.space.after[rows]     <- format_tier$space.after
		if (!is.na(format_tier$content.wrap))       ann$format.content.wrap[rows]    <- format_tier$content.wrap
		if (!is.na(format_tier$content.indent.align.filler.inside)) ann$format.content.indent.align.filler.inside[rows] <- format_tier$content.indent.align.filler.inside
		if (!is.na(format_tier$content.indent.align.char)) ann$format.content.indent.align.char[rows] <- format_tier$content.indent.align.char
		if (!is.na(format_tier$content.indent.align.mode)) ann$format.content.indent.align.mode[rows] <- format_tier$content.indent.align.mode
	}

	#---- . line numbers (only for tiers with line.nr.show = TRUE) ----
	ann$line <- ""
	numbered_rows <- which(ann$format.line.nr.show == TRUE)
	if (length(numbered_rows) > 0) {
		nums <- as.character(seq_along(numbered_rows))
		nums[seq_len(min(length(nums), 9))] <- stringr::str_pad(nums[seq_len(min(length(nums), 9))], width = 2, side = "left", pad = "0")
		ann$line[numbered_rows] <- nums
	}

	#---- . speaker ----
	#get tier names
	tierNames 				<- as.character(unique(ann$tierName))
	
	#width of speakers: global default from layout (without end character)
	text_body_width_speaker_default <- max(nchar(as.character(tierNames)))
	if (!is.na(l@speaker.width)) {
		if (l@speaker.width != -1) {
			text_body_width_speaker_default <- l@speaker.width
		}
	}

	
	#==== TEXT PREP ====
	#---- . gat formatting speaker acronyms ----
	#take the tier name
	ann$speaker <- ann$tierName
	
	#TRP pause 1: temporarily insert Identifier
	trppauses_pos <- stringr::str_detect(ann$content, options()$act.pauseIdentifierGATRegEx)
	if (length(trppauses_pos) > 0) {
		ann$speaker[trppauses_pos] <- "%TRPPAUSE%$(\u00a7($&\u00a7%/%"
	}
	
	#calculate if same speaker as before (based on main tiers only)
	is_main_tier <- ann$format.is.main

	last_main_speaker <- ""
	current_main_speaker <- ""
	ann$speaker_base <- ""
	ann_prev_main_speaker <- character(nrow(ann))
	for (i in seq_len(nrow(ann))) {
		ann_prev_main_speaker[i] <- last_main_speaker
		if (is_main_tier[i]) {
			current_main_speaker <- ann$tierName[i]
			if (trppauses_pos[i]) {
				last_main_speaker <- ""
			} else {
				last_main_speaker <- current_main_speaker
			}
		}
		ann$speaker_base[i] <- current_main_speaker
	}
	sameSpeaker_pos <- tolower(ann_prev_main_speaker) == tolower(ann$speaker_base) & is_main_tier

	#set same speakers as before to ""
	ann$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	
	#TRP pause 2: 
	if (length(trppauses_pos) > 0) {
		#remove temporary insert
		ann$speaker[trppauses_pos] <- ""
		#set to false
		included_speakers_pos[which(trppauses_pos)] <- FALSE
	}
	
	#per-annotation acronym processing
	for (i in which(included_speakers_pos)) {
		speaker_text <- ann$speaker[i]

		acronym_case <- ann$format.acronym.case[i]
		if (!is.na(acronym_case)) {
			speaker_text <- switch(acronym_case,
				"toupper"    = toupper(speaker_text),
				"tolower"    = tolower(speaker_text),
				"capitalize" = paste0(toupper(substr(speaker_text, 1, 1)),
				                      tolower(substr(speaker_text, 2, nchar(speaker_text)))),
				speaker_text
			)
		}

		acronym_search  <- ann$format.acronym.search[i]
		acronym_replace <- ann$format.acronym.replace[i]
		if (!is.na(acronym_search)) {
			speaker_text <- sub(acronym_search,
			                    ifelse(is.na(acronym_replace), "", acronym_replace),
			                    speaker_text)
		} else if (!is.na(l@speaker.regex)) {
			extracted <- stringr::str_extract(speaker_text, l@speaker.regex)
			if (!is.na(extracted)) speaker_text <- extracted
		}

		acronym_width <- ann$format.acronym.width[i]
		if (is.na(acronym_width) || acronym_width == 0) {
			acronym_width <- text_body_width_speaker_default
		}
		if (acronym_width > 0) {
			speaker_text <- substr(speaker_text, 1, acronym_width)
		}

		acronym_ending <- ann$format.acronym.ending[i]
		if (is.na(acronym_ending)) {
			acronym_ending <- l@speaker.ending
		}
		speaker_text <- paste0(speaker_text, acronym_ending)

		ann$speaker[i] <- speaker_text
	}

	#uniform speaker width: maximum of all computed speakers
	text_body_width_speaker <- text_body_width_speaker_default + nchar(l@speaker.ending)
	if (any(included_speakers_pos)) {
		text_body_width_speaker <- max(text_body_width_speaker,
		                               max(nchar(ann$speaker[included_speakers_pos])))
	}

	#pad all speakers to uniform width
	ann$speaker <- stringr::str_pad(ann$speaker, width=text_body_width_speaker, side="right", pad=" ")

	# ===== PRE-RENDER: LINE NUMBER WITH PADDING =====
	line_nr_width <- if (any(ann$line != "")) max(nchar(ann$line[ann$line != ""])) else 2
	ann$format.line.nr <- ifelse(
		ann$format.line.nr.show,
		ann$line,
		strrep(" ", line_nr_width)
	)

	# ===== PRE-RENDER: ACRONYM WITH PADDING =====
	ann$format.acronym <- ifelse(
		ann$format.acronym.show & !sameSpeaker_pos,
		ann$speaker,
		strrep(" ", text_body_width_speaker)
	)

	# ===== PRE-RENDER: PREFIX =====
	ann$format.prefix <- paste0(ann$spacebefore, ann$format.line.nr, " ", ann$format.acronym)

	# ===== PRE-RENDER: BLOCK SPACE (space.after) =====
	ann$format.insert.space.after <- FALSE
	visible_rows <- which(ann$format.show)
	if (length(visible_rows) > 0) {
		for (vi in seq_along(visible_rows)) {
			i <- visible_rows[vi]
			space_after_val <- ann$format.space.after[i]
			if (is.na(space_after_val) || space_after_val == "no") next

			if (space_after_val == "always") {
				ann$format.insert.space.after[i] <- TRUE
			} else if (space_after_val == "block") {
				if (vi == length(visible_rows)) {
					ann$format.insert.space.after[i] <- TRUE
				} else {
					i_next <- visible_rows[vi + 1]
					if (ann$format.is.main[i_next]) {
						ann$format.insert.space.after[i] <- TRUE
					}
				}
			}
		}
	}

	#---- . main variables ----
	text_exdent       <- l@spacesbefore + text_body_width_speaker + 3
	text_body_width   <- l@transcript.width - text_exdent
	text_all          <- c()
	tier_order <- match(ann$tierName, t@tiers$name)
	tier_order[is.na(tier_order)] <- 999L
	ann               <- ann[order(ann$startsec, tier_order), ]
	ann$text          <- ifelse(ann$format.is.main, stringr::str_trim(ann$content), ann$content)
	
	#---- . brackets ----
	if (l@brackets.align == TRUE) {
		ann <- .align_brackets(ann, text_body_width)
	}

	#---- . layer alignment ----
	ann <- .align_layers(ann)
	
	#---- . clean up ----
	#remove unnecessary spaces that make up for an entire line	
	searchString <- paste("^ {", as.character(text_body_width),"}", sep="")
	for (i in 1:length(ann$text)) {
		
		for (j in 1:10){
			ann$text[i] <- stringr::str_replace(ann$text[i], searchString, "")
		}
	}
	#remove line breaks that will lead to an error
	searchString <- "\\r?\n"
	for (i in 1:length(ann$text)) {
		ann$text[i] <- stringr::str_replace_all(ann$text[i], searchString, "")
	}
	
	# ==== OUTPUT ====
	# ---- . docx file from template ----
	doc <- officer::read_docx(path = inputTemplate)
	
	# ---- . header ----
	if (l@header.insert==TRUE) {
		#preface
		if (!is.null(headerPreface)) {
			if (!is.na(headerPreface)) {
				lines <- unlist(stringr::str_split(headerPreface, "\n"))
				style <- get_style_base(l,"header.preface")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#title
		if (!is.null(headerTitle)) {
			if (!is.na(headerTitle)) {
				lines <- unlist(stringr::str_split(headerTitle, "\n"))
				style <- get_style_base(l,"header.title")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#subtitle
		if (!is.null(headerSubtitle)) {
			if (!is.na(headerSubtitle)) {
				lines <- unlist(stringr::str_split(headerSubtitle, "\n"))
				style <- get_style_base(l,"header.subtitle")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		#description
		if (!is.null(headerDescription)) {
			if (!is.na(headerDescription)) {
				lines <- unlist(stringr::str_split(headerDescription, "\n"))
				style <- get_style_base(l,"header.description")$docx.template.name
				for (line in lines) {
					doc <- officer::body_add_par(doc, value = line, style )
				}
			}
		}
		if (headerInsertSource) {
			standardsource <- paste0(
				"(",
				t@name, ", ",
				helper_format_time(min(ann$startsec)), 
				"-", 
				helper_format_time(max(ann$endsec)),
				")")
			doc <- officer::body_add_par(doc, value = standardsource, style = get_style_base(l,"header.subtitle")$docx.template.name)			
		}
	}
	
	space_style_row <- get_style_user(l, name = "space")
	space_style_name <- if (!is.null(space_style_row) && !is.na(space_style_row$docx.template.name)) {
		space_style_row$docx.template.name
	} else {
		style_default_name
	}

	# ===== PRE-RENDER: CONTENT INDENT FOR LAYER ROWS =====
	if (any(!ann$format.is.main)) {
		for (i in which(!ann$format.is.main)) {
			indent_type <- NA_character_
			format_row <- get_style_user(l, name = ann$tierName[i])
			if (!is.null(format_row) && !is.na(format_row$content.indent)) {
				indent_type <- format_row$content.indent
			}
			if (is.na(indent_type)) indent_type <- "content"

			main_row_idx <- which(ann$format.is.main[1:i])
			main_row <- if (length(main_row_idx) > 0) max(main_row_idx) else 1
			main_leading_spaces <- nchar(stringr::str_extract(ann$text[main_row], "^\\s*"))
			if (is.na(main_leading_spaces)) main_leading_spaces <- 0

			if (indent_type == "none") {
				# none: far left, no padding. Build minimal prefix from actually shown elements only.
				none_prefix_parts <- character(0)
				if (ann$format.line.nr.show[i] && ann$line[i] != "") {
					none_prefix_parts <- c(none_prefix_parts, ann$line[i])
				}
				if (ann$format.acronym.show[i] && !sameSpeaker_pos[i]) {
					none_prefix_parts <- c(none_prefix_parts, stringr::str_trim(ann$speaker[i]))
				}
				ann$format.prefix[i] <- if (length(none_prefix_parts) > 0) paste0(paste(none_prefix_parts, collapse = ""), " ") else ""
			} else if (indent_type == "text") {
				main_text <- ann$text[main_row]
				skip_regex <- format_row$content.indent.text.skip
				skip_len <- 0
				if (!is.na(skip_regex)) {
					skip_match <- stringr::str_extract(main_text, skip_regex)
					if (!is.na(skip_match)) skip_len <- nchar(skip_match)
				}
				text_start <- main_leading_spaces + skip_len
				if (text_start > 0) {
					ann$text[i] <- paste0(strrep(" ", text_start), ann$text[i])
				}
			}
			# content: no change needed — prefix already provides correct position
			# align: no change needed — alignment handled separately
		}
	}

	# ---- . body
	for (i in 1:length(ann$text)) {
		if (!ann$format.show[i]) { next }

		if (i==100) {	text_exdent <- text_exdent +1 }
		if (i==1000) { 	text_exdent <- text_exdent +1 }
		if (i==10000) { text_exdent <- text_exdent +1 }

		# ---- . content ----
		turn_initial <- ann$format.prefix[i]
		content_text <- ann$text[i]

		wrap_width <- if (ann$format.content.wrap[i]) text_body_width else Inf

		# Handle return symbol ⏎ — split into segments
		if (grepl("\u23CE", content_text, fixed = TRUE)) {
			segments <- unlist(stringr::str_split(content_text, pattern = "\u23CE"))
			for (seg_idx in seq_along(segments)) {
				seg_prefix <- if (seg_idx == 1) turn_initial else strrep(" ", nchar(turn_initial))
				seg_turn <- stringi::stri_wrap(segments[seg_idx],
											   width           = wrap_width,
											   indent          = 0,
											   exdent          = text_exdent,
											   normalize       = FALSE,
											   initial         = seg_prefix,
											   whitespace_only = TRUE)
				for (line in seg_turn) {
					doc <- officer::body_add_par(doc, value = line, style = ann$format.style[i])
				}
			}
		} else {
			turn <- stringi::stri_wrap(content_text,
									   width           = wrap_width,
									   indent          = 0,
									   exdent          = text_exdent,
									   normalize       = FALSE,
									   initial         = turn_initial,
									   whitespace_only = TRUE)
			for (line in turn) {
				doc <- officer::body_add_par(doc, value = line, style = ann$format.style[i])
			}
		}

		# ---- . space after (pre-computed) ----
		if (ann$format.insert.space.after[i]) {
			doc <- officer::body_add_par(doc, "", style = space_style_name)
		}

	}

	# ==== SAVE ====
	if (!is.null(pathOutput)) {
		output_path <- paste0(tools::file_path_sans_ext(pathOutput), template_suffixes[template_idx], ".docx")
		print(x=doc, target = output_path)
	}

	results[[template_idx]] <- doc
	} # end multi-template loop

	if (length(results) == 1) return(results[[1]])
	return(results)
}
#==== FUNCTONS ====
get_style_base <- function(l, actStyleName) {
	id <- which(l@docx.styles.base$act.style.name==actStyleName)
	if (length(id)==0) {
		cli::cli_abort("Style {.val {actStyleName}} is not defined in your styles file. Add this style to your base styles.")
	} else {
		return (
			l@docx.styles.base[id[1],]
		)
	}
}

get_style_user <- function(l, name) {
	user_df <- l@docx.styles.user

	if (nrow(user_df) > 0 && "match.regex" %in% names(user_df)) {
		match_rows <- which(!is.na(user_df$match.regex))
		for (idx in match_rows) {
			if (stringr::str_detect(name, user_df$match.regex[idx])) {
				return(user_df[idx, , drop = FALSE])
			}
		}
	}
	return(data.frame(
		name              = "default",
		show              = TRUE,
		match.regex       = NA_character_,
		docx.template.name = NA_character_,
		line.nr.show      = TRUE,
		acronym.show      = TRUE,
		acronym.case      = NA_character_,
		acronym.search    = NA_character_,
		acronym.replace   = NA_character_,
		acronym.width     = 0,
		acronym.ending    = NA_character_,
		content.indent    = NA_character_,
		content.indent.text.skip        = NA_character_,
		content.indent.align.char       = NA_character_,
		content.indent.align.filler.inside = " ",
		content.indent.align.mode       = NA_character_,
		content.wrap      = TRUE,
		space.after       = NA_character_,
		comment           = NA_character_,
		stringsAsFactors  = FALSE
	))
}

export_docx_make_label <- function(transcript_name, headerTitle, startSec, endSec) {
	parts <- c()
	if (!is.null(headerTitle) && !is.na(headerTitle)) {
		parts <- c(parts, headerTitle)
	}
	if (!is.null(startSec) && !is.null(endSec)) {
		parts <- c(parts, paste0("[", round(startSec, 1), "s-", round(endSec, 1), "s]"))
	}
	if (length(parts) > 0) {
		paste0(paste(parts, collapse = " "), " / ", transcript_name)
	} else {
		transcript_name
	}
}

.align_brackets <- function(ann, text_body_width) {
	ann$firstBracketIsAligned         <- FALSE
	ann$bracketAlignedWithAnnotationI <- 0
	#i<-6
	#ann$text[i]
	#each annotation
	for (i in 1:length(ann$text)) {

		#check if the first bracket is already aligned
		if (ann$firstBracketIsAligned[i]) {
			startWithBracketI <- 2
		} else {
			startWithBracketI <- 1
		}

		#number of opening brackets
		openingBracketsI.nr 	<- 	stringr::str_count(ann$text[i], "\\[")

		#all brackets in an annotation : check if there are more brackets to be aligned
		if (startWithBracketI<=openingBracketsI.nr) {

			#search correspondence for each opening bracket
			x<-1
			for (x in startWithBracketI:openingBracketsI.nr) {

				#exit if next record set would be outside the data frame
				startSearching <- i+1
				if (startSearching>length(ann$text)) {
					break
				}

				#start: process all annotations FOLLOWING the current annotation i
				j<-startSearching
				for (j in startSearching:length(ann$text)) {

					#exit: if this annotation does not temporally overlap
					if (ann$startsec[j]> ann$endsec[i]) {
						break
					}

					#skip annotation : if it has already been moved/aligned
					if (!ann$firstBracketIsAligned[j]) {
						openingBracketsJ.nr <- 		stringr::str_count(ann$text[j], "\\[")

						#if there are opening brackets that can still be aligned
						if (openingBracketsJ.nr>0 ) {
							#get positions and contents of brackets i
							bracket.i <- data.frame(stringr::str_locate_all(ann$text[i], "\\[.*?\\]"), stringsAsFactors = FALSE)
							bracket.i <- bracket.i[x, ]
							bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(ann$text[i], "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
							bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
							bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(ann$text[i], bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
							bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(ann$text[i], bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)
							bracket.i <- cbind(bracket.i,
											   posSpaceInside=data.frame(
											   	stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"),
											   	stringsAsFactors= FALSE)$start,
											   row.names = NULL)


							#something went wrong, probably missing closing bracket
							if (is.null(bracket.i$bracketLength)) {		break}
							if (is.na(bracket.i$bracketLength)) {		break}


							#get positions and contents of brackets j
							bracket.j <- data.frame(stringr::str_locate_all(ann$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
							bracket.j <- bracket.j[1, ]
							bracket.j <- cbind(bracket.j, content        = as.character(unlist(stringr::str_extract_all(ann$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
							bracket.j <- cbind(bracket.j, bracketLength  = stringr::str_length(bracket.j$content) , row.names = NULL)
							bracket.j <- cbind(bracket.j, before         = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
							bracket.j <- cbind(bracket.j, after          = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
							bracket.j <- cbind(bracket.j, posSpaceInside = data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"), stringsAsFactors= FALSE)$start, row.names = NULL)

							#something went wrong, probably missing closing bracket
							if (is.null(bracket.j$bracketLength)) {		break}
							if (is.na(bracket.j$bracketLength)) {		break}


							#--- put start of annotation j in the right place
							ann$text[j] <- stringr::str_flatten(c(strrep(" ", abs(bracket.i$start-bracket.j$start)), ann$text[j]) , collapse="")
							ann$firstBracketIsAligned[j] <- TRUE

							#get positions and contents of brackets j AGAIN, because overal positino changed
							bracket.j <- data.frame(stringr::str_locate_all(ann$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
							bracket.j <- bracket.j[1, ]
							bracket.j <- cbind(bracket.j, content        = as.character(unlist(stringr::str_extract_all(ann$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
							bracket.j <- cbind(bracket.j, bracketLength  = stringr::str_length(bracket.j$content) , row.names = NULL)
							bracket.j <- cbind(bracket.j, before         = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
							bracket.j <- cbind(bracket.j, after          = stringr::str_trim(stringr::str_sub(ann$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
							bracket.j <- cbind(bracket.j, posSpaceInside = data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"), stringsAsFactors= FALSE)$start, row.names = NULL)

							#something went wrong, probably missing closing bracket
							if (is.null(bracket.j$bracketLength)) {		break}
							if (is.na(bracket.j$bracketLength)) {		break}

							#---- adjust length of brackets by inserting spaces inside the brackets
							#check which is the longer bracket
							difference <- bracket.j$bracketLength - bracket.i$bracketLength

							if (difference>0) {
								#modify i
								difference <- abs(difference)
								insert.char <- .detect_bracket_filler(bracket.i$content, ann$format.content.indent.align.filler.inside[i])
								if (bracket.i$after=="" | stringr::str_detect(bracket.i$after,"\\W")) {
									#if content ends after ] or a non-word character follows --> space immediately before ]
									insert.pos <- bracket.i$end-1
								} else if (!is.na(bracket.i$posSpaceInside)) {
									#if there is a space inside the bracket --> insert spaces there
									insert.pos <- bracket.i$start + bracket.i$posSpaceInside-1
								} else if (bracket.i$before=="") {
									#	if blanks or annotation starts before [ -> insert spaces after the opening bracket
									insert.pos <- bracket.i$start
								} else {
									#insert underscores before ]
									insert.pos <- bracket.i$end-1
									insert.char <- "_"
								}
								ann$text[i] <- stringr::str_c(stringr::str_sub(ann$text[i], start=1, end=insert.pos),
															  strrep(insert.char, difference),
															  stringr::str_sub(ann$text[i], start=insert.pos+1, end=stringr::str_length(ann$text[i])),
															  sep="",
															  collapse="")

							} else if (difference<0) {
								difference <- abs(difference)

								#modify j
								insert.char <- .detect_bracket_filler(bracket.j$content, ann$format.content.indent.align.filler.inside[j])
								if (bracket.j$after=="" | stringr::str_detect(bracket.j$after,"\\W")) {
									#if content ends after ] or a non-word charater follows --> space immediately before closing ]
									insert.pos <- bracket.j$end-1
								} else if (!is.na(bracket.j$posSpaceInside)) {
									#if there is a space inside the bracket --> insert spaces there
									insert.pos <- bracket.j$start + bracket.j$posSpaceInside -1
								} else if (bracket.j$before=="") {
									#if blanks or annotation starts before [ -> insert spaces after the opening bracket
									insert.pos <- bracket.j$start
								} else {
									#insert underscores before ]
									insert.pos <- bracket.i$end-1
									insert.char <- "_"
								}
								ann$text[j] <- stringr::str_c(stringr::str_sub( ann$text[j], start=1, end=insert.pos),
															  strrep(insert.char, difference),
															  stringr::str_sub(ann$text[j], insert.pos+1,  end=stringr::str_length(ann$text[j])), sep="",
															  collapse="")
							}

							#### - the following section is where things go wrong
							# 1) after the wrapping of i, every following annotation should be again pre processed
							# or
							# 2) the wrapping should be done manually by inserting spaces based on where the break was necessary

							wrap_width_i <- if (ann$format.content.wrap[i]) text_body_width else Inf
							text_i_wrapped <- ann$text[i]
							text_i_wrapped <-  stringi::stri_wrap(text_i_wrapped, width = wrap_width_i, indent=0, normalize=FALSE, whitespace_only=TRUE)

							#add spaces in the end to fill to full width of text
							text_i_wrapped <- stringr::str_pad(string=text_i_wrapped, width=wrap_width_i, side="right")

							#joint text to long string
							text_i_wrapped <- stringr::str_flatten(text_i_wrapped, collapse="")

							#calculate positions of brackets again
							bracket.i <- as.data.frame(stringr::str_locate_all(text_i_wrapped, "\\[.*?\\]"), stringsAsFactors = FALSE)
							bracket.i <- bracket.i[x, ]
							bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(text_i_wrapped, "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
							bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
							bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
							bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)
							bracket.i <- cbind(bracket.i, posSpaceInside=data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"), stringsAsFactors		= FALSE)$start, row.names = NULL)

						} #there are opening brackets in j
					} #annotation has already been aligned
				} # end: all annotations following i
			} #end: all brackets in an annotation
		} #all brackets in an annotation
	} #next annotation
	return(ann)
}

.align_layers <- function(ann) {
	align_rows <- which(!is.na(ann$format.content.indent.align.mode) & !ann$format.is.main)
	if (length(align_rows) == 0) return(ann)

	for (i in align_rows) {
		main_candidates <- which(ann$format.is.main[1:i])
		if (length(main_candidates) == 0) next
		main_idx <- max(main_candidates)

		main_text <- ann$text[main_idx]
		layer_text <- ann$text[i]
		align_chars <- ann$format.content.indent.align.char[i]
		align_mode <- ann$format.content.indent.align.mode[i]
		filler_inside <- ann$format.content.indent.align.filler.inside[i]

		if (is.na(align_chars) || is.na(main_text) || is.na(layer_text)) next
		if (nchar(align_chars) == 0) next

		align_char_vec <- strsplit(align_chars, "")[[1]]
		main_positions <- .find_symbol_positions(main_text, align_char_vec)
		layer_positions <- .find_symbol_positions(layer_text, align_char_vec)

		if (length(main_positions) == 0 || length(layer_positions) == 0) next

		if (align_mode == "point") {
			ann$text[i] <- .align_symbols_point(layer_text, layer_positions, main_positions, align_char_vec)
		} else if (align_mode == "bracket") {
			ann$text[i] <- .align_symbols_bracket(main_text, layer_text, layer_positions, main_positions, filler_inside, align_char_vec)
		}
	}
	return(ann)
}

.find_symbol_positions <- function(text, symbols) {
	positions <- integer(0)
	for (pos in seq_len(nchar(text))) {
		if (substr(text, pos, pos) %in% symbols) {
			positions <- c(positions, pos)
		}
	}
	positions
}

.align_symbols_point <- function(layer_text, layer_positions, main_positions, align_char_vec) {
	n_match <- min(length(layer_positions), length(main_positions))
	if (n_match == 0) return(layer_text)

	offset <- 0L
	for (sym_i in seq_len(n_match)) {
		main_pos <- main_positions[sym_i]
		layer_pos <- layer_positions[sym_i] + offset

		if (main_pos > layer_pos) {
			spaces_needed <- main_pos - layer_pos
			arrow_result <- .expand_arrow(layer_text, spaces_needed, align_char_vec)
			if (!is.null(arrow_result)) {
				layer_text <- arrow_result
				break
			}
			insert_at <- layer_positions[sym_i] + offset - 1L
			layer_text <- paste0(
				substr(layer_text, 1, insert_at),
				strrep(" ", spaces_needed),
				substr(layer_text, insert_at + 1L, nchar(layer_text))
			)
			offset <- offset + spaces_needed
		}
	}
	layer_text
}

.align_symbols_bracket <- function(main_text, layer_text, layer_positions, main_positions, filler_inside, align_char_vec) {
	n_match <- min(length(layer_positions), length(main_positions))
	if (n_match == 0) return(layer_text)

	main_types <- .classify_bracket_symbols(main_text, main_positions)
	layer_types <- .classify_bracket_symbols(layer_text, layer_positions)

	offset <- 0L
	inside_bracket <- FALSE
	bracket_open_pos <- NA_integer_
	for (sym_i in seq_len(n_match)) {
		main_pos <- main_positions[sym_i]
		layer_pos <- layer_positions[sym_i] + offset
		main_type <- main_types[sym_i]
		layer_type <- if (sym_i <= length(layer_types)) layer_types[sym_i] else "opening"

		if (main_type == "opening") {
			inside_bracket <- TRUE
			bracket_open_pos <- layer_pos
		}

		if (main_pos > layer_pos) {
			spaces_needed <- main_pos - layer_pos

			arrow_result <- .expand_arrow(layer_text, spaces_needed, align_char_vec)
			if (!is.null(arrow_result)) {
				layer_text <- arrow_result
				break
			}

			insert_at <- layer_positions[sym_i] + offset - 1L

			if (inside_bracket && main_type != "opening") {
				bracket_content <- substr(layer_text, bracket_open_pos, layer_pos)
				fill_char <- .detect_bracket_filler(bracket_content, filler_inside)
			} else {
				fill_char <- " "
			}

			layer_text <- paste0(
				substr(layer_text, 1, insert_at),
				strrep(fill_char, spaces_needed),
				substr(layer_text, insert_at + 1L, nchar(layer_text))
			)
			offset <- offset + spaces_needed
		}

		if (main_type == "closing") {
			inside_bracket <- FALSE
			bracket_open_pos <- NA_integer_
		}
	}
	layer_text
}

.classify_bracket_symbols <- function(text, positions) {
	n <- length(positions)
	if (n == 0) return(character(0))

	types <- character(n)
	text_len <- nchar(text)

	for (idx in seq_len(n)) {
		pos <- positions[idx]
		char_before <- if (pos > 1) substr(text, pos - 1, pos - 1) else ""
		char_after <- if (pos < text_len) substr(text, pos + 1, pos + 1) else ""

		before_is_space <- (char_before == "" || char_before == " ")
		after_is_space <- (char_after == "" || char_after == " ")

		if (before_is_space && !after_is_space) {
			types[idx] <- "opening"
		} else if (!before_is_space && after_is_space) {
			types[idx] <- "closing"
		} else if (!before_is_space && !after_is_space) {
			types[idx] <- "closing"
		} else {
			types[idx] <- "opening"
		}
	}
	types
}

.detect_bracket_filler <- function(bracket_content, default_filler) {
	if (is.na(bracket_content) || nchar(bracket_content) < 3) return(default_filler)
	inner <- substr(bracket_content, 2, nchar(bracket_content) - 1)
	if (nchar(inner) == 0) return(default_filler)
	opening_char <- substr(bracket_content, 1, 1)
	closing_char <- substr(bracket_content, nchar(bracket_content), nchar(bracket_content))
	boundary_chars <- unique(c(opening_char, closing_char))
	trailing_match <- stringr::str_extract(inner, "(.)\\1+$")
	if (is.na(trailing_match)) return(default_filler)
	candidate <- substr(trailing_match, 1, 1)
	if (candidate %in% boundary_chars) return(default_filler)
	candidate
}

.expand_arrow <- function(layer_text, spaces_needed, symbols) {
	trimmed <- trimws(layer_text)
	arrow_match <- stringr::str_match(trimmed, "^([-=]*)(>+)(.)$")
	if (is.na(arrow_match[1, 1])) return(NULL)
	last_char <- arrow_match[1, 4]
	if (!last_char %in% symbols) return(NULL)
	stem_char <- arrow_match[1, 2]
	arrows <- arrow_match[1, 3]
	fill <- if (nchar(stem_char) > 0) substr(stem_char, 1, 1) else "-"
	leading_spaces <- nchar(layer_text) - nchar(trimws(layer_text, which = "left"))
	paste0(
		strrep(" ", leading_spaces),
		strrep(fill, nchar(stem_char) + spaces_needed),
		arrows,
		last_char
	)
}
