#' Create cut lists for 'FFmpeg' 
#' 
#' This function creates FFmpeg commands to cut media files for each search results.
#' If you want to execute the commands (and cut the media files) you need to have FFmpeg installed on you computer. To install FFmpeg you can follow the instructions given in the vignette 'installation-ffmpeg'. Show the vignette with \code{vignette("installation-ffmpeg")}.
#' 
#' \emph{Cut lists}\cr
#' The commands are collected in cut lists. 
#' The cut lists will be stored in different ways:
#' * A cut list for for ALL search results will be stored in \code{s@cuts.cutlist.mac} to be used on MacOS and \code{s@cuts.cutlist.win} to be used on Windows.
#' * Individual cut lists for EACH search result will be stored in additional columns in the data frame \code{s@results}.
#' The cut lists that can be executed in the Terminal (Apple) or the Command Line Interface (Windows). 
#'  
#' \emph{Input media files}\cr
#' The function will use all files in  \code{corpus@transcripts[[ ]]@media}.
#' Therefore you will need to set the options \code{filterMediaInclude} filtering for which input media files you want to create the cuts.
#' The filter is a regular expression, e.g.  \code{'\\.(wav|aif)'} for '.wav' and '.aif' audio files or \code{'\\.mp4'} for '.mp4' video files.
#'
#' \emph{Output file names}\cr
#' The file names of the cuts are taken from the \code{resultID} column in the search results table. To change this, modify the contents of this column or define a different column using \code{options(act.export.filename.fromColumnName}. 
#' By default, the device descriptor of the original media will be appended to the file name. It is taken from the \code{devices} column of \code{corpus@transcripts[[ ]]@media}. To change this, modify that column.
#' 
#' \emph{Output format}\cr
#' The output format is predefined by in the options:
#' * Video cuts are built by \link{helper_ffmpeg_args_clip}: picture and sound start at the time ELAN shows (edit list alignment), H.264 yuv420p with the options \code{act.ffmpeg.video.bitrate}, \code{.crf}, \code{.keyframe_interval}, \code{.max_height}, \code{.max_width} and AAC with \code{act.ffmpeg.audio.bitrate}.
#' * Audio cuts are built by \link{helper_ffmpeg_args_audio}: copied without filter, otherwise written with the bit depth of the source; mp3 with \code{act.ffmpeg.audio.bitrate}.
#' * The channel versions (\code{_ch1}, \code{_ch2}) are stereo files with the left resp. right channel on both sides; the filters come from \link{helper_audio_filter_parts}.
#' * Stills and thumbnails are built by \link{helper_ffmpeg_args} (exact seek, JPG quality from \code{act.ffmpeg.image.quality}; thumbnails at most \code{act.ffmpeg.thumbnail.max_height} pixels high); each still gets the act.* EXIF comment through an exiftool line in the cut list (skipped when exiftool is not installed).
#'  
#'  
#' \emph{Extract stills}\cr
#' To extract stills the time information and the file names need to be stored in the search object in the results data frame, e.g. in \code{s@results}.
#' The information needs to be contained in a vector.
#' The elements of the vector are the time values in seconds, the name attribute will be used as file name, e.g. \code{myStills <- c(still1=1.0, still2=2.0)}.
#' The vector needs stored for each search result in the column called \code{stills.values} as a list, e.g code \code{s@results$stills.values[[1]] <- myStills}.
#' Stills will be stored in a sub folder called \code{stills} by default, if not otherwise defined in the column \code{stills.folder}. 
#' Please see the section \code{options_show} to customize the behavior of the function.
#'  
#'  
#' \emph{Cut list lines}\cr
#' The lines of the cut lists are written by \link{helper_cutlist_lines}. They call ffmpeg as set in the option \code{act.cutlist.ffmpeg} (default \code{"ffmpeg"}, found via the PATH of the machine that runs the cut list).
#'   
#' @md
#' 
#' @param x Corpus object; Please note: all media paths for a transcript need to be given as a list in the corpus object in \code{corpus@transcripts[[ ]]@media} . You can use the respective media functions. . 
#' @param s Search object.
#' @param exportMedia Logical; If \code{TRUE} media cuts (audio/video)  will be included in the cut list.
#' @param exportStills Logical; If \code{TRUE} stills (image) will be included in the cut list. For creating the list of stills to be exported see \link{search_cuts_media}.
#' @param exportThumbnail Logical; If \code{TRUE} a thumbnail image (.jpg, at most \code{act.ffmpeg.thumbnail.max_height} pixels high) will be exported to the main folder. The name will be the same as the media cut. By default the time value is the start of the search hit. To change this set time values in seconds in a column called \code{thumbnails} in \code{s@results}. 
#' @param cutSpanBeforesec Double; Start the media cut some seconds before the hit to include some context; the default \code{NULL} will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the media cut some seconds before the hit to include some context; the default \code{NULL} will take the value as set in @cuts.span.beforesec of the search object.
#' @param folderOutput Character string; path to folder where files will be written.
#' @param filterMediaInclude Character string; regular expression to match only some of the media files in \code{corpus@transcripts[[ ]]@media}.
#' @param filterMediaExclude Character string; regular expression to exclude some of the media files in \code{corpus@transcripts[[ ]]@media}. Empty string (default) excludes none.
#' @param videoFastPositioning Logical; no longer used. Video cuts, stills and thumbnails always seek exactly (two-stage seek, see \link{helper_ffmpeg_args}). Kept for compatibility.
#' @param videoCodecCopy Logical; If \code{TRUE} the video is copied instead of encoded (fast mode, e.g. for many search hits): a copy can only start at a keyframe, so each video cut starts at the keyframe before the cut (up to the keyframe distance of the camera earlier, often a few seconds) and keeps the full resolution; sound and picture stay in sync, the act.* metadata carry the real start. See \link{helper_ffmpeg_args_clip} and \link{helper_ffmpeg_keyframe}.
#' @param audioAsMP3 Logical; If \code{TRUE} audio cuts will be converted to .mp3 files (bit rate \code{act.ffmpeg.audio.bitrate}). If \code{FALSE} (default) the audio format is preserved.
#' @param audioPanning Integer; 0=leave audio as is (ch1&ch2) , 1=only channel 1 (ch1), 2=only channel 2 (ch2), 3=both channels separated (ch1&ch2), 4=all three versions (ch1&ch2, ch1, ch2). This setting will override the option made in 'act.ffmpeg.channels_from_column' .
#' @param audioNormalize Logical; If \code{TRUE} and option \code{act.ffmpeg.audio.loudnorm} is set, a loudnorm filter is appended to the audio filter chain. Default is \code{FALSE}.
#' @param outputOS Vector of character Strings; Saves FFMpeg cut list in format for \code{"win"}=windows, \code{"mac"}=apple ox/linux.
#' @param outputFileName Character String; Name of the cut list.
#' 
#' @return Search object; cut lists will be stored in \code{s@cuts.cutlist.mac} and \code{s@cuts.cutlist.win}.
#'
#' @seealso \link{search_cuts_media}, \code{vignette("install_ffmpeg", package = "act")}
#' 
#' @export
#'
#' @example inst/examples/search_cuts_media.R
#' 
search_cuts_media <- function(x,
							  s,
							  exportMedia          = TRUE,
							  exportStills         = TRUE,
							  exportThumbnail      = TRUE,
							  cutSpanBeforesec     = NULL,
							  cutSpanAftersec      = NULL,
							  folderOutput         = NULL,
							  filterMediaInclude   = "",
							  filterMediaExclude   = "",
							  videoFastPositioning = TRUE,
							  videoCodecCopy       = FALSE,
							  audioAsMP3           = FALSE,
							  audioPanning         = NULL,
							  audioNormalize       = FALSE,
							  outputOS             = c("mac", "win"),
							  outputFileName       = "FFMPEG_cutlist")
{
	if (1==2) {
#		x                    <- corpus
#		s                    <- s
#		exportMedia          <- TRUE
#		exportStills         <- TRUE
#		exportThumbnail      <- TRUE
#		cutSpanBeforesec     <- NULL
#		cutSpanAftersec      <- NULL
#		folderOutput         <- ""
#		filterMediaInclude   <- ""
#		videoFastPositioning <- TRUE
#		videoCodecCopy       <- FALSE
#		audioAsMP3           <- FALSE
#		audioPanning         <- NULL
#		audioNormalize       <- FALSE
#		exportStills         <- TRUE
	}
	
	#==== CHECKS ====
	.assert_corpus(x, missing = missing(x))
	.assert_search(s, missing = missing(s))
	if (is.null(s@results$transcriptName))       	{ cli::cli_abort("Data frame s@results does not contain column {.arg transcriptName}.") 	}
	if (nrow(s@results)==0) 	                    { cli::cli_abort("Data frame s@results does not contain any search results (data frame with 0 rows)") 	}
	if (!is.null(folderOutput)) {
		if (!options()$act.export.filename.fromColumnName %in% colnames(s@results)) {
			cli::cli_abort("The column defined in the option 'options()$act.export.filename.fromColumnName' does not exist in the data.frame with the search results.")
		}
	}
	if (!is.null(cutSpanBeforesec)) 	{
		if (length(cutSpanBeforesec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanBeforesec)) {
			cli::cli_abort("Parameter {.arg cutSpanBeforesec} needs to be a numeric value.")
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	if (!is.null(cutSpanAftersec)) 	{
		if (length(cutSpanAftersec)!=1) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to contain only one element as a numeric value.")
		}
		if (!is.numeric(cutSpanAftersec)) {
			cli::cli_abort("Parameter {.arg cutSpanAftersec} needs to be a numeric value.")
		}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}
	
	#stills.values:	check if column exists
	if (exportStills) {
		if (!"stills.values" %in% colnames(s@results)) {
			cli::cli_abort("The results data frame of your search object does not contain a column called {.val stills.values}.")
		}
	} 
	
	#==== VARIABLES ====
	myWarnings <- c()
	#---- . out folder ----
	#if cut list should be saved : set output folder
	if (is.null(folderOutput)) {
		out_folder_main <- "."
	} else {
		out_folder_main <- normalizePath(folderOutput, winslash = "/", mustWork = FALSE)
	}
	
	#---- . total cut lists ----
	mac_cutlist <- c()
	win_cutlist <- c()
	
	#---- . file names ---- 
	#add name of result and file type
	#name of sequence : sub folder for each search result
	filename.fromColumnName <- options()$act.export.filename.fromColumnName
	if(!filename.fromColumnName %in% colnames(s@results)) {
		filename.fromColumnName <- "resultID" 				
	}
	
	#==== MAIN LOOP: each search results ====
	#set progress bar
	helper_progress_set("Creating cutlist", max(1,nrow(s@results)))
	#initialize hiere (otherwise there will be an error if no media files at all are found)
	win_video <- c()
	mac_video <- c()

	fps_cache <- list()

	res<-1
	for (res in 1:nrow(s@results)) 	{
		#update progress bar
		helper_progress_tick()
		
		#---- . variables ----
		mac_cutlist <- c(mac_cutlist, paste0("#====  ", as.character(s@results[res, filename.fromColumnName])))
		cmd_titletext   <- stringr::str_flatten(c(s@results[res, filename.fromColumnName], " (", res, " of ", nrow(s@results)," search results)"), collapse="")
		
		#---- . folder ----
		#from main folder
		out_folder_current <- out_folder_main
		if (s@name!="") 	{ out_folder_current <- file.path(out_folder_current, s@name)	}
		
		#via folder grouping 1: default=by resultID
		foldergrouping.fromColumnName <- options()$act.export.folder.grouping1.fromColumnName
		if (foldergrouping.fromColumnName!="") {
			if(foldergrouping.fromColumnName %in% colnames(s@results)) {
			} else {
				foldergrouping.fromColumnName <- "resultID"
			}
			foldername <- as.character(s@results[res, foldergrouping.fromColumnName])
			if(!is.na(foldername)) {
				if(!length(foldername)==0) {
					if (foldername!="") {
						out_folder_current <- file.path(out_folder_current, foldername)
					}
				}
			}
		}
		
		#---- . paths media ----
		t <- NULL
		if (is.null(s@results$transcriptName[res])) {
			#transcript not found
			myWarnings <- c(myWarnings, sprintf("result %s '%s': transcript '%s' not found in corpus", res, as.character(s@results[res, options()$act.export.filename.fromColumnName]), as.character(s@results$transcriptName[res])))
		} else {
			t <- x@transcripts[[ s@results$transcriptName[res] ]]
			if (is.null(t)) {
				#transcript not found
				myWarnings <- c(myWarnings, sprintf("result %s '%s': transcript '%s' not found in corpus", res, as.character(s@results[res, options()$act.export.filename.fromColumnName]), as.character(s@results$transcriptName[res])))
			}
		}
		if (is.null(t)) {
			next
		}
		
		#==== SUB LOOP: each media file ====
		#get paths
		in_paths <- t@media$path
		#devices descriptor used as filename suffix; fall back to base name when absent
		if ("devices" %in% colnames(t@media)) {
			in_descriptors <- t@media$devices
		} else {
			in_descriptors <- stringr::str_to_lower(tools::file_path_sans_ext(basename(in_paths)))
		}
		if (!filterMediaInclude=="") {
			keep           <- grep(pattern=filterMediaInclude, in_paths)
			in_paths       <- in_paths[keep]
			in_descriptors <- in_descriptors[keep]
		}
		if (!filterMediaExclude=="") {
			keep           <- !grepl(pattern=filterMediaExclude, in_paths)
			in_paths       <- in_paths[keep]
			in_descriptors <- in_descriptors[keep]
		}

		#if no media path found: default name
		if (length(in_paths)==0) {
			in_paths       <- "DEFAULT_MEDIA_PATH"
			in_descriptors <- ""
		}
		j<-1
		for (j in 1:length(in_paths)) {
			#skip if path is NA
			if (is.na(in_paths[j])) {next}
			
			mac_cutlist <- c(mac_cutlist, paste0("#----  ", basename(in_paths[j])))
			
			#suffix of input media file
			in_suffix <- stringr::str_to_lower(tools::file_ext(in_paths[j]))
	
			if (in_suffix =="") {
				in_suffix <- "mp4"
			}
			
			#==== INFILEPATH ====
			mac_infilepath <- sprintf('PATH_INPUT="%s"', in_paths[j])
			mac_cutlist <- c(mac_cutlist, mac_infilepath)
			win_infilepath <- sprintf('SET "PATH_INPUT=%s"', in_paths[j])
			win_cutlist <- c(win_cutlist, win_infilepath)
			
			#==== MEDIA ====
			if (exportMedia) {
				#reset individual lists
				win_video <- c()
				mac_video <- c("#.... video")
				out_folder_video <- rep(out_folder_current, 3)
				
				#panned versions: group in folders by channels
				#if  CreatePannedVersions in the arguments if the functions is not set
				CreatePannedVersions <- 0
				if (!is.null(audioPanning)) {
					CreatePannedVersions <- audioPanning
				} else {
					#check if channels are set in the search results
					if(options()$act.ffmpeg.channels_from_column %in% colnames(s@results)) {
						#if it is set, take the value given there
						CreatePannedVersions <- s@results[res, options()$act.ffmpeg.channels_from_column]
						if (is.na(CreatePannedVersions)) {
							CreatePannedVersions <-0
						}
					}
				}
				
				if (CreatePannedVersions==0 ) {			#no panning
					#
				} else if (CreatePannedVersions==1 ) {	#only left
					#
				} else if (CreatePannedVersions==2 ) {	#only right
					#
				} else if (CreatePannedVersions==3)  {	#left and right
					out_folder_video[2] <- file.path(out_folder_video[2], "ch1")
					out_folder_video[3] <- file.path(out_folder_video[3], "ch2")
				} else if (CreatePannedVersions==4 ) {	#all three versions
					out_folder_video[1] <- file.path(out_folder_video[1], "ch1_ch2")
					out_folder_video[2] <- file.path(out_folder_video[2], "ch1")
					out_folder_video[3] <- file.path(out_folder_video[3], "ch2")
				}
				
				#---- . folder grouping 2 ----
				#subfolder for each cut
				foldergrouping.fromColumnName <- options()$act.export.folder.grouping2.fromColumnName
				if (foldergrouping.fromColumnName!="") {
					if(foldergrouping.fromColumnName %in% colnames(s@results)) {
						foldername <- as.character(s@results[res, foldergrouping.fromColumnName])
						if(!is.na(foldername)) {
							if(!length(foldername)==0) {
								if (foldername!="") {
									out_folder_video <- file.path(out_folder_video, foldername)
								}
							}
						}
					} 
				}
				
				#---- . directory commands ----
				out_folder_video_all <- c()
				if (CreatePannedVersions==0) { out_folder_video_all <- c(out_folder_video_all, out_folder_video[1])	  }
				if (CreatePannedVersions==1) { out_folder_video_all <- c(out_folder_video_all, out_folder_video[2])	  }
				if (CreatePannedVersions==2) { out_folder_video_all <- c(out_folder_video_all, out_folder_video[3])	  }
				if (CreatePannedVersions==3) { out_folder_video_all <- c(out_folder_video_all, out_folder_video[2:3]) }
				if (CreatePannedVersions==4) { out_folder_video_all <- c(out_folder_video_all, out_folder_video[1:3]) }
				
				out_folder_video_all <- unique(out_folder_video_all)
				if (length(out_folder_video_all)>0) {
					win_makedir  <- stringr::str_flatten(sprintf('IF NOT EXIST "%s" ( md "%s" )', out_folder_video_all, out_folder_video_all), collapse='\n')
					win_video <- c(win_video, win_makedir)
					mac_makedir  <- stringr::str_flatten(sprintf('mkdir -p "%s"', out_folder_video_all), collapse='\n')
					mac_video <- c(mac_video, mac_makedir)
				}
				
				#get format / suffix of input media file
				out_suffix <- in_suffix
				
				#media type
				is_audio_file <- identical(.media_type_of_ext(in_suffix), "audio")
				is_video_file <- !is_audio_file

				#if it is an audio file and should be converted to mp3
				if (is_audio_file & audioAsMP3) {
					#replace destination file extension with mp3
					out_suffix <- "mp3"
				}
				if (is_audio_file && !tolower(out_suffix) %in% c("wav", "aif", "aiff", "mp3", "m4a")) out_suffix <- "wav"
				if (is_video_file && !tolower(out_suffix) %in% c("mp4", "mov", "m4v")) out_suffix <- "mp4"
				
				#---- . out paths ----
				out_filePath <- rep("",3)
				filename <- as.character(s@results[res, filename.fromColumnName])
				#append media name to file name if there is more than one media file
				if(length(in_paths)>1) {
					if (!is.na(in_descriptors[j]) && in_descriptors[j]!="") {filename = paste0(filename, "__", in_descriptors[j])}
				}
				
				#replace everything that is not allowed in file names
				filename <- stringr::str_replace_all(filename, "[^\\p{L}\\p{N}_\\.-]", "_")
				
				out_filePath[1] <- file.path(out_folder_video[1], paste(filename, ".",     out_suffix, sep=""))
				out_filePath[2] <- file.path(out_folder_video[2], paste(filename, "_ch1.", out_suffix, sep=""))
				out_filePath[3] <- file.path(out_folder_video[3], paste(filename, "_ch2.", out_suffix, sep=""))
				
				#---- . commands ----
				startsec 	<- max(0, s@results$startsec[res] - s@cuts.span.beforesec)
				endsec 		<- min(s@results$endsec[res] + s@cuts.span.aftersec, t@length.sec)
				duration <- endsec - startsec

				fps_for_meta <- NULL
				if (is_video_file) {
					if (is.null(fps_cache[[in_paths[j]]])) {
						fps_cache[[in_paths[j]]] <- .metadata_probe_fps(in_paths[j])
					}
					fps_for_meta <- fps_cache[[in_paths[j]]]
				}
				meta_start <- if (is_video_file && isTRUE(videoCodecCopy))
					helper_ffmpeg_keyframe(in_paths[j], startsec) else startsec
				meta_argv <- helper_metadata_ffmpeg_argv(
					sourcePath = in_paths[j],
					startsec   = meta_start,
					clipID     = filename,
					fps        = fps_for_meta
				)

				channel_af <- lapply(c("stereo", "left", "right"), function(ch) {
					helper_audio_filter_parts(channel = ch, mono = FALSE, normalize = isTRUE(audioNormalize),
						ranges = NULL, mode = "beep", strength = 0.8, distort = NULL)$af
				})
				cmd_args <- lapply(1:3, function(v) {
					af <- channel_af[[v]]
					if (is_audio_file) {
						helper_ffmpeg_args_audio(
							output        = out_filePath[v],
							input         = in_paths[j],
							startsec      = startsec,
							duration      = duration,
							audioFilter   = af,
							filterComplex = NULL,
							audioBitrate  = getOption("act.ffmpeg.audio.bitrate", "192k"),
							metadataArgs  = meta_argv)
					} else {
						helper_ffmpeg_args_clip(
							output           = out_filePath[v],
							input            = in_paths[j],
							startsec         = startsec,
							duration         = duration,
							audioFilter      = if (length(af)) paste(af, collapse = ",") else NULL,
							maxHeight        = getOption("act.ffmpeg.video.max_height"),
							maxWidth         = getOption("act.ffmpeg.video.max_width"),
							videoBitrate     = getOption("act.ffmpeg.video.bitrate", "8M"),
							videoCrf         = getOption("act.ffmpeg.video.crf"),
							keyframeInterval = getOption("act.ffmpeg.video.keyframe_interval", 25),
							metadataArgs     = meta_argv,
							videoCopy        = isTRUE(videoCodecCopy))
					}
				})
				cmd_lines <- function(os) vapply(cmd_args, function(a)
					helper_cutlist_lines(list(list(type = "ffmpeg", args = a)), os = os,
					                     inputVariable = in_paths[j]), character(1))
				mac_cmd <- cmd_lines("mac")
				win_cmd <- stringi::stri_replace_all_fixed(cmd_lines("win"), "/", "\u0001")

				#---- . command compile ----
				#compile
				win_video <- c(win_video, makeVideoBlock("win", CreatePannedVersions, in_filePath=in_paths[j],  out_filename=as.character(s@results[res, filename.fromColumnName]), win_cmd, cmd_titletext))
				win_video <- stringr::str_flatten(win_video)
				win_video <- stringr::str_replace_all(win_video, "/", "\\\\")
				win_video <- stringi::stri_replace_all_fixed(win_video, "\u0001", "/")
				
				mac_video <- c(mac_video, makeVideoBlock("mac", CreatePannedVersions, in_filePath=in_paths[j],  out_filename=as.character(s@results[res, filename.fromColumnName]), mac_cmd, cmd_titletext))
				
				#---- . command add ----
				#to search
				s@results[res, "cuts.cutlist.mac"] <- stringr::str_flatten(mac_video, collapse="\n")
				s@results[res, "cuts.cutlist.win"] <- stringr::str_flatten(win_video, collapse="\n")
				#to total cutlist
				
				mac_cutlist <- c(mac_cutlist, mac_video)
				win_cutlist <- c(win_cutlist, win_video)
			}
			
			#===== THUMBNAIL  ====
			#extract thumbs (only if it is a video file)
			if (exportThumbnail & identical(.media_type_of_ext(in_suffix), "video")) 	{
				#check if time is set or default time
				time <- NA
				if("thumbnails" %in% colnames(s@results))  {
					#skip if col is present but current value is NA
					if (!is.na(s@results$thumbnails[res])) {
						if (!as.character(s@results$thumbnails[res])=="") {
							time<-as.double(s@results$thumbnails[res])
						}
					}
				} else {
					time<- s@results$startsec[res]
				}
				if (!is.na(time)) {
					#path
					#file by default sequence name
					filename <- as.character(s@results[res, filename.fromColumnName])
					
					#append media name to file name if there is more than one media file
					if(length(in_paths)>1) {
						if (!is.na(in_descriptors[j]) && in_descriptors[j]!="") {filename = paste0(filename, "__", in_descriptors[j])}
					}
					#replace everything that is not allowed in file names
					filename <- stringr::str_replace_all(filename, "[^\\p{L}\\p{N}_\\.-]", "_")
					#add jpg suffix
					filename <- paste0(filename, ".jpg")
					#compile path
					out_filePath <- file.path(out_folder_current, filename)
					
					#get command
					args <- helper_ffmpeg_args(
						output       = out_filePath,
						input        = in_paths[j],
						startsec     = time,
						imageQuality = getOption("act.ffmpeg.image.quality", 100),
						maxHeight    = getOption("act.ffmpeg.thumbnail.max_height"))
					exif_argv <- helper_metadata_exif_argv(
						file       = out_filePath,
						sourcePath = in_paths[j],
						startsec   = time,
						clipID     = tools::file_path_sans_ext(filename))
					thumb_entries <- list(list(type = "ffmpeg", args = args), list(type = "exiftool", args = exif_argv))
					win_cmd <- paste(helper_cutlist_lines(thumb_entries, os = "win", inputVariable = in_paths[j]), collapse = "\n ")
					mac_cmd <- paste(helper_cutlist_lines(thumb_entries, os = "mac", inputVariable = in_paths[j]), collapse = "\n ")
					
					#windows version thumb 
					#(flatten/ some replacements windows )
					win_info         <- c()
					win_makedir      <- sprintf('IF NOT EXIST "%s" ( md "%s" )', out_folder_current, out_folder_current)
					win_makedir      <- stringr::str_replace_all(win_makedir, "/", "\\\\")
					win_if_statement <- "\nIF EXIST \"%1$s\" ( \n %2$s\n)\n"
					win_if_statement <- sprintf(win_if_statement, "%PATH_INPUT%" ,win_cmd)
					win_thumbnail    <- c(win_info, win_makedir, win_if_statement)
					
					#mac version thumb
					mac_info         <- c("#.... thumbnail")
					mac_makedir      <- sprintf('mkdir -p "%s"', out_folder_current)
					mac_if_statement <- "\nif [ -f \"%1$s\" ]\nthen\n %2$s\nfi\n"
					mac_if_statement <- sprintf(mac_if_statement, "$PATH_INPUT" , mac_cmd)
					mac_thumbnail    <- c(mac_info, mac_makedir, mac_if_statement)
					
					#---- . command add ----
					#to search
					s@results[res, "cuts.thumbnail.mac"] <- stringr::str_flatten(mac_thumbnail, collapse="\n")
					s@results[res, "cuts.thumbnail.win"] <- stringr::str_flatten(win_thumbnail, collapse="\n")
					
					#to total cut list
					mac_cutlist <- c(mac_cutlist, mac_thumbnail)
					win_cutlist <- c(win_cutlist, win_thumbnail)
				}
			}
			
			#===== STILLS ====
			if (exportStills & identical(.media_type_of_ext(in_suffix), "video")) {
				#values
				stills.values <- unlist(s@results$stills.values[[res]])
				if (!length(stills.values)==0) {
					
					#folder  
					out_folder_stills <- "stills"
					if ("stills.folder" %in% colnames(s@results)) {
						out_folder_stills <- s@results$stills.folder[res]
					} 
					out_folder_stills <- file.path(out_folder_current, out_folder_stills)
					
					#get names
					stills.names <- names(stills.values)
					#make sure times are double
					stills.values <- as.double(stills.values)
					#append media name to file name if there is more than one media file
					if(length(in_paths)>1) {
						if (!is.na(in_descriptors[j]) && in_descriptors[j]!="") {stills.names = paste0(stills.names, "__", in_descriptors[j])}
					}
					#replace everything that is not allowed in file names
					stills.names <- stringr::str_replace_all(stills.names, "[^\\p{L}\\p{N}_\\.-]", "_")
					#add jpg suffix
					stills.names <- paste0(stills.names, ".jpg")
					#compile path
					out_filePaths <- file.path(out_folder_stills, stills.names)
					
					#get command
					args <- lapply(seq_along(stills.values), function(k) {
						helper_ffmpeg_args(
							output       = out_filePaths[k],
							input        = in_paths[j],
							startsec     = stills.values[k],
							imageQuality = getOption("act.ffmpeg.image.quality", 100),
							maxHeight    = NULL)
					})
					exif_argv <- helper_metadata_exif_argv(
						file       = out_filePaths,
						sourcePath = in_paths[j],
						startsec   = stills.values,
						clipID     = tools::file_path_sans_ext(stills.names))
					still_entries <- c(lapply(args, function(a) list(type = "ffmpeg", args = a)),
						list(list(type = "exiftool", args = exif_argv)))
					win_cmd <- stringr::str_flatten(helper_cutlist_lines(still_entries, os = "win", inputVariable = in_paths[j]), collapse = '\n')
					mac_cmd <- stringr::str_flatten(helper_cutlist_lines(still_entries, os = "mac", inputVariable = in_paths[j]), collapse = '\n')
					
					#windows version stills
					#(flatten/ some replacements windows )
					win_info         <- c()
					win_makedir      <- sprintf('IF NOT EXIST "%s" ( md "%s" )', out_folder_stills, out_folder_stills)
					win_makedir      <- stringr::str_replace_all(win_makedir, "/", "\\\\")
					win_if_statement <-"\nIF EXIST \"%1$s\" (\n %2$s\n %3$s\n)\n"
					win_if_statement <- sprintf(win_if_statement, "%PATH_INPUT%", win_makedir, win_cmd)
					win_stills       <- c(win_info, win_if_statement)
					
					#mac version stills
					mac_info         <- c("#.... stills")
					mac_makedir      <- sprintf('mkdir -p "%s"', out_folder_stills)
					mac_if_statement <- "\nif [ -f \"%1$s\" ]\nthen\n %2$s\n %3$s\nfi\n"
					mac_if_statement <- sprintf(mac_if_statement, "$PATH_INPUT", mac_makedir ,mac_cmd)
					mac_stills       <- c(mac_info, mac_if_statement)
					
					#---- . command add ----
					#to search
					s@results[res, "cuts.stills.mac"] <- stringr::str_flatten(mac_stills, collapse="\n")
					s@results[res, "cuts.stills.win"] <- stringr::str_flatten(win_stills, collapse="\n")
					
					#to total cutlist
					mac_cutlist <- c(mac_cutlist, mac_stills)
					win_cutlist <- c(win_cutlist, win_stills)
				}	
			}
		}
	}
	
	#==== SAVE entire cut list to search object ====
	s@cuts.cutlist.win <- stringr::str_flatten(win_cutlist, collapse="\n")
	s@cuts.cutlist.mac <- stringr::str_flatten(mac_cutlist, collapse="\n")
	if (length(win_cutlist)==0) {
		w <- unique(myWarnings)
		cli::cli_abort(c("No cut list created.", stats::setNames(w, rep("-", length(w)))))
	} 
	
	#==== SAVE to file ====
	# if output folder is given
	if (!is.null(folderOutput)) {
		#-- file name
		helper_cutlist_save(cutlistMac  = if ("mac" %in% outputOS) mac_cutlist else NULL, 
							cutlistWin  = if ("win" %in% outputOS) win_cutlist else NULL, 
							outFolder   = file.path(out_folder_main, s@name), 
							outFilename = outputFileName)
	}
	
	#=== print warnings
	if (length(myWarnings) > 0) {
		w <- unique(myWarnings)
		cli::cli_warn(c("{length(w)} transcript(s) not found in corpus:", stats::setNames(w, rep("-", length(w)))))
	}
	
	#=== return
	return(s)
}

#==== FUNCTIONS ====

makeVideoBlock <- function(os, CreatePannedVersionsBlock, in_filePath, out_filename, cmd, cmd_titletext) {
	#if (os=="win") {
	#	block <- "\nIF EXIST \"INFILEPATH\" ( \n title \"cmd_titletext\"\n FFMPEGcmd1\n FFMPEGcmd2\n FFMPEGcmd3\n)\n\n"
	#} else {
	#	block <- "\nif [ -f \"INFILEPATH\" ]\nthen\n FFMPEGcmd1\n FFMPEGcmd2\n FFMPEGcmd3\nfi\n\n"
	#}
	if (os=="win") {
		block <- "\nIF EXIST \"%PATH_INPUT%\" ( \n title \"cmd_titletext\"\n FFMPEGcmd1\n FFMPEGcmd2\n FFMPEGcmd3\n)\n\n"
	} else {
		block <- "\nif [ -f \"$PATH_INPUT\" ]\nthen\n FFMPEGcmd1\n FFMPEGcmd2\n FFMPEGcmd3\nfi\n\n"
	}
	
	block <- stringr::str_replace_all(block, "cmd_titletext", cmd_titletext)
	block <- stringr::str_replace_all(block, "OUTFILENAME", out_filename)
	block <- stringr::str_replace_all(block, "INFILEPATH", in_filePath)
	
	#0 only all audio
	#1 ch1
	#2 ch2
	#3 ch1 & 2
	#4 all audio & ch1 & ch2
	if (CreatePannedVersionsBlock==0) { 
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd1", cmd[1])
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd2\n", "" )
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd3\n", "")
	}
	
	if (CreatePannedVersionsBlock==1) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd3\n", "")
	}
	
	if (CreatePannedVersionsBlock==2) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd2\n", "")
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd3", cmd[3])
	}
	
	if (CreatePannedVersionsBlock==3) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd3", cmd[3])
	}
	
	if (CreatePannedVersionsBlock==4) { 
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd1", cmd[1])
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringi::stri_replace_all_fixed(block, "FFMPEGcmd3", cmd[3])
	}
	
	return (block)
}
