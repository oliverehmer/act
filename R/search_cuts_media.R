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
#' The function will use all files in  \code{corpus@transcripts[[ ]]@media.path}.
#' Therefore you will need to set the options \code{filterMediaInclude} filtering for which input media files you want to create the cuts.
#' The filter is a regular expression, e.g.  \code{'\\.(wav|aif)'} for '.wav' and '.aif' audio files or \code{'\\.mp4'} for '.mp4' video files.
#' 
#' \emph{Output format}\cr
#' The output format is predefined by in the options:
#' * \code{act.ffmpeg.command} defines the basic FFmpeg command
#' * \code{act.ffmpeg.command.fastVideoPostioning} defines the FFmpeg command to be used with large video files.
#' 
#' The default is to generate mp4 video cuts. You can also use the following commands to change the output format:\cr\cr
#' MP4 video cuts with original video quality: 
#' * \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp4" -hide_banner')}
#' * \code{options(act.ffmpeg.command.fastVideoPostioning = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp4" -hide_banner')}
#' 
#' MP4 video cuts with reduced video quality:
#' * \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -vf scale=1920:-1 -b:v 1M -b:a 192k -y "OUTFILEPATH.mp4" -hide_banner')}
#' * \code{options(act.ffmpeg.command.fastVideoPostioning = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -vf scale=1920:-1 -b:v 6M -b:a 192k -y "OUTFILEPATH.mp4" -hide_banner')}
#' 
#' WAV audio cuts: 
#' * \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.wav" -hide_banner')}
#' * \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp3" -hide_banner')}
#' 
#' 
#' \emph{Advanced options}\cr
#' You can adjust the FFmpeg commands according to your needs.
#' The following options define the FFmpeg command that will be used by the package. The command needs to contain place holders which will be replaced by the actual values in the package. If you want to define your own ffmpeg command, please make sure to use the following placeholders:
#' * \code{INFILEPATH} path to the input media file.
#' * \code{OUTFILEPATH} path where the output media file will be saved
#' * \code{OPTIONS} FFmpeg options that will be applied additionally, in particular fast video positioning.
#' * \code{TIMESTART} time in seconds where to begin the cutting
#' * \code{TIMESTARTMINUS10SECONDS} time in seconds where to begin the cutting, in case that fast video positioning is being used.
#' * \code{TIMEDURATION} duration of cuts.
#'   
#' @md
#' 
#' @param x Corpus object; Please note: all media paths for a transcript need to be given as a list in the corpus object in \code{corpus@transcripts[[ ]]@media.path} . You can use the respective media functions. . 
#' @param s Search object.
#' @param cutSpanBeforesec Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param cutSpanAftersec Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.
#' @param outputFolder Character string; path to folder where files will be written.
#' @param filterMediaInclude Character string; regular expression to match only some of the media files in \code{corpus@transcripts[[ ]]@media.path}.
#' @param fastVideoPostioning Logical; If \code{TRUE} the FFmpeg command will be using the parameter fast video positioning as specified in \code{options()$act.ffmpeg.command.fastVideoPostioning}.
#' @param videoCodecCopy Logical; if \code{TRUE} FFMPEG will use the option *codec copy* for videos.
#' @param audioCutsAsMP3 Logical; If \code{TRUE} audio cuts will be exported as '.mp3' files, using  \code{options()$act.ffmpeg.command.audioCutsAsMP3}.
#' @param Panning Integer; 0=leave audio as is (ch1&ch2) , 1=only channel 1 (ch1), 2=only channel 2 (ch2), 3=both channels separated (ch1&ch2), 4=all three versions (ch1&ch2, ch1, ch2). This setting will override the option made in 'act.ffmpeg.exportchannels.fromColumnName' .
#' 
#' @return Search object; cut lists will be stored in \code{s@cuts.cutlist.mac} and \code{s@cuts.cutlist.win}.
#' @export
#'
#' @example inst/examples/search_cuts_media.R
#' 
search_cuts_media <- function(x, 
							  s, 
							  cutSpanBeforesec    = NULL,
							  cutSpanAftersec     = NULL,
							  outputFolder        = NULL, 
							  filterMediaInclude  = "", 
							  fastVideoPostioning = TRUE, 
							  videoCodecCopy      = FALSE, 
							  audioCutsAsMP3      = FALSE, 
							  Panning             = NULL) {
	#x <- examplecorpus
	#x <- corpus
	#s <- mysearch
	#outputFolder <- 
	#outputFolder <- NULL
	#filterMediaInclude <- ""
	#fastVideoPostioning <- TRUE
	#videoCodecCopy <- FALSE
	#audioCutsAsMP3 <- FALSE
	#Panning <- NULL
	
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		}	else { if (!methods::is(x,"corpus")   )	{stop("Parameter 'x' needs to be a corpus object.") } }
	if (missing(s)) 	{stop("Search object in parameter 's' is missing.") 		}	else { if (!methods::is(s, "search")	)	{stop("Parameter 's' needs to be a search object.") 	} }
	if (is.null(s@results$transcript.name))       	{ stop("Data frame s@results does not contain column 'transcript.name'.") 	}
	if (nrow(s@results)==0) 	                    { stop("Data frame s@results does not contain any search results (data frame with 0 rows)") 	}

	myWarnings <- c()

	#set progress bar	
	helper_progress_set("Creating cutlist", max(1,nrow(s@results)))
	
	#--- if cut list should be saved - check it  output folder exists
	if (is.null(outputFolder)) {
		output_folder_cutlist <- "."
	} else {
		output_folder_cutlist <- normalizePath(outputFolder, winslash = "/")
	}
	if (!is.null(cutSpanBeforesec)) 	{
		if (length(cutSpanBeforesec)!=1) {
			stop("Parameter 'cutSpanBeforesec' needs to contain only one element as a numeric value.") 
		}
		if (!is.numeric(cutSpanBeforesec)) {
			stop("Parameter 'cutSpanBeforesec' needs to be a numeric value.") 
		}
		s@cuts.span.beforesec       <- cutSpanBeforesec
	}
	if (!is.null(cutSpanAftersec)) 	{
		if (length(cutSpanAftersec)!=1) {
			stop("Parameter 'cutSpanAftersec' needs to contain only one element as a numeric value.") 
		}
		if (!is.numeric(cutSpanAftersec)) {
			stop("Parameter 'cutSpanAftersec' needs to be a numeric value.") 
		}
		s@cuts.span.aftersec       <- cutSpanAftersec
	}

	#make total lists
	cutlist_total_mac <- c()
	cutlist_total_win <- c()
	
	i <- 1
	#for each search result
	for (i in 1:nrow(s@results)) 	{
		#update progress bar
		helper_progress_tick()
		
		#reset individual lists
		cutlist_win <- c()
		cutlist_mac <- c()

		#=== get transcript
		t <- NULL
		if (is.null(s@results$transcript.name[i])) {
			#transcript not found
			myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcript.name[i]) ), collapse="\n", sep="\n")
		} else {
			t <- x@transcripts[[ s@results$transcript.name[i] ]]
			if (is.null(t)) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- Result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcript.name[i]) ), collapse="\n", sep="\n")
			}
		}
		
		if (!is.null(t)) {
			#---get paths of input files
			input_paths <- t@media.path

			if (length(input_paths)==0) {
				myWarnings <- c(myWarnings, "- No media files found for: '", t@name, "' No cuts added to cut list. \n")
			} else {
				if (filterMediaInclude!="") {
					input_paths <- input_paths[grep(pattern=filterMediaInclude, input_paths)]
				}
		
				#for each media file
				j <- 1
				for (j in 1:length(input_paths)) {
					#====== file name of original
					myMediaFileName <- stringr::str_to_lower(basename(tools::file_path_sans_ext(   input_paths[j])))
					
					#====== FOLDERS
					output_folder_result <- rep(output_folder_cutlist, 3)

					#--- add name of search
					if (s@name!="") 	{ output_folder_result <- file.path(output_folder_result, s@name)	}
					
					#--- foldergrouping1: subfolder for each search result
					foldergrouping.fromColumnName <- options()$act.export.foldergrouping1.fromColumnName
					if (foldergrouping.fromColumnName!="") {
						if(foldergrouping.fromColumnName %in% colnames(s@results)) {
						} else {
							foldergrouping.fromColumnName <- "resultID"
						}
	
						foldername <- as.character(s@results[i, foldergrouping.fromColumnName])
						if(!is.na(foldername)) {
							if(!length(foldername)==0) {
								if (foldername!="") {
									output_folder_result <- file.path(output_folder_result, foldername)
								}
							}
						}
					}
	
					#====== PANNED
					#if  CreatePannedVersions in the arguments if the functions is not set
					CreatePannedVersions <- 0
					if (!is.null(Panning)) {
						CreatePannedVersions <- Panning
					} else { 
						#check if channels are set in the search results
						if(options()$act.ffmpeg.exportchannels.fromColumnName %in% colnames(s@results)) {
							#if it is set, take the value given there
							CreatePannedVersions <- s@results[i, options()$act.ffmpeg.exportchannels.fromColumnName]
							if (is.na(CreatePannedVersions)) {
								CreatePannedVersions <-0
							}
						}
					}
					
					if (CreatePannedVersions==0 ) {			#no panning
					} else if (CreatePannedVersions==1 ) {	#only left
					} else if (CreatePannedVersions==2 ) {	#only right
					} else if (CreatePannedVersions==3)  {	#left and right
						output_folder_result[2] <- file.path(output_folder_result[2], "ch1")
						output_folder_result[3] <- file.path(output_folder_result[3], "ch2")
					} else if (CreatePannedVersions==4 ) {	#all three versions
						output_folder_result[1] <- file.path(output_folder_result[1], "ch1_ch2")
						output_folder_result[2] <- file.path(output_folder_result[2], "ch1")
						output_folder_result[3] <- file.path(output_folder_result[3], "ch2")
					}
	
					#--- create subfolder folder for each media file if more than 1 media file
					if (length(input_paths)>1) {
						output_folder_result <- file.path(output_folder_result, stringr::str_replace_all(myMediaFileName, ".*_icas\\d*_*", ""))
					}
					
					#--- foldergrouping2: subfolder for each cut
					foldergrouping.fromColumnName <- options()$act.export.foldergrouping2.fromColumnName
					if (foldergrouping.fromColumnName!="") {
						if(foldergrouping.fromColumnName %in% colnames(s@results)) {
							foldername <- as.character(s@results[i, foldergrouping.fromColumnName])
							if(!is.na(foldername)) {
								if(!length(foldername)==0) {
									if (foldername!="") {
										output_folder_result <- file.path(output_folder_result, foldername)
									}
								}
							}
						} 
					}

					#====== add create directory commands
					output_folder_result_all <- c()
					if (CreatePannedVersions==0) { output_folder_result_all <- c(output_folder_result_all, output_folder_result[1])					}
					if (CreatePannedVersions==1) { output_folder_result_all <- c(output_folder_result_all, output_folder_result[2])					}
					if (CreatePannedVersions==2) { output_folder_result_all <- c(output_folder_result_all, output_folder_result[3])					}
					if (CreatePannedVersions==3) { output_folder_result_all <- c(output_folder_result_all, output_folder_result[2:3])					}
					if (CreatePannedVersions==4) { output_folder_result_all <- c(output_folder_result_all, output_folder_result[1:3])}
					
					output_folder_result_all <- unique(output_folder_result_all)
					if (length(output_folder_result_all)>0) {
						#cmd_makedir_win  <- stringr::str_flatten(stringr::str_replace_all('IF NOT EXIST "OUTFOLDER" ( md "OUTFOLDER" )', "OUTFOLDER", output_folder_result_all), collapse='\n')
						cmd_makedir_win  <- stringr::str_flatten(sprintf('IF NOT EXIST "%s" ( md "%s" )', output_folder_result_all, output_folder_result_all), collapse='\n')
						cutlist_win <- c(cutlist_win, cmd_makedir_win)
						
						#cmd_makedir_mac  <- stringr::str_flatten(stringr::str_replace_all('mkdir -p "OUTFOLDER"', "OUTFOLDER", output_folder_result_all), collapse='\n')
						cmd_makedir_mac  <- stringr::str_flatten(sprintf('mkdir -p "%s"', output_folder_result_all), collapse='\n')
						cutlist_mac <- c(cutlist_mac, cmd_makedir_mac)
					}
					
					#====== FILE NAME
					#add name of result and file type
					#--- name of sequence : sub folder for each search result
					filename.fromColumnName <- options()$act.export.filename.fromColumnName
					if(filename.fromColumnName %in% colnames(s@results)) {
					} else {							
						filename.fromColumnName <- "resultID" 				
					}

					#====== FFMPEG cmd 
					#get format / suffix of input media file
					out_suffix <- stringr::str_to_lower(tools::file_ext(input_paths[j]))
					
					#set standard command
					cmd <- options()$act.ffmpeg.command.main
					
					#if it is a video file and fast positioning is used
					#print("----")
					#print(j)
					#print(out_suffix)
					#print(fastVideoPostioning)
					if (out_suffix %in% options()$act.fileformats.video & fastVideoPostioning) {
						#print(cmd)
						cmd <- options()$act.ffmpeg.command.fastVideoPostioning
					} 
					
					#if it is a audio file and should be converted to mp3
					if (out_suffix %in% options()$act.fileformats.audio & audioCutsAsMP3) {
						cmd <- options()$act.ffmpeg.command.audioCutsAsMP3
						#replace destination file extension with mp3
						out_suffix <- "mp3"
					} 
					
					#=== destination path
					out_filepath <- rep("",3)
					filename <- as.character(s@results[i, filename.fromColumnName])
					
					#replace everything that is not allowed in file names
					filename <- stringr::str_replace_all(filename, '/', "_")
					filename <- stringr::str_replace_all(filename, '\\\\', "_")
					filename <- stringr::str_replace_all(filename, '^\\.', "")
					
					out_filepath[1] <- file.path(output_folder_result[1], paste(filename, ".",     out_suffix, sep=""))
					out_filepath[2] <- file.path(output_folder_result[2], paste(filename, "_ch1.", out_suffix, sep=""))
					out_filepath[3] <- file.path(output_folder_result[3], paste(filename, "_ch2.", out_suffix, sep=""))
					
					#====== Replacements in command
					#--- times
					startSec 	<- max(0, s@results$startSec[i] - s@cuts.span.beforesec)
					endSec 		<- min(s@results$endSec[i] + s@cuts.span.aftersec, t@length.sec)
					
					#--- replace general place holders	
					cmd <- 	stringr::str_replace_all(cmd, "TIMESTART\\b", 			    as.character(startSec))
					cmd <- 	stringr::str_replace_all(cmd, "TIMESTARTMINUS10SECONDS",	as.character(max(0, startSec - 10)))
					cmd <- 	stringr::str_replace_all(cmd, "TIMEDURATION", 		     	as.character(endSec-startSec))
					
		
					#--- file paths 
					cmd    <- 	stringr::str_replace_all(cmd, "INFILEPATH", 				input_paths[j])
					
					#--- versions
					cmd    <-   rep(cmd,3)
					
					if (videoCodecCopy==TRUE) {
						cmd[1]  <- 	stringr::str_replace_all(cmd[1], " OPTIONS ", " -c:v copy ")
						cmd[2] <- 	stringr::str_replace_all(cmd[2], " OPTIONS ", " -af \"pan=1c|c0=c0\" -c:v copy ")
						cmd[3] <- 	stringr::str_replace_all(cmd[3], " OPTIONS ", " -af \"pan=1c|c0=c1\" -c:v copy ")
					} else {
						cmd[1]  <- 	stringr::str_replace_all(cmd[1], " OPTIONS ", " ")									#replacing with a space is important
						cmd[2] <- 	stringr::str_replace_all(cmd[2], " OPTIONS ", " -af \"pan=1c|c0=c0\" ")
						cmd[3] <- 	stringr::str_replace_all(cmd[3], " OPTIONS ", " -af \"pan=1c|c0=c1\" ")
					}
					
					cmd[1] <- 	stringr::str_replace_all(cmd[1], "OUTFILEPATH", out_filepath[1])			
					cmd[2] <- 	stringr::str_replace_all(cmd[2], "OUTFILEPATH", out_filepath[2])
					cmd[3] <- 	stringr::str_replace_all(cmd[3], "OUTFILEPATH", out_filepath[3])											
					

					#old panning options: c0=1.0*c0+0.0*c1
					
					#====== make the ffmpeg command block
					titletext   <- stringr::str_flatten(c(s@results[i, filename.fromColumnName], " (", i, " of ", nrow(s@results)," sequences)"), collapse="")
					cutlist_win <- c(cutlist_win, makeBlock("win", CreatePannedVersions, in_filepath=input_paths[j],  out_filename=as.character(s@results[i, filename.fromColumnName]), cmd, titletext))	
					cutlist_mac <- c(cutlist_mac, makeBlock("mac", CreatePannedVersions, in_filepath=input_paths[j],  out_filename=as.character(s@results[i, filename.fromColumnName]), cmd, titletext))

					#======  make some replacements
					#for windows 
					cutlist_win <- stringr::str_flatten(cutlist_win)
					cutlist_win <- stringr::str_replace_all(cutlist_win, "/", "\\\\")

					#for mac : I can not remember what this was useful for
					#cutlist_mac <- stringr::str_replace_all(cutlist_mac, "\\\\", "/")
					
					
					#--- assign to s@
					#OLD for mac, add that it is an executable
					#s@results[i, "cuts.cutlist.mac"] <- stringr::str_flatten(c("#!/bin/sh", cutlist_mac), collapse="\n")
					
					s@results[i, "cuts.cutlist.mac"] <- stringr::str_flatten(cutlist_mac, collapse="\n")
					s@results[i, "cuts.cutlist.win"] <- stringr::str_flatten(cutlist_win, collapse="\n")
					
					#====== add blocks
					#--- to collected lists
					cutlist_total_mac <- c(cutlist_total_mac, cutlist_mac)
					cutlist_total_win <- c(cutlist_total_win, cutlist_win)
				}
			}
		}
	}
	
	#save to search object
	s@cuts.cutlist.win <- stringr::str_flatten(cutlist_total_win, collapse="\n")
	s@cuts.cutlist.mac <- stringr::str_flatten(cutlist_total_mac, collapse="\n")
	
	if (length(cutlist_win)==0) {
		stop(c(myWarnings, "No cut list created."))
	} else {
		#--- save cutlists
		# if output folder is given
		if (!is.null(outputFolder)) {
			#-- make the destination folder, if it does not exist
			output_folder_cutlist.sub <- file.path(output_folder_cutlist, s@name)
			if (dir.exists(output_folder_cutlist.sub)==FALSE) 	{
				dir.create(output_folder_cutlist.sub, recursive=TRUE)
			}
			
			#-- name
			cutlistFileNameSansExt="FFMPEG_cutlist"
			if (s@name!="") {
				cutlistFileNameSansExt <- stringr::str_c(cutlistFileNameSansExt, "_",s@name)
			}
			
			#--- save cutlist_total_win as cmd
			myFilepath 	<- file.path(output_folder_cutlist.sub, paste(cutlistFileNameSansExt, "_win.cmd", sep=""))
			fileConn 	<- file(myFilepath)
			writeLines(cutlist_total_win, fileConn)
			close(fileConn)
			
			#--- save cutlist_total_mac as executable
			#add that it is an executable
			cutlist_total_mac <- c("#!/bin/sh",cutlist_total_mac)
			#save
			myFilepath 	<- file.path(output_folder_cutlist.sub, paste(cutlistFileNameSansExt, "_mac", sep=""))
			fileConn 	<- file(myFilepath)
			writeLines(cutlist_total_mac, fileConn)
			close(fileConn)
			
			#make executable on a mac or a linux machine
			if (file.exists(myFilepath)) {
				if (Sys.info()["sysname"]=="Darwin") {
					system(paste("chmod 755 '", myFilepath, "'", sep=""))
				}				
			}
		}
	}
	
	#=== print warnings
	if (length(myWarnings)>0) { warning(myWarnings) }
	
	#=== return
	return(s)
}

makeBlock <- function(os, CreatePannedVersionsBlock, in_filepath, out_filename, cmd, titletext) {
	if (os=="win") {
		block <- '
IF EXIST "INFILEPATH" ( 
 title "TITLETEXT"
 FFMPEGcmd1
 FFMPEGcmd2
 FFMPEGcmd3
)\n\n'

	} else {
		block <-'
if [ -f "INFILEPATH" ]
then
 FFMPEGcmd1
 FFMPEGcmd2
 FFMPEGcmd3
fi\n\n'
	}
	
	block <- stringr::str_replace_all(block, "TITLETEXT", titletext)
	block <- stringr::str_replace_all(block, "OUTFILENAME", out_filename)
	block <- stringr::str_replace_all(block, "INFILEPATH", in_filepath)
	
	#0 only all audio
	#1 ch1
	#2 ch2
	#3 ch1 & 2
	#4 all audio & ch1 & ch2
	if (CreatePannedVersionsBlock==0) { 
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd1", cmd[1])
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd2\n", "" )
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd3\n", "")
	}
	
	if (CreatePannedVersionsBlock==1) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd3\n", "")
	}
	
	if (CreatePannedVersionsBlock==2) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd2\n", "")
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd3", cmd[3])
	}
	
	if (CreatePannedVersionsBlock==3) { 
		block <- 	stringr::str_replace_all(block, " *FFMPEGcmd1\n", "")
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd3", cmd[3])
	}
	
	if (CreatePannedVersionsBlock==4) { 
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd1", cmd[1])
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd2", cmd[2])
		block <- 	stringr::str_replace_all(block, "FFMPEGcmd3", cmd[3])
	}
	
	return (block)
}
