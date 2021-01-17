act.options.default <- list (
	#--- do not reset
	act.excamplecorpusURL                   = " http://www.romanistik.uni-freiburg.de/ehmer/files/digitalhumanities/act_examplecorpus.zip",
	act.updateX                             = TRUE,
	act.showprogress				    	= TRUE,
	
	act.path.praat					    	= "",
	act.path.sendpraat					    = "",
	act.path.elan                           = "",

	act.fileformats.video                      = c("mp4", "mov"),
	act.fileformats.audio                      = c("wav", "aif", "aiff", "mp3"),
	
	act.ffmpeg.command.main	 	 	           = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.fastVideoPostioning     = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.audioCutsAsMP3          = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPAT" -hide_banner',
	act.ffmpeg.exportchannels.fromColumnName       = "channels", 
	
	act.import.readEmptyIntervals 			= FALSE,
	act.import.scanSubfolders                 = TRUE,
	act.import.storeFileContentInTranscript   = TRUE,
	
	act.export.foldergrouping1.fromColumnName 		= "resultID",
	act.export.foldergrouping2.fromColumnName 		= "",
	act.export.filename.fromColumnName 			    = "resultID",
	
	act.separator_between_intervals 		= "&",
	act.separator_between_tiers				= "#",
	act.separator_between_words				= "^\\s|\\|\\'|\\#|\\/|\\\\\\\\",
	act.wordCount.regex 					= '(?<=[^|\\b])[A-z\\u00C0-\\u00FA\\-\\:]+(?=\\b|\\s|_|$)'
)

#' Options of the package
#'
#' The package has numerous options that change the internal workings of the package.
#' 
#' There are several options that change the way the package works. They are set globally.
#' * Use \code{options(name.of.option = value)} to set an option.
#' * Use \code{options()$name.of.option} to get the current value of an option.
#' * Use \code{act::options_reset} to set all options to the default value.
#' * Use \code{act::options_delete} to clean up and delete all option settings.
#'
#' The package uses the following options.
#' 
#' \emph{Program}
#' * \code{act.excamplecorpusURL} character strings; where to download example media files.
#' * \code{act.updateX} Logical; If \code{TRUE} the original corpus object 'x' passed passed to the search functions \code{search_new} and \code{search_run} will also be updated, in case that during the search fulltexts are created or the normalization is performed. 
#' * \code{act.showprogress} logical; if \code{TRUE} a progress bar will be shown during (possibly) time consuming operations.
#' 
#' \emph{Paths}
#' * \code{act.path.praat} Character string; path to the 'Praat' executable on your computer. Only necessary if you use the functions to remote control Praat using Praat scripts.
#' * \code{act.path.sendpraat} Character string; path to the 'sendpraat' executable on your computer. Only necessary if you use the functions to remote control Praat using Praat scripts.
#' * \code{act.path.elan} Character string; path to the 'ELAN' executable on your computer. Only necessary if you want to open search results in ELAN.
#' 
#' \emph{File formats}
#' * \code{act.fileformats.video} Vector of character strings; Suffixes of video files that will be identified; default is 'c("mp4", "mov")'.
#' * \code{act.fileformats.audio} Vector of character strings; Suffixes of audio files that will be identified; default is 'c("wav", "aif", "mp3")'.
#'
#' \emph{FFMPEG commands and options}
#' * \code{act.ffmpeg.command} Character string; 'FFmpeg' command that is used for cutting video files.
#' * \code{act.ffmpeg.command.fastVideoPostioning} Character string; 'FFmpeg' command that is used for cutting video files using the 'FFmpeg' option  'fast video positioning'. This is considerably faster when working with long video files.
#' * \code{act.ffmpeg.command.audio} Character string; 'FFmpeg' command that is used for cutting/generating uncompressed audio files.
#' * \code{act.ffmpeg.command.UsefastVideoPostioning} Logical; if \code{TRUE} the 'FFmpeg' option using fast video positioning (ant the respective commands as defined in the other options) will be used.
#' * \code{act.ffmpeg.exportchannels.fromColumnName} Character string; Name of the column in the data frame \code{s@results} from information, which audio channel to export, will be taken.
#' 
#' \emph{Import annotation files}
#' * \code{act.import.readEmptyIntervals} Logical; if \code{TRUE} empty intervals in you annotation files will be read, if \code{FALSE} empty intervals will be skipped.
#' * \code{act.import.scanSubfolders} Logical; if \code{TRUE} sub folders will also be scanned for annotation files; if \code{FALSE} only the main level of the folders specified in \code{paths.annotation.files} of your corpus object will be scanned. 
#' * \code{act.import.storeFileContentInTranscript} if \code{TRUE} the contents of the original annotation file will be stored in \code{transcript@file.content}. Set to \code{FALSE} if you want to keep your corpus object small.
#' 
#' \emph{Export}
#' * \code{act.export.foldergrouping1.fromColumnName} Character string; Name of sub folders that will be created in the folder of the search result, level 1.
#' * \code{act.export.foldergrouping2.fromColumnName}  Character string; Name of sub folders that will be created in the folder of the search result, level 2.
#' * \code{act.export.filename.fromColumnName}  Character string; Name of the column from which the file names for exported files will be taken.
#'  
#' \emph{Miscellaneous}
#' * \code{act.separator_between_intervals} Character; Single character that is used for separating intervals when creating the full text.
#' * \code{act.separator_between_tiers} Character; Single character that is used for separating tiers when creating the full text.
#' * \code{act.separator_between_words} Character string; regular expression with alternatives that count as separators between words. Used for preparing the concordance.
#' * \code{act.wordCount.regex} Character string; regular expression that is used to count words.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' library(act)
#' \dontrun{
#' act::options_show()
#' }
#' 
options_show <- function () {
	cat("options", fill=TRUE)
	cat("  Program", fill=TRUE)
	cat("    act.excamplecorpusURL                          : ",paste("'", options()$act.excamplecorpusURL,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.showprogress                               : ",paste("'", options()$act.showprogress,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.updateX                                    : ",paste("'", options()$act.updateX,"'",sep="", collapse=""), fill=TRUE)
	cat("  Paths", fill=TRUE)
	cat("    act.path.praat                                 : ",paste("'", options()$act.path.praat,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.path.sendpraat                             : ",paste("'", options()$act.path.sendpraat, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.path.elan                                  : ",paste("'", options()$act.path.elan, "'",sep="", collapse=""),fill=TRUE)
	cat("", fill=TRUE)
	cat("  File formats", fill=TRUE)
	cat("    act.fileformats.video                          : ",paste("'", options()$act.fileformats.video, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.fileformats.audio                          : ",paste("'", options()$act.fileformats.audio, "'",sep="", collapse=""),fill=TRUE)
	cat("", fill=TRUE)	
	cat("  FFMPEG commands and options", fill=TRUE)
	cat("    act.ffmpeg.command                             : ",paste("'", options()$act.ffmpeg.command,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.ffmpeg.command.fastVideoPostioning         : ",paste("'", options()$act.ffmpeg.command.fastVideoPostioning, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.ffmpeg.command.audio                       : ",paste("'", options()$act.ffmpeg.command.audio, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.ffmpeg.command.UsefastVideoPostioning      : ",paste("'", options()$act.ffmpeg.command.UsefastVideoPostioning, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.ffmpeg.exportchannels.fromColumnName       : ",paste("'", options()$act.ffmpeg.exportchannels.fromColumnName, "'",sep="", collapse=""),fill=TRUE)
	cat("", fill=TRUE)
	cat("  Import annotation files", fill=TRUE)
	cat("    act.import.readEmptyIntervals                  : ", options()$act.import.readEmptyIntervals, fill=TRUE)
	cat("    act.import.scanSubfolders                      : ", options()$act.import.scanSubfolders, fill=TRUE)
	cat("    act.import.storeFileContentInTranscript        : ", options()$act.import.scanSubfolders, fill=TRUE)
	cat("", fill=TRUE)
	cat("  Export", fill=TRUE)
	cat("    act.export.foldergrouping1.fromColumnName      : ",paste("'", options()$act.export.foldergrouping1.fromColumnName,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.export.foldergrouping2.fromColumnName      : ",paste("'", options()$act.export.foldergrouping2.fromColumnName,"'",sep="", collapse=""), fill=TRUE)
	cat("    act.export.filename.fromColumnName             : ",paste("'", options()$act.export.filename.fromColumnName, "'",sep="", collapse=""),fill=TRUE)
	cat("", fill=TRUE)
	cat("  Miscellaneous", fill=TRUE)
	cat("    act.separator_between_intervals                : ",paste("'", options()$act.separator_between_intervals, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.separator_between_tiers                    : ",paste("'", options()$act.separator_between_tiers, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.separator_between_words                    : ",paste("'", options()$act.separator_between_words, "'",sep="", collapse=""),fill=TRUE)
	cat("    act.wordCount.regex                            : ",paste("'", options()$act.wordCount.regex, "'",sep="", collapse=""), fill=TRUE)
}

	
#' delete all options set by the package from R options
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_delete()
options_delete <- function() {

	options(act.excamplecorpusURL                   = NULL)
	options(act.showprogress                        = NULL)
	options(act.updateX                             = NULL)

	options(act.path.praat                          = NULL)
	options(act.path.sendpraat						= NULL)
	options(act.path.elan	  					    = NULL)
	
	options(act.fileformats.video                   = NULL)
	options(act.fileformats.audio                   = NULL)
	
	options(act.ffmpeg.command.main	 	 	 	    = NULL)
	options(act.ffmpeg.command.fastVideoPostioning  = NULL)
	options(act.ffmpeg.command.audioCutsAsMP3       = NULL)
	options(act.ffmpeg.exportchannels.fromColumnName= NULL)
			
	options(act.import.readEmptyIntervals 		    = NULL)
	options(act.import.scanSubfolders               = NULL)
	options(act.import.storeFileContentInTranscript = NULL)
	
	options(act.export.foldergrouping1.fromColumnName= NULL)
	options(act.export.foldergrouping2.fromColumnName= NULL)
	options(act.export.filename.fromColumnName       = NULL)
	
	options(act.separator_between_intervals          = NULL)
	options(act.separator_between_tiers              = NULL)
	options(act.separator_between_words              = NULL)
	options(act.wordCount.regex                      = NULL)
}


#' Reset options to default values
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_reset()
options_reset <- function () {
	options(act.options.default[2:length(act.options.default)])
}
