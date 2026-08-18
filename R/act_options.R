.act_defaults <- list(
	act.excamplecorpusURL = list(
		value       = "https://github.com/oliverehmer/act_examplecorpus/archive/refs/heads/main.zip",
		group       = "program",
		description = "URL for downloading example corpus"
	),
	act.updateX = list(
		value       = TRUE,
		group       = "program",
		description = "Update original corpus object in search"
	),
	act.showprogress = list(
		value       = TRUE,
		group       = "program",
		description = "Show progress bars during time-consuming operations"
	),

	act.path.praat = list(
		value       = "",
		group       = "path",
		description = "Path to Praat executable",
		type        = "path"
	),
	act.path.sendpraat = list(
		value       = "",
		group       = "path",
		description = "Path to sendpraat executable",
		type        = "path"
	),
	act.path.elan = list(
		value       = "",
		group       = "path",
		description = "Path to ELAN executable",
		type        = "path"
	),
	act.path.ffmpeg = list(
		value       = "ffmpeg",
		group       = "path",
		description = "Path to the ffmpeg executable; default uses the binary on the system PATH",
		type        = "path"
	),

	act.media.fileformats.video = list(
		value       = c("mp4", "mov"),
		group       = "media",
		description = "Recognized video file suffixes"
	),
	act.media.fileformats.audio = list(
		value       = c("wav", "aif", "aiff", "mp3"),
		group       = "media",
		description = "Recognized audio file suffixes"
	),
	act.media.audio_as_fallback = list(
		value       = FALSE,
		group       = "media",
		description = "If TRUE, audio files are only selected when no video file is present (used by media_select)"
	),
	act.media.video_max = list(
		value       = NA_integer_,
		group       = "media",
		description = "Maximum number of video files selected per transcript (NA = all). Used by media_select."
	),
	act.media.audio_max = list(
		value       = NA_integer_,
		group       = "media",
		description = "Maximum number of audio files selected per transcript (NA = all). Used by media_select."
	),
	act.media.video_priority = list(
		value       = NULL,
		group       = "media",
		description = "Ordered regex patterns for video file priority; first matching pattern wins (used by media_select)"
	),
	act.media.audio_priority = list(
		value       = NULL,
		group       = "media",
		description = "Ordered file extensions for audio priority; first matching extension wins (used by media_select)"
	),

	act.cutlist.os = list(
		value       = c("mac", "win"),
		group       = "export",
		description = "Terminal script formats written by helper_cutlist_save(): 'mac' (POSIX sh, also runs on Linux) and/or 'win' (.cmd)."
	),

	act.ffmpeg.command.video = list(
		value       = 'ffmpeg INPUTARGS OPTIONS -y "OUTFILEPATH" -hide_banner',
		group       = "ffmpeg",
		description = "FFmpeg command for video cuts. INPUTARGS is filled by act based on videoFastPositioning."
	),
	act.ffmpeg.command.audio = list(
		value       = 'ffmpeg INPUTARGS -c copy -y "OUTFILEPATH" -hide_banner',
		group       = "ffmpeg",
		description = "FFmpeg command for audio cuts (stream copy)."
	),
	act.ffmpeg.command.audio.mp3 = list(
		value       = 'ffmpeg INPUTARGS OPTIONS -y "OUTFILEPATH" -hide_banner',
		group       = "ffmpeg",
		description = "FFmpeg command for MP3 audio export."
	),
	act.ffmpeg.command.images = list(
		value       = 'ffmpeg INPUTARGS -frames:v 1 -q:v 2 -update 1 -y "OUTFILEPATH"',
		group       = "ffmpeg",
		description = "FFmpeg command for still image extraction. INPUTARGS is filled by act based on videoFastPositioning."
	),
	act.ffmpeg.channels_from_column = list(
		value       = "channels",
		group       = "ffmpeg",
		description = "Column name for audio channel export"
	),
	act.ffmpeg.write_metadata = list(
		value       = TRUE,
		group       = "ffmpeg",
		description = "Write act.* metadata tags (and timecode track for MP4) into cut files. See helper_metadata_ffmpeg_args() and helper_metadata_exif_write()."
	),
	act.ffmpeg.audio.loudnorm = list(
		value       = NULL,
		group       = "ffmpeg",
		description = "loudnorm filter string for audio normalization applied during cutting (NULL = disabled). Example: \"loudnorm=I=-16:LRA=7:TP=-2\""
	),
	act.ffmpeg.audio.anony_filter = list(
		value       = NULL,
		group       = "ffmpeg",
		description = "Audio anonymization filter type: \"silence\", \"beep\", \"noise\", or NULL (disabled)"
	),
	act.ffmpeg.audio.anony_beep_freq = list(
		value       = 1000L,
		group       = "ffmpeg",
		description = "Frequency in Hz for the beep audio anonymization filter"
	),

	act.import.readEmptyIntervals = list(
		value       = FALSE,
		group       = "import",
		description = "Read empty intervals from annotation files"
	),
	act.layout.wrap.marker = list(
		value       = "mondada",
		group       = "layout",
		description = "Continuation marker style of the alignment engine: 'mondada' (-> arrow) or 'arrow'"
	),
	act.layout.label.mode = list(
		value       = "mondada",
		group       = "layout",
		description = "Tier label mode for multimodal layer lines: 'mondada' (label dropped when actor is the speaker) or 'always'"
	),
	act.layout.keeptogether.char = list(
		value       = "\u203f",
		group       = "layout",
		description = "Character in annotation content that glues the adjacent parts together: no line break, no fill insertion at this spot; the character itself is not printed - an adjacent space is kept but glued (corpus convention - do not change mid-project)"
	),
	act.layout.linebreak.char = list(
		value       = "\u23ce",
		group       = "layout",
		description = "Character in annotation content that forces a manual line break in the alignment engine (corpus convention - do not change mid-project)"
	),
	act.import.replaceNewlinesWith = list(
		value       = " ",
		group       = "import",
		description = "Replace line breaks in annotation content with this string on import (NA = keep line breaks)"
	),
	act.import.scanSubfolders = list(
		value       = TRUE,
		group       = "import",
		description = "Scan subfolders for annotation files"
	),
	act.import.storefileContentInTranscript = list(
		value       = TRUE,
		group       = "import",
		description = "Store original file content in transcript object"
	),

	act.export.filename.fromColumnName = list(
		value       = "resultID",
		group       = "export",
		description = "Column name for export filenames"
	),
	act.export.folder.grouping1.fromColumnName = list(
		value       = "resultID",
		group       = "export",
		description = "Column for folder grouping level 1"
	),
	act.export.folder.grouping2.fromColumnName = list(
		value       = "",
		group       = "export",
		description = "Column for folder grouping level 2"
	),

	act.separator_between_intervals = list(
		value       = "&",
		group       = "parsing",
		description = "Separator between intervals in fulltext"
	),
	act.separator_between_tiers = list(
		value       = "#",
		group       = "parsing",
		description = "Separator between tiers in fulltext"
	),
	act.separator_between_words = list(
		value       = "^\\s|\\|\\'|\\#|\\/|\\\\\\\\",
		group       = "parsing",
		description = "Regex for word separators in concordance"
	),
	act.wordCountRegEx = list(
		value       = '(?<=[^|\\b])[A-z\\u00C0-\\u00FA\\-\\:]+(?=\\b|\\s|_|$)',
		group       = "parsing",
		description = "Regex for word counting"
	),
	act.pauseIdentifierGATRegEx = list(
		value       = '^\\s*(\\([\\d\\.-]*\\)\\s*)+$',
		group       = "parsing",
		description = "Regex for GAT pause identification, supports multiple consecutive pauses"
	),
	act.concordanceWidth = list(
		value       = NULL,
		group       = "parsing",
		description = "Number of characters left/right of the search hit in the concordance (NULL = search class default of 120)"
	)
)

# Simple named list of default values for .onLoad and options_reset
act.options.default <- lapply(.act_defaults, function(x) x$value)


#' Options of the package
#'
#' The package has numerous options that change the internal workings of the package.
#'
#' There are several options that change the way the package works. They are set globally.
#' * Use `options(name.of.option = value)` to set an option.
#' * Use `options()$name.of.option` to get the current value of an option.
#' * Use `act::options_reset()` to set all options to the default value.
#' * Use `act::options_delete()` to clean up and delete all option settings.
#'
#' Options are organized in groups:
#'
#' **Program:** General behavior settings (progress bars, corpus updates).
#'
#' **Paths:** Paths to external programs (Praat, ELAN).
#'
#' **Media:** Recognized audio and video file extensions and the media
#' selection settings (priority, maximum number, audio fallback).
#'
#' **FFmpeg:** Commands and options for media cutting and still extraction.
#'
#' **Import:** Settings for reading annotation files.
#'
#' **Export:** Settings for file naming and folder structure.
#'
#' **Parsing:** Separators, word counting, and pause identification patterns.
#'
#' @param group Character string; optional name of an option group to show. One of \code{"program"}, \code{"path"}, \code{"media"}, \code{"ffmpeg"}, \code{"import"}, \code{"export"}, \code{"parsing"}. If \code{NULL} (the default), all options are shown.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' library(act)
#' \dontrun{
#' act::options_show()
#' }

options_show <- function (group = NULL) {
	groups <- list(
		program     = "program",
		path        = "path",
		media       = "media",
		ffmpeg      = "ffmpeg",
		import      = "import",
		export      = "export",
		parsing     = "parsing"
	)

	if (!is.null(group) && !group %in% names(groups)) {
		cli::cli_abort("Unknown group {.val {group}}. Available: {.val {names(groups)}}")
	}

	all_names <- character()
	for (grp_id in names(groups)) {
		grp_opts <- Filter(function(x) identical(x$group, grp_id), .act_defaults)
		all_names <- c(all_names, names(grp_opts))
	}
	w_name   <- max(nchar(all_names), na.rm = TRUE) + 2
	w_source <- 12
	total_w  <- 120

	cli::cli_rule("act options")

	for (grp_id in names(groups)) {
		if (!is.null(group) && grp_id != group) next

		grp_opts <- Filter(function(x) identical(x$group, grp_id), .act_defaults)
		if (length(grp_opts) == 0) next

		cat("\n")
		cli::cli_text("{.strong {groups[[grp_id]]}}")

		for (nm in names(grp_opts)) {
			current <- getOption(nm)
			default_val <- grp_opts[[nm]]$value

			if (is.null(current)) {
				val_str <- "NULL"
			} else if (length(current) == 1 && is.na(current)) {
				val_str <- "NA"
			} else if (length(current) == 1 && is.character(current) && current == "") {
				val_str <- "\"\""
			} else {
				val_str <- paste(as.character(current), collapse = ", ")
			}

			source_str <- if (identical(current, default_val)) "[default]" else "[user]"
			label      <- stringr::str_pad(nm, w_name, side = "right")
			source_pad <- stringr::str_pad(source_str, w_source, side = "right")
			cat(paste0("  ", label, source_pad, val_str, "\n"))
		}
	}
	cat("\n")
}


#' Delete all options set by the package from R options
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_delete()
options_delete <- function() {
	for (nm in names(.act_defaults)) {
		do.call(options, stats::setNames(list(NULL), nm))
	}
}


#' Reset options to default values
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_reset()
options_reset <- function () {
	options(act.options.default)
}
