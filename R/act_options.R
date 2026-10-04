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
	act.media.timeline = list(
		value       = "editlist",
		group       = "media",
		description = "Time line on which stills and cuts are taken, matching the player the annotations were made with: 'editlist' (default) honours the edit list of MP4/MOV files like ELAN with AV Foundation or JavaFX and the browser; 'raw' ignores it like ELAN with the VLC player library. In both cases the frame on screen at the given time is taken."
	),
	act.media.ffprobe.timeout = list(
		value       = 10,
		group       = "media",
		description = "Seconds after which a single ffprobe call (media metadata, frame rate) is abandoned, so a hanging disk cannot block the session. NA or 0 = no limit."
	),

	act.time.format.transcript = list(
		value       = "h:mm:ss.s",
		group       = "export",
		description = "Time format in transcript outputs (DOCX/TXT header source line, layout reports). One of h:mm:ss.s, h:mm:ss.ss, h:mm:ss:ff, mm:ss.ss, s.s, s.ss."
	),

	act.cutlist.os = list(
		value       = c("mac", "win"),
		group       = "export",
		description = "Terminal script formats written by helper_cutlist_save(): 'mac' (POSIX sh, also runs on Linux) and/or 'win' (.cmd)."
	),
	act.cutlist.ffmpeg = list(
		value       = "ffmpeg",
		group       = "export",
		description = "How the cut lists call ffmpeg (helper_cutlist_lines()). Default 'ffmpeg': found via the PATH of the machine that runs the cut list. Set a full path only when that machine needs one; running ffmpeg from R uses act.path.ffmpeg."
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
		description = "Audio anonymization type: \"beep\", \"noise\", \"mute\" (also \"silence\") or \"distort\"; start value of the viewer. NULL: the viewer starts with \"beep\", helper_audio_filter_build() does not anonymize."
	),
	act.ffmpeg.audio.anony_beep_freq = list(
		value       = 800L,
		group       = "ffmpeg",
		description = "Frequency in Hz for the beep audio anonymization filter"
	),
	act.ffmpeg.video.bitrate = list(
		value       = "8M",
		group       = "ffmpeg",
		description = "Video bit rate of clips (H.264), used when act.ffmpeg.video.crf is NULL: a number with M (Mbit/s) or k (kbit/s), e.g. '4M', '8M', '12M', '16M'. The same value applies to the software encoder (libx264) and the Mac hardware encoder (act.ffmpeg.video.hardware)."
	),
	act.ffmpeg.video.crf = list(
		value       = NULL,
		group       = "ffmpeg",
		description = "Constant rate factor of clips instead of a bit rate (e.g. 18 or 23, lower is better); NULL = use act.ffmpeg.video.bitrate. Only the software encoder knows it: with a value set clips are always encoded with libx264."
	),
	act.ffmpeg.video.hardware = list(
		value       = TRUE,
		group       = "ffmpeg",
		description = "Encode clips with the video encoder of the Mac (h264_videotoolbox, several times faster than libx264). Without a Mac or with an ffmpeg that lacks it: a warning and libx264. Windows cut lists always use libx264."
	),
	act.ffmpeg.video.exact_timing = list(
		value       = TRUE,
		group       = "ffmpeg",
		description = "Write encoded clips with sound with -avoid_negative_ts disabled, so the picture is not shifted by the AAC encoder delay (about 21 ms) behind the sound. Set to FALSE if PowerPoint has problems with such clips, e.g. when cropping the picture."
	),
	act.ffmpeg.video.codec.copy.fail = list(
		value       = "abort",
		group       = "ffmpeg",
		description = "What happens when a video is to be copied instead of encoded (videoCodecCopy) and no keyframe can be found for the start (ffprobe missing or failed, source not readable): 'abort' stops with an error, 'encode' encodes the clip instead and shows a warning."
	),
	act.ffmpeg.video.keyframe_interval = list(
		value       = 25L,
		group       = "ffmpeg",
		description = "Distance of keyframes in clips, in frames (-g)."
	),
	act.ffmpeg.video.max_height = list(
		value       = 1080L,
		group       = "ffmpeg",
		description = "Maximum height of clips in pixels (scaled down, never up). NULL = no limit."
	),
	act.ffmpeg.video.max_width = list(
		value       = NULL,
		group       = "ffmpeg",
		description = "Maximum width of clips in pixels (scaled down, never up). NULL = no limit."
	),
	act.ffmpeg.audio.bitrate = list(
		value       = "192k",
		group       = "ffmpeg",
		description = "Bit rate of AAC sound in clips and of MP3 files."
	),
	act.ffmpeg.image.quality = list(
		value       = 100,
		group       = "ffmpeg",
		description = "JPG quality of still images from 1 (worst) to 100 (best); converted to the ffmpeg scale -q:v 31..2. ROI crops always use the best quality."
	),
	act.ffmpeg.thumbnail.max_height = list(
		value       = 720L,
		group       = "ffmpeg",
		description = "Maximum height in pixels of thumbnails (sequence preview image, search hit thumbnail, recording keyframe). NULL or 0 = full size."
	),

	act.import.readEmptyIntervals = list(
		value       = FALSE,
		group       = "import",
		description = "Read empty intervals (empty or whitespace-only content) from annotation files"
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
	act.layout.rectangle.char = list(
		value       = "\u25ad",
		group       = "layout",
		description = "Character in layer annotation content that controls the rectangle layout of its symbol segment: alone it forces the rectangle, a directly following number caps its line count, 1 forbids it; the character and its number are not printed (corpus convention - do not change mid-project)"
	),
	act.layout.rectangle.max.lines = list(
		value       = 2L,
		group       = "layout",
		description = "Line cap of the automatic rectangle layout for layer descriptions: 0 disables rectangles entirely (manual markers included), 1 disables only the automatic ones, 2 and more allow automatic rectangles up to that many lines"
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
		value       = FALSE,
		group       = "import",
		description = "Store original file content in transcript object (slot file.content); off by default because it roughly doubles the memory and cache size of a corpus"
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
