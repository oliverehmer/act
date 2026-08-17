#' Select media files from a media table
#'
#' Filters the rows of a media table (as stored in \code{transcript@media})
#' down to the files that should be used, e.g. for export or playback. The
#' selection is controlled by the \code{act.media.*} options and can be
#' overridden per call.
#'
#' The function does not access the file system; it only filters the given
#' table. Existence on disk is not checked (see \link{media_path_to_existing_file}
#' for that).
#'
#' @param media Data.frame; media table with at least the columns \code{path} and \code{type} (as in \code{transcript@media}).
#' @param include Character string or \code{NA}; regular expression, only rows whose \code{path} matches are kept. \code{NA} keeps all.
#' @param exclude Character string or \code{NA}; regular expression, rows whose \code{path} matches are removed. \code{NA} removes none.
#' @param apply Logical; if \code{TRUE} the priority/maximum/audio-fallback selection is applied, if \code{FALSE} only \code{include}/\code{exclude}/\code{maxFiles} are applied.
#' @param maxFiles Integer or \code{NULL}; overall maximum number of rows returned (\code{NULL} or \code{NA} = no limit), applied last.
#' @param audioAsFallback Logical; if \code{TRUE} audio files are only returned when no video file is present.
#' @param videoMaxFiles Integer, \code{NA} or \code{NULL}; maximum number of video files (\code{NA}/\code{NULL} = no limit).
#' @param audioMaxFiles Integer, \code{NA} or \code{NULL}; maximum number of audio files (\code{NA}/\code{NULL} = no limit).
#' @param videoPriority Character vector or \code{NULL}; regex patterns matched against video file names for prioritization. First matching pattern wins.
#' @param audioPriority Character vector or \code{NULL}; file extensions for audio prioritization. First matching extension wins.
#'
#' @return Data.frame; the selected rows of the media table.
#'
#' @seealso \link{media_assign}, \link{media_path_to_existing_file}
#'
#' @export
#'
media_select <- function(media,
						 include         = NA,
						 exclude         = NA,
						 apply           = TRUE,
						 maxFiles        = NULL,
						 audioAsFallback = getOption("act.media.audio_as_fallback", FALSE),
						 videoMaxFiles   = getOption("act.media.video_max"),
						 audioMaxFiles   = getOption("act.media.audio_max"),
						 videoPriority   = getOption("act.media.video_priority"),
						 audioPriority   = getOption("act.media.audio_priority")) {

	if (nrow(media) == 0) return(media)

	if (!is.na(include) && nzchar(include)) {
		media <- media[stringr::str_detect(media$path, stringr::regex(include, ignore_case = TRUE)), , drop = FALSE]
	}
	if (!is.na(exclude) && nzchar(exclude)) {
		media <- media[!stringr::str_detect(media$path, stringr::regex(exclude, ignore_case = TRUE)), , drop = FALSE]
	}
	if (nrow(media) == 0) return(media)

	if (apply) {
		video <- media[!is.na(media$type) & media$type == "video", , drop = FALSE]
		audio <- media[!is.na(media$type) & media$type == "audio", , drop = FALSE]

		if (!is.null(videoPriority) && nrow(video) > 0) {
			video <- .media_prioritize(video, videoPriority, "name")
		}
		if (!is.null(videoMaxFiles) && !is.na(videoMaxFiles) && nrow(video) > videoMaxFiles) {
			video <- video[seq_len(videoMaxFiles), , drop = FALSE]
		}

		if (!is.null(audioPriority) && nrow(audio) > 0) {
			audio <- .media_prioritize(audio, audioPriority, "ext")
		}
		if (!is.null(audioMaxFiles) && !is.na(audioMaxFiles) && nrow(audio) > audioMaxFiles) {
			audio <- audio[seq_len(audioMaxFiles), , drop = FALSE]
		}

		if (audioAsFallback) {
			media <- if (nrow(video) > 0) video else audio
		} else {
			media <- rbind(video, audio)
		}
	}

	if (!is.null(maxFiles) && !is.na(maxFiles) && nrow(media) > maxFiles) {
		media <- media[seq_len(maxFiles), , drop = FALSE]
	}

	media
}

.media_prioritize <- function(media, priority, match_by = "name") {
	for (p in priority) {
		if (match_by == "name") {
			matched <- media[stringr::str_detect(basename(media$path), stringr::regex(p, ignore_case = TRUE)), , drop = FALSE]
		} else {
			matched <- media[tolower(tools::file_ext(media$path)) == tolower(p), , drop = FALSE]
		}
		if (nrow(matched) > 0) return(matched)
	}
	return(media)
}
