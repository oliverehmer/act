#' Build ffmpeg metadata arguments for cut files
#'
#' Returns a string that can be appended to ffmpeg command lines to write
#' `act.*` tags into the comment field of the output file. For video files
#' (when `fps` is supplied) a native timecode track is added via the
#' `-timecode` option.
#'
#' Returns an empty string when option `act.ffmpeg.write_metadata` is
#' `FALSE`, allowing callers to disable tagging globally without changing
#' call sites.
#'
#' Tag format: `act.source=<path>;act.startsec=<sec_3nks>;act.clipID=<id>`.
#'
#' @param sourcePath Character string. Full path to the source media file.
#' @param startsec Numeric. Start time in original source media, in seconds.
#' @param clipID Character string. Unique cut identifier (e.g. transcript
#'   name for cuts from a corpus, output filename stem otherwise).
#' @param fps Numeric or `NULL`. Frame rate of the source video. When
#'   supplied a `-timecode HH:MM:SS:FF` argument is added. `NULL` (default)
#'   omits the timecode track (suitable for audio-only outputs).
#'
#' @return Character string with `-metadata comment="..."` and optionally
#'   `-timecode "HH:MM:SS:FF"`. Returns `""` if metadata writing is
#'   disabled.
#'
#' @seealso [media_metadata_read()], [helper_metadata_exif_write()]
#'
#' @export
helper_metadata_ffmpeg_args <- function(sourcePath, startsec, clipID, fps = NULL) {
	argv <- helper_metadata_ffmpeg_argv(sourcePath, startsec, clipID, fps)
	if (length(argv) == 0) {
		return("")
	}
	paste(shQuote(argv), collapse = " ")
}


#' Build ffmpeg metadata arguments for cut files as an argument vector
#'
#' Same tags as [helper_metadata_ffmpeg_args()], but returned UNQUOTED as a
#' character vector, ready for `system2()` / an ffmpeg argument list. Callers
#' that assemble commands as vectors must use this variant: the comment carries
#' the source path, which may contain spaces, so splitting the quoted string
#' from [helper_metadata_ffmpeg_args()] again is not safe.
#'
#' Returns `character(0)` when option `act.ffmpeg.write_metadata` is `FALSE`.
#'
#' @inheritParams helper_metadata_ffmpeg_args
#'
#' @return Character vector of ffmpeg arguments, or `character(0)` if metadata
#'   writing is disabled.
#'
#' @seealso [helper_metadata_ffmpeg_args()], [media_metadata_read()]
#'
#' @export
helper_metadata_ffmpeg_argv <- function(sourcePath, startsec, clipID, fps = NULL) {
	if (!isTRUE(getOption("act.ffmpeg.write_metadata", TRUE))) {
		return(character(0))
	}

	comment <- .metadata_comment_build(sourcePath, startsec, clipID)
	argv <- c("-metadata", paste0("comment=", comment))

	if (!is.null(fps) && !is.na(fps) && is.numeric(fps) && fps > 0) {
		argv <- c(argv, "-timecode", .metadata_timecode_build(startsec, fps))
	}

	argv
}


#' Write act.* tags into the EXIF UserComment of a JPG file
#'
#' Uses the bundled exiftool from the `exifr` package (which itself
#' requires Perl on the system). When `exifr` is not installed or `perl`
#' is not on the system path, the function silently returns without
#' modifying the file. The single setup hint is emitted by
#' `iclo::config()` (not by this helper, to keep it usable as a quiet
#' library primitive).
#'
#' Uses `-overwrite_original` to avoid `.jpg_original` backup files.
#'
#' @param file Character string. Path to the JPG file to tag.
#' @param sourcePath Character string. Full path to the source media file
#'   the still was extracted from.
#' @param startsec Numeric. Time in original source media, in seconds,
#'   where the still frame was captured.
#' @param clipID Character string. Cut identifier.
#'
#' @return Invisibly `NULL`.
#'
#' @seealso [media_metadata_read()], [helper_metadata_ffmpeg_args()]
#'
#' @export
helper_metadata_exif_write <- function(file, sourcePath, startsec, clipID) {
	if (!requireNamespace("exifr", quietly = TRUE)) return(invisible(NULL))
	if (!nzchar(Sys.which("perl")))                 return(invisible(NULL))
	if (!file.exists(file))                          return(invisible(NULL))

	comment <- .metadata_comment_build(sourcePath, startsec, clipID)

	tryCatch(
		exifr::exiftool_call(
			args   = c(
				"-q",
				paste0("-UserComment=", shQuote(comment)),
				"-overwrite_original"
			),
			fnames = file,
			quiet  = TRUE
		),
		error = function(e) invisible(NULL)
	)

	invisible(NULL)
}


#' Read act.* metadata and container properties from media files
#'
#' Universal reader for media files. Supported formats: mp4, mov, wav, mp3
#' (via ffprobe) and jpg, jpeg (via exifr/exiftool). Returns a data.frame
#' with one row per file. All columns are always present; columns that do
#' not apply to the file format are `NA`.
#'
#' Column groups:
#' - act tag columns: `source`, `source.exists`, `startsec`,
#'   `startsec.source`, `clip.id`, `timecode`, `comment`
#' - container columns: `container.format`, `length.sec`, `file.size`,
#'   `mtime`
#' - video columns: `video.width`, `video.height`, `video.fps`,
#'   `video.codec`, `video.bitrate`
#' - audio columns: `audio.sample.rate`, `audio.channels`, `audio.codec`,
#'   `audio.bitrate`
#' - image columns: `image.width`, `image.height`, `bit.depth`
#'
#' The `comment` column carries consistency notes joined by `; `. Examples:
#' `"ok"`, `"no tags, using timecode"`, `"no tags, no timecode"`,
#' `"source missing on disk"`, `"INCONSISTENT: tag=X.XXX timecode=Y.YYY"`,
#' `"INCONSISTENT: clipID tag=X file=Y"`, `"clipID derived from filename"`,
#' `"unsupported format"`.
#'
#' Priority for `startsec`: `act.startsec` tag wins. If absent, the
#' timecode track value is used (MP4 tmcd stream). If neither, `NA`.
#'
#' @param file Character. Single file path or a vector of paths.
#' @param tolerance_sec Numeric. Absolute tolerance in seconds for the
#'   consistency check between `act.startsec` tag and timecode track.
#'   Default is `0.05`.
#'
#' @return A data.frame with one row per input file.
#'
#' @seealso [helper_metadata_ffmpeg_args()], [helper_metadata_exif_write()]
#'
#' @export
media_metadata_read <- function(file, tolerance_sec = 0.05) {
	rows <- lapply(file, .metadata_read_one, tolerance_sec = tolerance_sec)
	do.call(rbind, rows)
}


# ===== INTERNAL HELPERS =====

# Build the act.* comment string. Sanitises double quotes (-> single) and
# strips newlines/control characters to keep it ffmpeg-safe.
.metadata_comment_build <- function(sourcePath, startsec, clipID) {
	source_s <- .metadata_sanitise(sourcePath)
	clipID_s  <- .metadata_sanitise(clipID)
	startsec_s <- if (is.numeric(startsec) && !is.na(startsec)) {
		formatC(startsec, format = "f", digits = 3)
	} else {
		"NA"
	}

	sprintf(
		"act.source=%s;act.startsec=%s;act.clipID=%s",
		source_s,
		startsec_s,
		clipID_s
	)
}

# Convert numeric seconds + fps to "HH:MM:SS:FF" string for tmcd track.
.metadata_timecode_build <- function(startsec, fps) {
	s_total <- max(0, as.numeric(startsec))
	fps_int <- as.integer(round(fps))
	h <- floor(s_total / 3600)
	m <- floor((s_total %% 3600) / 60)
	s <- floor(s_total %% 60)
	f <- round((s_total * fps_int) %% fps_int)
	if (f >= fps_int) f <- fps_int - 1L
	sprintf("%02d:%02d:%02d:%02d", h, m, s, f)
}

# Remove characters that would break the comment string or ffmpeg parsing.
.metadata_sanitise <- function(x) {
	if (is.null(x) || length(x) == 0 || is.na(x)) return("")
	x <- as.character(x)
	x <- gsub('"', "'", x, fixed = TRUE)
	x <- gsub("\\\\", "/", x)
	x <- gsub("[\r\n\t]", " ", x)
	x <- gsub(";", ",", x, fixed = TRUE)
	x
}

# Parse a "HH:MM:SS:FF" or "HH:MM:SS;FF" timecode string into seconds.
# Returns NA if the input does not match the pattern. fps is needed only
# for the frame component; pass 25 as a safe fallback when unknown.
.metadata_timecode_to_sec <- function(tc, fps = 25) {
	if (is.null(tc) || is.na(tc) || !nzchar(tc)) return(NA_real_)
	m <- regmatches(
		tc,
		regexec("^(\\d{1,2}):(\\d{2}):(\\d{2})[:;](\\d{1,3})$", tc)
	)[[1]]
	if (length(m) != 5) return(NA_real_)
	h <- as.numeric(m[2])
	mn <- as.numeric(m[3])
	s <- as.numeric(m[4])
	fr <- as.numeric(m[5])
	fps_use <- if (is.numeric(fps) && !is.na(fps) && fps > 0) fps else 25
	h * 3600 + mn * 60 + s + fr / fps_use
}

# Parse the comment field (act.source=...;act.startsec=...;act.clipID=...)
# into a named list. Returns list of NAs if parsing fails.
.metadata_comment_parse <- function(comment) {
	out <- list(source = NA_character_, startsec = NA_real_, clipID = NA_character_)
	if (is.null(comment) || is.na(comment) || !nzchar(comment)) return(out)

	parts <- strsplit(comment, ";", fixed = TRUE)[[1]]
	for (p in parts) {
		kv <- strsplit(p, "=", fixed = TRUE)[[1]]
		if (length(kv) < 2) next
		key <- kv[1]
		val <- paste(kv[-1], collapse = "=")
		if (identical(key, "act.source"))    out$source   <- val
		if (identical(key, "act.startsec")) {
			n <- suppressWarnings(as.numeric(val))
			if (!is.na(n)) out$startsec <- n
		}
		if (identical(key, "act.clipID"))     out$clipID    <- val
	}
	out
}

# Filename stem without `__`-tag suffix.
.metadata_filename_stem <- function(file_path) {
	base <- basename(file_path)
	stem <- tools::file_path_sans_ext(base)
	parts <- strsplit(stem, "__", fixed = TRUE)[[1]]
	parts[1]
}

# Read all info for one media file. Dispatch by extension.
.metadata_read_one <- function(file_path, tolerance_sec = 0.05) {
	row <- .metadata_row_empty()
	row$file <- file_path

	ext <- tolower(tools::file_ext(file_path))
	row$format <- ext

	vol <- .media_volume_info(file_path)
	row$volume.kind <- vol$kind
	row$volume.name <- vol$name

	if (ext %in% c("mp4", "mov", "wav", "mp3")) {
		row <- .metadata_read_ffprobe(file_path, row)
	} else if (ext %in% c("jpg", "jpeg")) {
		row <- .metadata_read_exifr(file_path, row)
	} else {
		row$comment <- "unsupported format"
		return(row)
	}

	.metadata_finalise(row, tolerance_sec)
}

# Empty one-row data.frame with the full column schema.
.metadata_row_empty <- function() {
	data.frame(
		file              = NA_character_,
		format            = NA_character_,
		volume.kind       = NA_character_,
		volume.name       = NA_character_,
		source            = NA_character_,
		source.exists     = NA,
		startsec          = NA_real_,
		startsec.source   = NA_character_,
		clip.id           = NA_character_,
		timecode          = NA_character_,
		comment           = NA_character_,
		container.format  = NA_character_,
		length.sec      = NA_real_,
		file.size         = NA_real_,
		mtime             = as.POSIXct(NA),
		video.width       = NA_integer_,
		video.height      = NA_integer_,
		video.fps         = NA_real_,
		video.codec       = NA_character_,
		video.bitrate     = NA_real_,
		audio.sample.rate = NA_integer_,
		audio.channels    = NA_integer_,
		audio.codec       = NA_character_,
		audio.bitrate     = NA_real_,
		image.width       = NA_integer_,
		image.height      = NA_integer_,
		bit.depth         = NA_integer_,
		stringsAsFactors  = FALSE
	)
}

# Read mp4/mov/wav/mp3 via a single ffprobe call (json output).
.metadata_read_ffprobe <- function(file_path, row) {
	if (!file.exists(file_path)) {
		row$comment <- "file not found"
		return(row)
	}

	out <- tryCatch(
		suppressWarnings(system2(
			"ffprobe",
			args = c(
				"-v", "error",
				"-print_format", "json",
				"-show_format",
				"-show_streams",
				shQuote(file_path)
			),
			stdout = TRUE, stderr = FALSE
		)),
		error = function(e) character(0)
	)

	if (length(out) == 0) {
		row$comment <- "ffprobe failed"
		return(row)
	}

	json <- tryCatch(jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE),
	                 error = function(e) NULL)
	if (is.null(json)) {
		row$comment <- "ffprobe json parse failed"
		return(row)
	}

	fmt <- json$format
	if (!is.null(fmt)) {
		row$container.format <- fmt$format_name %||% NA_character_
		row$length.sec     <- suppressWarnings(as.numeric(fmt$duration %||% NA))
		row$file.size        <- suppressWarnings(as.numeric(fmt$size %||% NA))
		comment_tag <- fmt$tags$comment %||% NA_character_
		parsed <- .metadata_comment_parse(comment_tag)
		row$source   <- parsed$source
		row$startsec <- parsed$startsec
		row$clip.id    <- parsed$clipID
	}

	if (file.exists(file_path)) {
		fi <- file.info(file_path)
		row$mtime <- fi$mtime
		if (is.na(row$file.size)) row$file.size <- fi$size
	}

	for (st in json$streams) {
		ctype <- st$codec_type %||% ""
		if (identical(ctype, "video")) {
			row$video.width   <- suppressWarnings(as.integer(st$width   %||% NA))
			row$video.height  <- suppressWarnings(as.integer(st$height  %||% NA))
			row$video.fps     <- .metadata_parse_rational(st$r_frame_rate %||% NA_character_)
			row$video.codec   <- st$codec_name %||% NA_character_
			row$video.bitrate <- suppressWarnings(as.numeric(st$bit_rate %||% NA))
		} else if (identical(ctype, "audio")) {
			row$audio.sample.rate <- suppressWarnings(as.integer(st$sample_rate %||% NA))
			row$audio.channels    <- suppressWarnings(as.integer(st$channels    %||% NA))
			row$audio.codec       <- st$codec_name %||% NA_character_
			row$audio.bitrate     <- suppressWarnings(as.numeric(st$bit_rate %||% NA))
		} else if (identical(ctype, "data")) {
			tc <- st$tags$timecode %||% NA_character_
			if (!is.na(tc) && nzchar(tc) && is.na(row$timecode)) {
				row$timecode <- tc
			}
		}
		stream_tc <- st$tags$timecode %||% NA_character_
		if (!is.na(stream_tc) && nzchar(stream_tc) && is.na(row$timecode)) {
			row$timecode <- stream_tc
		}
	}

	row
}

# Read jpg/jpeg via exifr.
.metadata_read_exifr <- function(file_path, row) {
	if (!file.exists(file_path)) {
		row$comment <- "file not found"
		return(row)
	}

	fi <- file.info(file_path)
	row$mtime     <- fi$mtime
	row$file.size <- fi$size

	if (!requireNamespace("exifr", quietly = TRUE) || !nzchar(Sys.which("perl"))) {
		row$comment <- "exifr not available"
		return(row)
	}

	exif <- tryCatch(suppressWarnings(exifr::read_exif(file_path)),
	                 error = function(e) NULL)
	if (is.null(exif) || nrow(exif) == 0) {
		row$comment <- "exif read failed"
		return(row)
	}

	get_exif <- function(name) {
		if (name %in% colnames(exif)) exif[[name]] else NA
	}

	user_comment <- get_exif("UserComment")
	parsed <- .metadata_comment_parse(user_comment)
	row$source   <- parsed$source
	row$startsec <- parsed$startsec
	row$clip.id    <- parsed$clipID

	row$image.width  <- suppressWarnings(as.integer(get_exif("ImageWidth")))
	row$image.height <- suppressWarnings(as.integer(get_exif("ImageHeight")))
	row$bit.depth    <- suppressWarnings(as.integer(get_exif("BitsPerSample")))

	row
}

# Parse rational string "50/1" or "30000/1001" to numeric fps.
.metadata_parse_rational <- function(s) {
	if (is.null(s) || is.na(s) || !nzchar(s)) return(NA_real_)
	parts <- strsplit(s, "/", fixed = TRUE)[[1]]
	if (length(parts) == 2) {
		num <- suppressWarnings(as.numeric(parts[1]))
		den <- suppressWarnings(as.numeric(parts[2]))
		if (!is.na(num) && !is.na(den) && den != 0) return(num / den)
	}
	n <- suppressWarnings(as.numeric(s))
	if (!is.na(n)) return(n)
	NA_real_
}

# Apply final consistency checks: startsec source, source.exists, comment
# notes, clipID consistency with filename stem.
.metadata_finalise <- function(row, tolerance_sec) {
	notes <- character(0)

	has_tag_startsec <- !is.na(row$startsec)
	tc_sec <- NA_real_
	if (!is.na(row$timecode)) {
		fps_use <- if (!is.na(row$video.fps)) row$video.fps else 25
		tc_sec  <- .metadata_timecode_to_sec(row$timecode, fps_use)
	}
	has_timecode <- !is.na(tc_sec) && tc_sec > 0

	if (has_tag_startsec) {
		row$startsec.source <- "tag"
		if (has_timecode) {
			if (abs(row$startsec - tc_sec) > tolerance_sec) {
				notes <- c(notes, sprintf(
					"INCONSISTENT: tag=%.3f timecode=%.3f",
					row$startsec, tc_sec
				))
			}
		}
	} else if (has_timecode) {
		row$startsec        <- tc_sec
		row$startsec.source <- "timecode"
		notes <- c(notes, "no tags, using timecode")
	} else {
		row$startsec.source <- "none"
		notes <- c(notes, "no tags, no timecode")
	}

	stem <- .metadata_filename_stem(row$file)
	is_image <- !is.na(row$format) && tolower(row$format) %in% c("jpg", "jpeg")
	if (is.na(row$clip.id) || !nzchar(row$clip.id)) {
		row$clip.id <- stem
		notes <- c(notes, "clipID derived from filename")
	} else if (identical(row$clip.id, stem)) {
		# exact match
	} else if (is_image && startsWith(stem, paste0(row$clip.id, "_"))) {
		# still belonging to the cut (e.g. clipID_still_NNN)
	} else {
		notes <- c(notes, sprintf("INCONSISTENT: clipID tag=%s file=%s", row$clip.id, stem))
	}

	if (!is.na(row$source) && nzchar(row$source)) {
		row$source.exists <- file.exists(row$source)
		if (isFALSE(row$source.exists)) {
			notes <- c(notes, "source missing on disk")
		}
	}

	row$comment <- if (length(notes) == 0) "ok" else paste(notes, collapse = "; ")

	row
}


#' Probe the frame rate of a media file
#'
#' One ffprobe stream query instead of a full json read. Callers that write
#' metadata need the frame rate for the timecode track, see
#' [helper_metadata_ffmpeg_argv()].
#'
#' @param file_path Character string. Path to the media file.
#'
#' @return Numeric frame rate, or `NA_real_` when ffprobe fails or the file
#'   holds no video stream.
#'
#' @seealso [helper_metadata_ffmpeg_argv()], [helper_metadata_ffmpeg_args()]
#'
#' @export
helper_metadata_probe_fps <- function(file_path) {
	.metadata_probe_fps(file_path)
}


# Probe frame rate of a single media file via ffprobe (one stream query
# instead of full json). Returns NA when ffprobe fails or the file is not
# a video. Used by search_cuts_media to cache fps per source file.
.metadata_probe_fps <- function(file_path) {
	if (!file.exists(file_path)) return(NA_real_)
	out <- tryCatch(
		suppressWarnings(system2(
			"ffprobe",
			args = c(
				"-v", "error",
				"-select_streams", "v:0",
				"-show_entries", "stream=r_frame_rate",
				"-of", "csv=p=0",
				shQuote(file_path)
			),
			stdout = TRUE, stderr = FALSE
		)),
		error = function(e) character(0)
	)
	if (length(out) == 0 || !nzchar(out[1])) return(NA_real_)
	.metadata_parse_rational(out[1])
}


# Local null-coalescing operator (avoids importing rlang).
`%||%` <- function(a, b) if (is.null(a)) b else a


# ===== VOLUME INFO =====

# Cache: one lookup per mount point, keyed by the mount point path.
.METADATA_VOLUME_CACHE <- new.env(parent = emptyenv())

# Determine the storage volume of a file path: kind ("internal", "external",
# "network" or NA) and the volume name. Purely mount-table based - no file
# access, so it also works for paths whose file does not exist (yet).
.media_volume_info <- function(file_path) {
	na <- list(kind = NA_character_, name = NA_character_)
	out <- tryCatch({
		p <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
		if (.Platform$OS.type == "windows") {
			.media_volume_info_windows(p)
		} else if (Sys.info()[["sysname"]] == "Darwin") {
			.media_volume_info_mac(p)
		} else {
			.media_volume_info_linux(p)
		}
	}, error = function(e) na)
	if (is.null(out)) na else out
}

# macOS: mount table gives the mount point + fs type (network types first);
# diskutil distinguishes internal from external devices and yields the name.
.media_volume_info_mac <- function(p) {
	mp <- .media_volume_mountpoint_unix(p)
	key <- paste0("mac:", mp)
	hit <- .METADATA_VOLUME_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	fstype <- .media_volume_fstype_unix(mp)
	res <- if (!is.na(fstype) &&
			fstype %in% c("smbfs", "afpfs", "nfs", "webdav", "cifs")) {
		list(kind = "network", name = basename(mp))
	} else {
		info <- tryCatch(suppressWarnings(system2("diskutil",
			args = c("info", shQuote(mp)), stdout = TRUE, stderr = FALSE)),
			error = function(e) character(0))
		internal <- any(grepl("^\\s*Internal:\\s*Yes", info)) ||
			any(grepl("^\\s*Device Location:\\s*Internal", info))
		external <- any(grepl("^\\s*Internal:\\s*No", info)) ||
			any(grepl("^\\s*Device Location:\\s*External", info))
		nm <- sub("^\\s*Volume Name:\\s*", "",
			grep("^\\s*Volume Name:", info, value = TRUE)[1])
		if (is.na(nm) || !nzchar(trimws(nm))) nm <- basename(mp)
		if (identical(mp, "/") && !nzchar(nm)) nm <- "/"
		kind <- if (internal) "internal" else if (external) "external"
			else NA_character_
		list(kind = kind, name = trimws(nm))
	}
	assign(key, res, envir = .METADATA_VOLUME_CACHE)
	res
}

# Linux: findmnt resolves mount point, fs type and label in one call;
# a removable flag in /sys marks external devices.
.media_volume_info_linux <- function(p) {
	out <- tryCatch(suppressWarnings(system2("findmnt",
		args = c("-T", shQuote(p), "-o", "TARGET,FSTYPE,LABEL,SOURCE", "-n",
			"-P"), stdout = TRUE, stderr = FALSE)),
		error = function(e) character(0))
	if (length(out) == 0) return(list(kind = NA_character_, name = NA_character_))
	key <- paste0("lnx:", out[1])
	hit <- .METADATA_VOLUME_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	gv <- function(k) {
		m <- regmatches(out[1], regexec(paste0(k, '="([^"]*)"'), out[1]))[[1]]
		if (length(m) == 2) m[2] else ""
	}
	fstype <- gv("FSTYPE"); label <- gv("LABEL")
	src <- gv("SOURCE"); target <- gv("TARGET")
	kind <- if (grepl("^(nfs|cifs|smb|sshfs|fuse.sshfs|davfs)", fstype)) {
		"network"
	} else {
		dev <- basename(sub("[0-9]+$", "", src))
		rem <- tryCatch(readLines(file.path("/sys/class/block", dev,
			"removable"), warn = FALSE)[1], error = function(e) NA_character_)
		if (identical(rem, "1")) "external"
		else if (identical(rem, "0")) "internal" else NA_character_
	}
	nm <- if (nzchar(label)) label
		else if (identical(kind, "network")) basename(src)
		else if (nzchar(target)) basename(target) else NA_character_
	if (identical(nm, "")) nm <- NA_character_
	res <- list(kind = kind, name = nm)
	assign(key, res, envir = .METADATA_VOLUME_CACHE)
	res
}

# Windows: UNC paths are network shares; drive letters are classified via the
# Scripting.FileSystemObject drive type (2 removable, 3 fixed, 4 network).
.media_volume_info_windows <- function(p) {
	if (grepl("^//", p)) {
		parts <- strsplit(sub("^//", "", p), "/")[[1]]
		nm <- if (length(parts) >= 2) parts[2] else parts[1]
		return(list(kind = "network", name = nm))
	}
	drive <- toupper(substr(p, 1, 2))
	if (!grepl("^[A-Z]:$", drive))
		return(list(kind = NA_character_, name = NA_character_))
	key <- paste0("win:", drive)
	hit <- .METADATA_VOLUME_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	cmd <- sprintf(paste0("$d=(New-Object -ComObject Scripting.FileSystemObject",
		").GetDrive('%s'); Write-Output ($d.DriveType.ToString()+'|'+",
		"$d.VolumeName)"), drive)
	out <- tryCatch(suppressWarnings(system2("powershell",
		args = c("-NoProfile", "-Command", shQuote(cmd)),
		stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
	res <- list(kind = NA_character_, name = NA_character_)
	if (length(out) >= 1 && grepl("|", out[1], fixed = TRUE)) {
		kv <- strsplit(out[1], "|", fixed = TRUE)[[1]]
		ty <- kv[1]
		nm <- if (length(kv) >= 2 && nzchar(kv[2])) kv[2] else drive
		kind <- if (identical(ty, "2")) "external"
			else if (identical(ty, "3")) "internal"
			else if (identical(ty, "4")) "network" else NA_character_
		res <- list(kind = kind, name = nm)
	}
	assign(key, res, envir = .METADATA_VOLUME_CACHE)
	res
}

# Longest mount point that is a prefix of the path (unix mount table).
.media_volume_mountpoint_unix <- function(p) {
	out <- tryCatch(suppressWarnings(system2("mount", stdout = TRUE,
		stderr = FALSE)), error = function(e) character(0))
	mps <- sub("^.* on (.*) \\(.*$", "\\1", out)
	mps <- mps[nzchar(mps)]
	cand <- mps[startsWith(paste0(p, "/"), paste0(sub("/$", "", mps), "/"))]
	if (length(cand) == 0) return("/")
	cand[which.max(nchar(cand))]
}

# File system type of a unix mount point (from the mount table).
.media_volume_fstype_unix <- function(mp) {
	out <- tryCatch(suppressWarnings(system2("mount", stdout = TRUE,
		stderr = FALSE)), error = function(e) character(0))
	ln <- out[grepl(paste0(" on ", mp, " ("), out, fixed = TRUE)][1]
	if (is.na(ln)) return(NA_character_)
	m <- regmatches(ln, regexec("\\(([^,)]+)", ln))[[1]]
	if (length(m) == 2) m[2] else NA_character_
}
