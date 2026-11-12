#' Kind of storage a path lies on
#'
#' Tells for each path whether it lies on an internal disk, an external disk
#' or a network drive. The answer comes from the table of mounted file
#' systems (macOS and other Unix: \code{mount} / \code{/proc/mounts}; Windows:
#' the drive type), never from the path itself, so it does not touch the
#' storage medium and does not hang on a network drive that does not answer.
#'
#' Rules: a network file system (smbfs, afpfs, nfs, cifs, webdav, sshfs ...)
#' or a mount that is not local is \code{"network"}; a local mount is
#' \code{"internal"} or \code{"external"} as the system says (macOS
#' \code{diskutil}, Linux \code{findmnt}, Windows drive type; the same answer
#' as the column \code{volume.kind} of the media metadata), else guessed from
#' the mount point (macOS below \code{/Volumes/}, Linux below \code{/media/},
#' \code{/run/media/}, \code{/mnt/} = external). A path below \code{/Volumes/}
#' on a drive that is not mounted, and what cannot be decided, is
#' \code{"unknown"}. The table of mounts is read at most once every 30
#' seconds, the system once per mount point.
#'
#' @param paths Vector of character strings; paths of files or folders (they need not exist).
#'
#' @return Vector of character strings, one of \code{"internal"}, \code{"external"}, \code{"network"}, \code{"unknown"} per path.
#'
#' @seealso \link{helper_storage_speed}, \link{helper_media_unique}
#'
#' @export
#'
#' @examples
#' helper_storage_kind(c(tempdir(), "/Volumes/some_drive/file.mp4"))
helper_storage_kind <- function(paths) {
	paths <- as.character(paths)
	if (length(paths) == 0) return(character(0))
	out <- rep("unknown", length(paths))
	ok <- !is.na(paths) & nzchar(paths)
	if (!any(ok)) return(out)
	p <- path.expand(paths[ok])
	if (.Platform$OS.type == "windows") {
		# one question per drive: the drive type from .media_volume_info()
		drive <- ifelse(startsWith(stringr::str_replace_all(p, stringr::fixed("\\"), "/"), "//"), "//", toupper(substr(p, 1, 2)))
		kinds <- vapply(unique(drive), function(d) .media_volume_info(if (d == "//") "//server/share" else paste0(d, "/"))$kind %||% NA_character_, character(1))
		k <- kinds[drive]
		out[ok] <- ifelse(is.na(k), "unknown", k)
		return(out)
	}
	mounts <- .storage_mounts()
	if (is.null(mounts) || nrow(mounts) == 0) return(out)
	p <- stringr::str_remove(p, "/+$")
	p[!nzchar(p)] <- "/"
	point <- .storage_mount_of(p, mounts)
	# one question per mount point: network from the mount table, internal or
	# external from .media_volume_info() (diskutil / findmnt, kept per mount)
	kind_of <- vapply(unique(point[!is.na(point)]), function(mp) {
		row <- mounts[mounts$point == mp, , drop = FALSE][1, ]
		if (identical(row$kind, "network")) return("network")
		k <- tryCatch(.media_volume_info(mp)$kind, error = function(e) NA_character_)
		if (is.null(k) || is.na(k)) row$kind else k
	}, character(1))
	out[ok] <- ifelse(is.na(point), "unknown", kind_of[point])
	out
}


#' Reading speed of a storage medium
#'
#' Reads a few megabytes of one file and returns the speed in MB/s. The
#' reading starts at a random place in the file, so the cache of the
#' operating system rarely answers instead of the medium; the time includes
#' opening the file (on a network drive the first answer is often the slow
#' part). Reading stops at \code{timeout}: the speed is then what came in
#' until then, and the attribute \code{timed_out} is \code{TRUE}.
#'
#' The function itself waits for the file: on a network drive that does not
#' answer at all it can hang. Call it in a background process with a time
#' limit of its own when that matters.
#'
#' @param path Character string; path of a file to read (ideally a media file of some megabytes).
#' @param megabytes Numeric; how much to read (MB).
#' @param timeout Numeric; time limit in seconds.
#'
#' @return Numeric; MB/s, \code{NA} when the file cannot be read. Attributes \code{bytes} (read), \code{seconds} (taken), \code{timed_out} (logical).
#'
#' @seealso \link{helper_storage_kind}
#'
#' @export
#'
#' @examples
#' f <- tempfile(); writeBin(as.raw(sample(0:255, 2e6, TRUE)), f)
#' helper_storage_speed(f, megabytes = 1, timeout = 1.5)
helper_storage_speed <- function(path, megabytes = 4, timeout = 1.5) {
	path <- as.character(path)[1]
	fail <- structure(NA_real_, bytes = 0, seconds = NA_real_, timed_out = FALSE)
	if (is.na(path) || !nzchar(path)) return(fail)
	want <- max(1, megabytes) * 1024^2
	t0 <- Sys.time()
	con <- tryCatch(file(path, open = "rb"), error = function(e) NULL, warning = function(w) NULL)
	if (is.null(con)) return(fail)
	on.exit(close(con), add = TRUE)
	size <- suppressWarnings(file.size(path))
	if (!is.na(size) && size > 2 * want) {
		tryCatch(seek(con, where = floor(stats::runif(1) * (size - want))), error = function(e) NULL)
	}
	got <- 0
	timed_out <- FALSE
	chunk <- 256 * 1024
	repeat {
		piece <- tryCatch(readBin(con, "raw", n = chunk), error = function(e) raw(0))
		got <- got + length(piece)
		elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
		if (length(piece) == 0 || got >= want) break
		if (elapsed >= timeout) { timed_out <- TRUE; break }
	}
	elapsed <- max(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1e-6)
	if (got == 0) return(structure(NA_real_, bytes = 0, seconds = elapsed, timed_out = timed_out))
	structure(got / 1024^2 / elapsed, bytes = got, seconds = elapsed, timed_out = timed_out)
}


#' One media file per name, from the best place
#'
#' Keeps one path of every media file that lies at several places. Which
#' files count as the same is set by \code{uniqueBy}: the same file name
#' (\code{"name"}), or the same name and the same size (\code{"name_size"};
#' two files with the same name but another size are then two files and both
#' are kept). Of the same files the one from the best place is kept: with
#' \code{preferLocal = TRUE} an internal disk before an external disk before
#' a network drive (\link{helper_storage_kind}); among places of the same
#' kind, and with \code{preferLocal = FALSE}, the first one in \code{paths}.
#'
#' @param paths Vector of character strings; paths of media files, in the order of the search folders.
#' @param uniqueBy Character string; \code{"name"} or \code{"name_size"}.
#' @param preferLocal Logical; if \code{TRUE} local places win over network drives.
#' @param kinds Vector of character strings or \code{NULL}; the kind of place of each path (\code{"internal"}, \code{"external"}, \code{"network"}, \code{"unknown"}), when the caller knows better than \link{helper_storage_kind}; \code{NULL} asks \link{helper_storage_kind}.
#'
#' @return Vector of character strings; the kept paths, in the order of \code{paths}.
#'
#' @seealso \link{media_assign}, \link{helper_storage_kind}
#'
#' @export
helper_media_unique <- function(paths, uniqueBy = c("name", "name_size"), preferLocal = FALSE, kinds = NULL) {
	uniqueBy <- match.arg(uniqueBy)
	paths <- as.character(paths)
	if (length(paths) < 2) return(paths)
	key <- basename(paths)
	if (identical(uniqueBy, "name_size")) key <- paste(key, suppressWarnings(file.size(paths)), sep = "\r")
	rank <- if (isTRUE(preferLocal)) {
		if (is.null(kinds) || length(kinds) != length(paths)) kinds <- helper_storage_kind(paths)
		r <- unname(c(internal = 1, external = 2, unknown = 2, network = 3)[kinds])
		r[is.na(r)] <- 2
		r
	} else rep(1, length(paths))
	ord <- order(rank, seq_along(paths))
	keep <- ord[!duplicated(key[ord])]
	paths[sort(keep)]
}


# ===== MOUNT TABLE =====

# Mounted file systems (point, kind), read at most every 30 seconds: the
# command does not touch the media, so it is safe with a hanging drive.
.storage_mounts <- function() {
	cached <- act.environment$storage_mounts
	if (!is.null(cached) && difftime(Sys.time(), cached$time, units = "secs") < 30) return(cached$table)
	tab <- tryCatch(if (Sys.info()[["sysname"]] == "Linux") .storage_mounts_linux() else .storage_mounts_mac(),
	                error = function(e) NULL)
	act.environment$storage_mounts <- list(time = Sys.time(), table = tab)
	tab
}

.STORAGE_NETWORK_TYPES <- c("smbfs", "afpfs", "nfs", "nfs4", "cifs", "smb3", "webdav", "davfs", "fuse.sshfs", "sshfs", "ftp", "9p", "fuse.rclone")

# macOS: "<device> on <point> (<type>, <flags>)"; lines given = the output of
# mount from elsewhere (tests)
.storage_mounts_mac <- function(lines = NULL) {
	if (is.null(lines)) lines <- suppressWarnings(system2("mount", stdout = TRUE, stderr = FALSE))
	# the mount point starts at the first " on /": a volume named "Fotos on Tour"
	# keeps its name whole
	m <- stringr::str_match(lines, "^(.*?) on (/.*) \\(([^,)]+)(.*)\\)$")
	m <- m[!is.na(m[, 1]), , drop = FALSE]
	if (nrow(m) == 0) return(NULL)
	type <- tolower(m[, 4]); flags <- m[, 5]; point <- m[, 3]
	local <- stringr::str_detect(flags, "\\blocal\\b")
	kind <- ifelse(type %in% .STORAGE_NETWORK_TYPES | !local, "network",
	        ifelse(startsWith(point, "/Volumes/"), "external", "internal"))
	data.frame(point = point, type = type, kind = kind, stringsAsFactors = FALSE)
}

# The mount point of each path: the longest mount point that is a prefix of
# it. A path below /Volumes/ whose drive is not mounted gets NA (only "/"
# would match it, but the drive is simply not connected).
.storage_mount_of <- function(paths, mounts = .storage_mounts()) {
	if (is.null(mounts) || nrow(mounts) == 0) return(rep(NA_character_, length(paths)))
	pts <- mounts$point[order(nchar(mounts$point), decreasing = TRUE)]
	vapply(paths, function(one) {
		for (mp in pts) {
			if (identical(mp, "/")) return(if (startsWith(one, "/Volumes/")) NA_character_ else "/")
			if (identical(one, mp) || startsWith(one, paste0(mp, "/"))) return(mp)
		}
		NA_character_
	}, character(1), USE.NAMES = FALSE)
}

# Linux: /proc/mounts "<device> <point> <type> <options> 0 0" (spaces in the point as \040)
.storage_mounts_linux <- function() {
	lines <- readLines("/proc/mounts", warn = FALSE)
	f <- stringr::str_split_fixed(lines, " ", 4)
	point <- stringr::str_replace_all(f[, 2], stringr::fixed("\\040"), " "); type <- tolower(f[, 3])
	kind <- ifelse(type %in% .STORAGE_NETWORK_TYPES, "network",
	        ifelse(stringr::str_detect(point, "^/(media|run/media|mnt)/"), "external", "internal"))
	data.frame(point = point, type = type, kind = kind, stringsAsFactors = FALSE)
}
