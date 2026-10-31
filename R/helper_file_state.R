#' Fingerprint of files (Git blob hash)
#'
#' Computes the fingerprint of the content of files as Git does
#' (\code{git hash-object}): SHA-1 of \code{"blob <size>\\0"} followed by the
#' bytes of the file. Two files with the same fingerprint have the same
#' content, byte for byte (a byte order mark and the line endings count).
#'
#' @param filePaths Vector of character strings; paths of the files.
#'
#' @return Vector of character strings, one fingerprint per file;
#'   \code{NA} for a file that does not exist or cannot be read.
#' @export
#'
#' @examples
#' library(act)
#' path <- system.file("extdata", "examplecorpus", "sync",
#'                     "SYNC_rotar_y_flexionar.eaf", package = "act")
#' helper_file_hash_compute(path)
helper_file_hash_compute <- function(filePaths) {
	vapply(as.character(filePaths), function(path) {
		if (is.na(path) || !nzchar(path)) return(NA_character_)
		size <- suppressWarnings(file.size(path))
		if (is.na(size)) return(NA_character_)
		bytes <- tryCatch(readBin(path, what = "raw", n = size), error = function(e) NULL)
		if (is.null(bytes)) return(NA_character_)
		.file_hash_raw(bytes)
	}, character(1), USE.NAMES = FALSE)
}

#' State of a file on disk
#'
#' The state a transcript keeps of the file it was read from or last written
#' to (slot \code{file.state} of a \code{transcript} object): path,
#' fingerprint, size, modification time and the time the state was taken. It
#' is compared with the file on disk before the file is overwritten
#' (\link{helper_file_state_changed}).
#'
#' @param filePath Character string; path of the file.
#' @param hash Character string; the fingerprint when it is known already
#'   (computed from bytes that were read anyway), \code{NULL} to compute it.
#'
#' @return A list with \code{path}, \code{hash} (see
#'   \link{helper_file_hash_compute}), \code{size} (bytes), \code{mtime}
#'   (POSIXct) and \code{time} (POSIXct, when the state was taken); an empty
#'   list when the file does not exist.
#' @export
#'
#' @examples
#' library(act)
#' path <- system.file("extdata", "examplecorpus", "sync",
#'                     "SYNC_rotar_y_flexionar.eaf", package = "act")
#' helper_file_state(path)
helper_file_state <- function(filePath, hash = NULL) {
	if (is.null(filePath) || !length(filePath) || is.na(filePath[1]) || !nzchar(filePath[1]) || !file.exists(filePath[1])) {
		return(list())
	}
	info <- file.info(filePath[1])
	if (is.null(hash) || is.na(hash[1])) hash <- helper_file_hash_compute(filePath[1])
	.file_state_make(filePath[1], hash, info)
}

#' Has a file changed since its state was taken?
#'
#' Compares the file on disk with a state taken earlier
#' (\link{helper_file_state}). The content decides: when size and
#' modification time are the same, the file counts as unchanged without being
#' read; otherwise its fingerprint is compared. A modification time that
#' changed without a change of the content (e.g. by a sync service) therefore
#' does not count as a change.
#'
#' @param state List; a state from \link{helper_file_state} (e.g. the slot
#'   \code{file.state} of a transcript).
#' @param filePath Character string; the file to compare. Default is the path
#'   of the state; a state of another file counts as no state.
#'
#' @return \code{FALSE} unchanged, \code{TRUE} changed or no longer there,
#'   \code{NA} no state for this file.
#' @export
#'
#' @examples
#' library(act)
#' path <- system.file("extdata", "examplecorpus", "sync",
#'                     "SYNC_rotar_y_flexionar.eaf", package = "act")
#' state <- helper_file_state(path)
#' helper_file_state_changed(state)
helper_file_state_changed <- function(state, filePath = NULL) {
	if (is.null(state) || !is.list(state) || !length(state) || !length(state$path) || !length(state$hash) ||
		is.na(state$hash[1])) {
		return(NA)
	}
	if (is.null(filePath)) filePath <- state$path
	if (!.file_same_path(state$path[1], filePath)) return(NA)
	if (!file.exists(filePath)) return(TRUE)
	info <- file.info(filePath)
	if (!is.null(state$size) && !is.null(state$mtime) && !is.na(state$size) && !is.na(state$mtime) &&
		isTRUE(as.numeric(info$size) == as.numeric(state$size)) &&
		isTRUE(abs(as.numeric(info$mtime) - as.numeric(state$mtime)) < 1)) {
		return(FALSE)
	}
	!identical(.file_hash_seen(filePath, info), as.character(state$hash[1]))
}

# A sync service (OneDrive) may change the time of a file alone: the hash is
# then remembered for that size and time, so the next check does not read
# the whole file again (one entry per file, for the R session).
.FILE_HASH_SEEN <- new.env(parent = emptyenv(), hash = TRUE)

.file_hash_seen <- function(filePath, info) {
	key <- normalizePath(filePath, winslash = "/", mustWork = FALSE)
	seen <- .FILE_HASH_SEEN[[key]]
	if (!is.null(seen) && isTRUE(seen$size == as.numeric(info$size)) &&
		isTRUE(abs(seen$mtime - as.numeric(info$mtime)) < 1)) return(seen$hash)
	hash <- helper_file_hash_compute(filePath)
	assign(key, list(size = as.numeric(info$size), mtime = as.numeric(info$mtime), hash = hash), envir = .FILE_HASH_SEEN)
	hash
}

.file_state_make <- function(path, hash, info) {
	list(path  = path,
		 hash  = as.character(hash[1]),
		 size  = as.numeric(info$size),
		 mtime = info$mtime,
		 time  = Sys.time())
}

# the fingerprint of bytes read already (helper_annotationfile_read)
.file_hash_raw <- function(bytes) {
	digest::digest(c(charToRaw(paste0("blob ", length(bytes))), as.raw(0L), bytes),
				   algo = "sha1", serialize = FALSE)
}

.file_same_path <- function(a, b) {
	if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return(FALSE)
	identical(normalizePath(as.character(a), winslash = "/", mustWork = FALSE),
			  normalizePath(as.character(b), winslash = "/", mustWork = FALSE))
}
