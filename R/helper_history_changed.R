#' Changed since the last import or export
#'
#' Reads the \code{@history} of a transcript (or of every transcript of a
#' corpus) and reports whether entries were added after the last
#' \emph{baseline} entry. Baseline entries are the import entries written by
#' the import functions (\code{import_eaf}, \code{import_textgrid}, ...;
#' every entry whose modification starts with \code{"import_"}) and the
#' export marks written by \code{\link{helper_history_mark_export}}.
#'
#' @param x Transcript object or corpus object.
#'
#' @return Named logical vector, one element per transcript (\code{TRUE} =
#'   changed since the last import or export). For a single transcript the
#'   vector has length one.
#'
#' @seealso \link{helper_history_mark_export}
#'
#' @export
helper_history_changed <- function(x) {
	if (methods::is(x, "transcript")) {
		out <- .history_changed_single(x)
		names(out) <- x@name
		return(out)
	}
	.assert_corpus(x, missing = missing(x))
	vapply(x@transcripts, .history_changed_single, logical(1))
}

#' Mark transcripts as exported
#'
#' Appends an \code{export} entry to the \code{@history} of the given
#' transcripts of a corpus. \code{\link{corpus_export}} does not change the
#' corpus it is given; call this helper afterwards so that
#' \code{\link{helper_history_changed}} knows that the current state has been
#' written to disk.
#'
#' @param x Corpus object.
#' @param transcriptNames Vector of character strings; names of the exported
#'   transcripts. If left unspecified, all transcripts are marked.
#' @param formats Vector of character strings; the exported formats.
#' @param folderOutput Character string; the output folder.
#'
#' @return Corpus object with the updated histories.
#'
#' @seealso \link{helper_history_changed}, \link{corpus_export}
#'
#' @export
helper_history_mark_export <- function(x, transcriptNames = NULL, formats = NULL,
										folderOutput = NULL) {
	.assert_corpus(x, missing = missing(x))
	if (is.null(transcriptNames)) transcriptNames <- names(x@transcripts)
	transcriptNames <- intersect(transcriptNames, names(x@transcripts))
	for (i in transcriptNames) {
		x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history) + 1]] <- list(
			modification = "export",
			systime      = Sys.time(),
			formats      = formats,
			folderOutput = folderOutput
		)
	}
	x
}

# baseline = last import_* or export entry; changed = any entry after it
# that did change something (a cure or a rename that touched nothing counts
# as no change)
.history_changed_single <- function(t) {
	h <- t@history
	if (!length(h)) return(FALSE)
	mods <- vapply(h, function(e) as.character(e$modification %||% ""), character(1))
	base <- which(startsWith(mods, "import_") | mods == "export")
	after <- if (length(base)) h[seq_along(h) > max(base)] else h
	any(vapply(after, .history_entry_is_change, logical(1)))
}

# an entry reports a change unless every counter it carries is zero and no
# 'corrected' flag is set
.history_entry_is_change <- function(e) {
	if (!is.list(e) || !length(e)) return(FALSE)
	nms <- names(e)
	counts <- e[grepl("\\.count$", nms)]
	flags  <- e[grepl("corrected$", nms)]
	counts <- counts[vapply(counts, function(v) is.numeric(v) && length(v) == 1 && !is.na(v), logical(1))]
	flags  <- flags[vapply(flags, function(v) is.logical(v) && length(v) == 1, logical(1))]
	if (!length(counts) && !length(flags)) return(TRUE)
	any(unlist(counts) != 0) || any(unlist(flags), na.rm = TRUE)
}

# path, modification time and size of the imported file (NA when the
# transcript came from a character vector or an object)
.history_file_info <- function(filePath) {
	if (is.null(filePath) || !is.character(filePath) || !length(filePath) ||
		is.na(filePath[1]) || !nzchar(filePath[1]) || !file.exists(filePath[1]))
		return(list(path = NA_character_, file.mtime = NA, file.size = NA_real_))
	info <- file.info(filePath[1])
	list(path = filePath[1], file.mtime = info$mtime, file.size = as.numeric(info$size))
}
