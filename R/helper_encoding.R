#' Detect the text encoding of an annotation file
#'
#' Determines the encoding of a text file deterministically using a
#' four-step strategy:
#'
#' \enumerate{
#'   \item BOM detection in the first bytes: UTF-8 BOM
#'     (\code{EF BB BF}), UTF-16 LE BOM (\code{FF FE}),
#'     UTF-16 BE BOM (\code{FE FF}).
#'   \item NULL-byte guard in the first 1000 bytes. Plain text files
#'     never contain \code{0x00}; UTF-16 without BOM would be full of
#'     NULL bytes and would otherwise pass \code{base::validUTF8()}
#'     (because ASCII-range bytes are formally valid UTF-8).
#'   \item UTF-8 validation via \code{stringi::stri_enc_isutf8()}
#'     on the whole file.
#'   \item Fallback to LATIN1 (every byte sequence is valid LATIN1).
#' }
#'
#' @param filePath Character string; path to the file.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{encoding}}{One of \code{"UTF-8"}, \code{"UTF-8-BOM"},
#'     \code{"UTF-16LE"}, \code{"UTF-16BE"}, \code{"LATIN1"},
#'     \code{"UTF-16-no-BOM-likely"}, or \code{NA_character_} on error.}
#'   \item{\code{confidence}}{One of \code{"bom"}, \code{"validated"},
#'     \code{"fallback"}, \code{"unknown"}, or \code{"error"}.}
#'   \item{\code{message}}{Human-readable explanation when the result is
#'     not \code{"ok"}.}
#' }
#'
#' @seealso \code{\link{helper_read_annotation_file}}
#'
#' @export
helper_detect_file_encoding <- function(filePath) {

	if (!file.exists(filePath)) {
		return(list(encoding = NA_character_, confidence = "error",
					message = "File does not exist"))
	}

	fsize <- file.info(filePath)$size
	if (is.na(fsize) || fsize == 0) {
		return(list(encoding = NA_character_, confidence = "error",
					message = "File is empty"))
	}

	all_bytes <- tryCatch(
		readBin(filePath, what = "raw", n = fsize),
		error = function(e) NULL
	)
	if (is.null(all_bytes) || length(all_bytes) == 0) {
		return(list(encoding = NA_character_, confidence = "error",
					message = "Could not read file"))
	}

	# File contains only a BOM (2 or 3 bytes) - no real content
	if (length(all_bytes) <= 3) {
		if (length(all_bytes) == 2 &&
			(identical(all_bytes, as.raw(c(0xFF, 0xFE))) ||
			 identical(all_bytes, as.raw(c(0xFE, 0xFF))))) {
			return(list(encoding = NA_character_, confidence = "error",
						message = "File contains only a UTF-16 BOM"))
		}
		if (length(all_bytes) == 3 &&
			identical(all_bytes, as.raw(c(0xEF, 0xBB, 0xBF)))) {
			return(list(encoding = NA_character_, confidence = "error",
						message = "File contains only a UTF-8 BOM"))
		}
		return(list(encoding = NA_character_, confidence = "error",
					message = "File too short"))
	}

	# BOM detection
	if (all_bytes[1] == as.raw(0xFF) && all_bytes[2] == as.raw(0xFE)) {
		return(list(encoding = "UTF-16LE", confidence = "bom", message = ""))
	}
	if (all_bytes[1] == as.raw(0xFE) && all_bytes[2] == as.raw(0xFF)) {
		return(list(encoding = "UTF-16BE", confidence = "bom", message = ""))
	}
	if (length(all_bytes) >= 3 &&
		all_bytes[1] == as.raw(0xEF) && all_bytes[2] == as.raw(0xBB) &&
		all_bytes[3] == as.raw(0xBF)) {
		return(list(encoding = "UTF-8-BOM", confidence = "bom", message = ""))
	}

	# No BOM: check for NULL bytes in the first 1000 bytes.
	# Plain text files never contain 0x00; UTF-16 without BOM in the ASCII
	# range would pass validUTF8() but be full of NULL bytes.
	check_range <- seq_len(min(1000L, length(all_bytes)))
	if (any(all_bytes[check_range] == as.raw(0x00))) {
		return(list(encoding = "UTF-16-no-BOM-likely", confidence = "unknown",
					message = paste0("File contains NULL bytes in the first ",
									 "1000 bytes; likely UTF-16 without BOM ",
									 "or a binary file")))
	}

	# Validate as UTF-8 via stringi (handles raw bytes correctly)
	is_valid_utf8 <- tryCatch(
		isTRUE(stringi::stri_enc_isutf8(list(all_bytes))),
		error   = function(e) FALSE,
		warning = function(w) FALSE
	)
	if (is_valid_utf8) {
		return(list(encoding = "UTF-8", confidence = "validated", message = ""))
	}

	# Fallback: LATIN1 (every byte sequence is valid LATIN1)
	list(encoding = "LATIN1", confidence = "fallback",
		 message = "Not valid UTF-8; falling back to LATIN1")
}


#' Read an annotation file with automatic encoding detection
#'
#' High-level reader that combines encoding detection with decoding,
#' UTF-8 BOM stripping, optional XML declaration check
#' (for EAF/EXB files), and optional header verification (e.g. TextGrid).
#'
#' The function reads the file with the detected encoding, removes a
#' leading UTF-8 BOM character from line 1 if present, and (for XML
#' files) extracts the declared encoding from the \code{<?xml ... ?>}
#' header and compares it to the detected one. Mismatches are reported
#' as a warning when \code{verbose = TRUE} but the file is still read
#' with the detected encoding.
#'
#' @param filePath Character string; path to the file.
#' @param expectedHeader Character vector; required prefixes of the
#'   first N lines, e.g.
#'   \code{c('File type = "ooTextFile"', 'Object class = "TextGrid"')}.
#'   If \code{NULL}, no header check is performed.
#' @param fileType Character string; one of \code{"textgrid"},
#'   \code{"eaf"}, \code{"exb"}, \code{"srt"}, or \code{NULL}. Controls
#'   XML declaration detection for EAF/EXB files even when line 1 does
#'   not start with \code{"<?xml"}.
#' @param verbose Logical; if \code{TRUE} (default) warnings are emitted
#'   via \code{cli::cli_warn()}. If \code{FALSE}, only the return value
#'   contains the diagnostics.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{lines}}{Character vector with file contents,
#'     or \code{NULL} on error.}
#'   \item{\code{encoding_detected}}{Detected encoding (see
#'     \code{\link{helper_detect_file_encoding}}).}
#'   \item{\code{encoding_declared}}{Encoding declared in the XML header,
#'     or \code{NA_character_}.}
#'   \item{\code{encoding_match}}{Logical; whether declared and detected
#'     encoding belong to the same family, or \code{NA} if not
#'     applicable.}
#'   \item{\code{status}}{\code{"ok"} or \code{"error"}.}
#'   \item{\code{message}}{Collected diagnostic messages, separated by
#'     \code{"; "}.}
#' }
#'
#' @seealso \code{\link{helper_detect_file_encoding}},
#'   \code{\link{helper_encodings_equivalent}}
#'
#' @export
helper_read_annotation_file <- function(filePath,
										 expectedHeader = NULL,
										 fileType       = NULL,
										 verbose        = TRUE) {

	messages <- character(0)

	enc_info <- helper_detect_file_encoding(filePath)

	if (enc_info$confidence == "error" || is.na(enc_info$encoding)) {
		if (verbose) {
			msg <- enc_info$message
			cli::cli_warn("{msg}: {.file {filePath}}")
		}
		return(list(
			lines             = NULL,
			encoding_detected = enc_info$encoding,
			encoding_declared = NA_character_,
			encoding_match    = NA,
			status            = "error",
			message           = enc_info$message
		))
	}

	if (enc_info$confidence == "unknown") {
		if (verbose) {
			msg <- enc_info$message
			cli::cli_warn("{msg}: {.file {filePath}}")
		}
		return(list(
			lines             = NULL,
			encoding_detected = enc_info$encoding,
			encoding_declared = NA_character_,
			encoding_match    = NA,
			status            = "error",
			message           = enc_info$message
		))
	}

	read_encoding <- switch(
		enc_info$encoding,
		"UTF-8"     = "UTF-8",
		"UTF-8-BOM" = "UTF-8",
		"UTF-16LE"  = "UTF-16LE",
		"UTF-16BE"  = "UTF-16BE",
		"LATIN1"    = "windows-1252",
		enc_info$encoding
	)

	#read bytes and convert to UTF-8 directly: a text-mode connection with
	#encoding= would convert to the NATIVE encoding instead, which is lossy on
	#non-UTF-8 locales (Windows). windows-1252 is used for LATIN1-detected
	#files because it is the practical superset (0x80-0x9F: euro sign, smart
	#quotes, dashes) and identical to ISO-8859-1 everywhere else.
	lines <- tryCatch({
		raw_content <- readBin(filePath, what = "raw", n = file.info(filePath)$size)
		txt <- stringi::stri_encode(raw_content, from = read_encoding, to = "UTF-8")
		stringi::stri_split_lines1(txt)
	}, error = function(e) NULL)

	if (is.null(lines)) {
		msg <- paste0("Could not read file with encoding '", read_encoding, "'")
		if (verbose) cli::cli_warn(msg)
		return(list(
			lines             = NULL,
			encoding_detected = enc_info$encoding,
			encoding_declared = NA_character_,
			encoding_match    = NA,
			status            = "error",
			message           = msg
		))
	}

	if (length(lines) > 0) {
		lines[1] <- stringr::str_remove(lines[1], "^\\x{feff}")
	}

	declared_encoding <- NA_character_
	encoding_match    <- NA

	is_xml_type     <- !is.null(fileType) && tolower(fileType) %in% c("eaf", "exb")
	starts_with_xml <- length(lines) > 0 &&
					   stringr::str_detect(lines[1], "^\\s*<\\?xml")

	if (is_xml_type || starts_with_xml) {
		decl_match <- stringr::str_match(
			lines[1],
			"<\\?xml[^>]*encoding\\s*=\\s*[\"']([^\"']+)[\"']"
		)
		if (!is.na(decl_match[1, 2])) {
			declared_encoding <- decl_match[1, 2]
			encoding_match    <- helper_encodings_equivalent(enc_info$encoding,
															 declared_encoding)
			if (!isTRUE(encoding_match)) {
				msg <- sprintf(
					paste0("Declared XML encoding '%s' does not match detected ",
						   "encoding '%s'; file read as detected."),
					declared_encoding, enc_info$encoding
				)
				messages <- c(messages, msg)
				if (verbose) cli::cli_warn(msg)
			}
		}
	}

	status <- "ok"
	if (!is.null(expectedHeader) && length(expectedHeader) > 0) {
		for (h_idx in seq_along(expectedHeader)) {
			if (h_idx > length(lines)) {
				status   <- "error"
				msg      <- sprintf("Expected header line %d is missing", h_idx)
				messages <- c(messages, msg)
				if (verbose) cli::cli_warn(msg)
				break
			}
			if (!startsWith(lines[h_idx], expectedHeader[h_idx])) {
				status <- "error"
				msg <- sprintf(
					"Expected header line %d does not match (got: '%s...')",
					h_idx, substr(lines[h_idx], 1, 60)
				)
				messages <- c(messages, msg)
				if (verbose) cli::cli_warn(msg)
				break
			}
		}
	}

	list(
		lines             = lines,
		encoding_detected = enc_info$encoding,
		encoding_declared = declared_encoding,
		encoding_match    = encoding_match,
		status            = status,
		message           = paste(messages, collapse = "; ")
	)
}


#' Normalize an encoding name for synonym-tolerant comparison
#'
#' Normalizes encoding labels to a canonical family name by lowercasing,
#' removing hyphens/underscores/whitespace, and mapping common aliases.
#'
#' Examples:
#' \itemize{
#'   \item \code{"UTF-8"} -> \code{"utf8"}
#'   \item \code{"utf-8-BOM"} -> \code{"utf8"}
#'   \item \code{"ISO-8859-1"} -> \code{"latin1"}
#'   \item \code{"UTF-16LE"} -> \code{"utf16"}
#' }
#'
#' @param x Character string; encoding name. \code{NULL}, \code{NA} or
#'   empty string return \code{NA_character_}.
#'
#' @return Character string with the canonical family name, or
#'   \code{NA_character_}.
#'
#' @seealso \code{\link{helper_encodings_equivalent}}
#'
#' @export
helper_normalize_encoding_name <- function(x) {
	if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
	x <- tolower(x)
	x <- stringr::str_replace_all(x, "[-_\\s]", "")

	if (x %in% c("utf8", "utf8bom")) return("utf8")
	if (x %in% c("utf16", "utf16le", "utf16be", "utf16nobomlikely")) {
		return("utf16")
	}
	if (x %in% c("latin1", "iso88591", "windows1252", "cp1252")) {
		return("latin1")
	}
	x
}


#' Test whether two encoding labels refer to the same family
#'
#' Compares two encoding labels after normalization via
#' \code{\link{helper_normalize_encoding_name}}. Returns \code{NA} when
#' either input is missing or empty.
#'
#' @param a Character string; first encoding name.
#' @param b Character string; second encoding name.
#'
#' @return Logical or \code{NA}.
#'
#' @seealso \code{\link{helper_normalize_encoding_name}}
#'
#' @export
helper_encodings_equivalent <- function(a, b) {
	na <- helper_normalize_encoding_name(a)
	nb <- helper_normalize_encoding_name(b)
	if (is.na(na) || is.na(nb)) return(NA)
	identical(na, nb)
}
