# Internal helpers for resolving and naming docx template paths.
# Not exported; cross-package access via act:::.resolve_docx_templates.

# Resolves and validates a layout's docx template paths.
# Returns a named character vector with cleaned, existing paths.
# - Drops NA / empty entries
# - Falls back to the act-internal default template if nothing remains
# - Errors if any explicitly set path does not exist
# - Ensures every entry has a name (uses suffix-from-diff for unnamed entries)
.resolve_docx_templates <- function(l) {
	templates <- l@docx.template.path
	templates <- templates[!is.na(templates) & nzchar(templates)]

	if (length(templates) == 0) {
		default <- system.file("extdata", "docx", "template_transcript.docx", package = "act")
		if (!file.exists(default)) {
			cli::cli_abort("Unable to find {.arg template_transcript.docx}. Please reinstall {.pkg act}.")
		}
		templates <- c(default = default)
	}

	missing_files <- templates[!file.exists(templates)]
	if (length(missing_files) > 0) {
		cli::cli_abort("Template file(s) not found: {.path {missing_files}}")
	}

	# ensure names: prefer existing names, derive from filename diff otherwise
	nms <- names(templates)
	if (is.null(nms)) nms <- rep("", length(templates))
	needs_name <- !nzchar(nms)
	if (any(needs_name)) {
		derived <- .derive_template_suffixes(templates)
		nms[needs_name] <- derived[needs_name]
	}
	names(templates) <- nms

	templates
}

# Derives a short suffix per template path based on the differing
# part of the basenames (after stripping common prefix and extension).
# Returns "" for length-1 inputs.
.derive_template_suffixes <- function(templates) {
	if (length(templates) <= 1) return("")
	base_names <- tools::file_path_sans_ext(basename(templates))
	chars <- strsplit(base_names, "")
	min_len <- min(vapply(chars, length, integer(1)))
	prefix_len <- 0L
	if (min_len > 0L) {
		for (ci in seq_len(min_len)) {
			if (length(unique(vapply(chars, `[`, character(1), ci))) == 1L) {
				prefix_len <- ci
			} else {
				break
			}
		}
	}
	suffixes <- character(length(templates))
	for (i in seq_along(base_names)) {
		diff_part <- substr(base_names[i], prefix_len + 1L, nchar(base_names[i]))
		diff_part <- gsub("^_+|_+$", "", diff_part)
		if (nchar(diff_part) > 15L || !nzchar(diff_part)) {
			diff_part <- paste0("template", formatC(i, width = 2, flag = "0"))
		}
		suffixes[i] <- diff_part
	}
	suffixes
}
