#' Normalize transcriptions
#'
#' Normalizes the contents of transcriptions in a corpus object using a normalization matrix.
#' Function returns a corpus object with normalized transcription and updates the original corpus object passed as argument to x.
#'
#' If the normalization matrix is missing or empty (0 rows), a warning is issued
#' and the corpus object is returned unchanged.
#'
#' @param x Corpus object.
#' @param transcriptNames Vector of character strings; Names of the transcripts for which you want to search media files; leave empty if you want to search media for all transcripts in the corpus object.
#' @param forceUpdate Logical; If \code{TRUE} transcripts will be normalized in any case, if \code{FALSE} transcripts will be only normalized if there was a modification to the transcript since the last normalization.
#'
#' @export
#'
#' @examples
#' library(act)
#'
#' examplecorpus <- act::transcripts_update_normalization(x=examplecorpus)
#'
transcripts_update_normalization <- function(x,
											 transcriptNames           = NULL,
											 forceUpdate               = FALSE){
	#=== check data
	.assert_corpus(x, missing = missing(x))
	if (is.null(x@transcripts))     {
		cli::cli_warn("No transcripts found in corpus object x.")
		return(x)
	}
	if (length(x@transcripts)==0) 	{
		cli::cli_warn("No transcripts found in corpus object x.")
		return(x)
	}

	#=== matrix check: missing or empty -> warn and skip
	if (is.null(x@normalization.matrix) ||
		!is.data.frame(x@normalization.matrix) ||
		nrow(x@normalization.matrix) == 0) {
		cli::cli_warn(paste0(
			"Normalization matrix is missing or empty; skipping normalization. ",
			"Set it via 'x@normalization.matrix <- act::matrix_load(path=...)'."))
		return(x)
	}
	act_replacementMatrix <- x@normalization.matrix

	#=== check matrix
	if ("search" %in% colnames(act_replacementMatrix)==FALSE) {	cli::cli_abort("Column {.arg search} is missing in normalization matrix. The matrix needs to contain colums {.arg search} and {.arg replace}")}
	if ("replace" %in% colnames(act_replacementMatrix)==FALSE){	cli::cli_abort("Column {.arg replace} is missing in normalization matrix. The matrix needs to contain colums {.arg search} and {.arg replace}")	}
	#replace NA by empty strings
	act_replacementMatrix$replace[is.na(act_replacementMatrix$replace)] <- ""

	#=== create named vector for replacement
	mymatrix 		<- as.character(act_replacementMatrix$replace)
	names(mymatrix) <- act_replacementMatrix$search

	#=== check if the matrix works
	out <- tryCatch(
		{
			#This is the 'try' part
			stringr::str_replace_all("test string", mymatrix)
		},
		error=function(cond) {
			#this is the error part
			NULL
		}
	)
	if (is.null(out)) 						{	cli::cli_abort("Normalization matrix seems to be containing invalid regular expressions.")		}

	#=== if no filter is set, process all transcripts
	if (is.null(transcriptNames)) {transcriptNames <- names(x@transcripts)}

	#=== compute current signature per transcript and decide which need update
	# Sort ann by startsec + tierName before hashing so the signature is
	# independent of the annotation row order (otherwise sorting that happens
	# inside transcripts_update_fulltexts() would invalidate signatures
	# between sessions).
	current_sigs <- vapply(transcriptNames, function(i) {
		ann <- x@transcripts[[i]]@annotations
		if (is.null(ann) || nrow(ann) == 0) {
			content <- character(0)
		} else {
			ann <- ann[order(ann$startsec, ann$tierName), ]
			content <- as.character(ann$content)
		}
		digest::digest(list(content, x@normalization.matrix), algo = "xxhash64")
	}, character(1))

	needs_update <- vapply(seq_along(transcriptNames), function(k) {
		if (forceUpdate) return(TRUE)
		tr <- x@transcripts[[transcriptNames[k]]]
		stored_sig <- tr@normalization.signature
		if (length(stored_sig) == 0) return(TRUE)
		stored_sig != current_sigs[k]
	}, logical(1))
	transcriptNames.update <- transcriptNames[needs_update]
	current_sigs.update    <- current_sigs[needs_update]

	if (length(transcriptNames.update) == 0) {
		return(x)
	}

	helper_progress_set("Updating normalization", length(transcriptNames.update))
	chunk_starts <- seq(1L, length(transcriptNames.update), by = 50L)
	for (chunk_start in chunk_starts) {
		chunk_idx <- chunk_start:min(chunk_start + 49L, length(transcriptNames.update))

		#=== collect all content vectors across transcripts into one big vector
		contents.list <- lapply(transcriptNames.update[chunk_idx], function(i) {
			ann <- x@transcripts[[i]]@annotations
			if (is.null(ann) || nrow(ann) == 0) return(character(0))
			as.character(ann$content)
		})
		n_per <- lengths(contents.list)

		all.content <- unlist(contents.list, use.names = FALSE)
		if (is.null(all.content)) all.content <- character(0)

		#=== single vectorized normalization pass per chunk
		# Regex patterns are compiled once per chunk instead of once per transcript.
		if (length(all.content) > 0) {
			all.norm <- stringr::str_to_lower(all.content)
			all.norm <- stringr::str_replace_all(all.norm, mymatrix)
			all.norm <- stringr::str_trim(all.norm, side = "both")
		} else {
			all.norm <- character(0)
		}

		#=== split normalized vector back into per-transcript slices
		ends   <- cumsum(n_per)
		starts <- c(1L, utils::head(ends, -1) + 1L)

		for (k in seq_along(chunk_idx)) {
			helper_progress_tick()
			i <- transcriptNames.update[chunk_idx[k]]
			if (n_per[k] > 0) {
				x@transcripts[[i]]@annotations$content.norm <- all.norm[starts[k]:ends[k]]
			}
			x@transcripts[[i]]@normalization.signature <- current_sigs.update[chunk_idx[k]]
		}
	}

	return(x)
}
