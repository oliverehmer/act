#' Add annotation to transcript with overlap handling
#'
#' Adds a single annotation (interval or point) to a transcript object.
#' Automatically checks for overlapping annotations and handles them based on
#' the overwrite parameter. Always ensures no invalid overlaps exist.
#'
#' @param x Transcript object.
#' @param tierName Character string; name of the tier (exact name, not regex).
#' @param startsec Numeric; start time in seconds.
#' @param endsec Numeric; end time in seconds.
#' @param content Character string; content of the annotation. Default `""`.
#' @param content.norm Character string; normalized content. Default `""`.
#' @param overwrite Logical; if \code{FALSE}, throws error when overlap exists.
#'   If \code{TRUE}, trims/splits existing overlapping annotations. Default \code{FALSE}.
#'
#' @return Transcript object with added annotation.
#'
#' @details
#' ## Overlap Handling
#' 
#' The function always checks for overlapping annotations in the specified tier:
#' 
#' **When \code{overwrite = FALSE} (default):**
#' - If overlap exists → Error with \code{stop()}
#' - If no overlap → Insert new annotation
#' 
#' **When \code{overwrite = TRUE}:**
#' - Existing overlapping annotations are trimmed/split:
#'   - Left remainder: \code{[old_start, new_start]} (if \code{old_start < new_start})
#'   - Right remainder: \code{[new_end, old_end]} (if \code{new_end < old_end})
#'   - Middle part is replaced by new annotation
#' - New annotation is inserted
#' 
#' ## Automatic Operations
#' - Assigns unique annotation ID
#' - Sets all char.* fields to NA (filled by fulltext operations)
#' - Maintains proper data.frame structure
#' - Updates modification time and history
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(act)
#' 
#' # Add annotation (error if overlap exists)
#' t <- annotations_add(
#'   x = examplecorpus@transcripts[[1]],
#'   tierName = "status",
#'   startsec = 10.5,
#'   endsec = 15.2,
#'   content = "o0.s0"
#' )
#' 
#' # Add annotation and overwrite overlaps
#' t <- annotations_add(
#'   x = examplecorpus@transcripts[[1]],
#'   tierName = "status",
#'   startsec = 10.5,
#'   endsec = 15.2,
#'   content = "new content",
#'   overwrite = TRUE
#' )
#' }
annotations_add <- function(
    x,
    tierName,
    startsec,
    endsec,
    content = "",
    content.norm = "",
    overwrite = FALSE
) {
  
  # ===== PARAMETER VALIDATION =====
  
  .assert_transcript(x, arg = "x", missing = missing(x))
  
  if (missing(tierName)) {
    cli::cli_abort("Parameter {.arg tierName} is missing.")
  }
  
  if (missing(startsec)) {
    cli::cli_abort("Parameter {.arg startsec} is missing.")
  }
  
  if (missing(endsec)) {
    cli::cli_abort("Parameter {.arg endsec} is missing.")
  }
  
  # Check if tier exists
  if (!(tierName %in% x@tiers$name)) {
    cli::cli_abort("Tier {.val {tierName}} does not exist in transcript. Use {.fn tiers_add} first.")
  }
  
  # Validate time range
  if (startsec >= endsec) {
    cli::cli_abort("Parameter {.arg startsec} must be less than {.arg endsec}.")
  }
  
  # ===== CHECK FOR OVERLAPS =====
  
  # Find overlapping annotations in the same tier
  overlaps <- x@annotations[
    x@annotations$tierName == tierName &
    !(x@annotations$endsec <= startsec | x@annotations$startsec >= endsec),
  ]
  
  has_overlaps <- nrow(overlaps) > 0
  
  # ===== HANDLE OVERLAPS =====
  
  if (has_overlaps) {
    if (!overwrite) {
      # Error on overlap when overwrite = FALSE
      cli::cli_abort(c(
        "Cannot add annotation [{startsec}-{endsec}] in tier {.val {tierName}}: overlapping annotations exist.",
        "Existing annotations:",
        stats::setNames(
          sprintf("[%.2f-%.2f]: '%s'", overlaps$startsec, overlaps$endsec, overlaps$content),
          rep("*", nrow(overlaps))
        ),
        "i" = "Set {.arg overwrite} to {.val TRUE} to replace overlapping content."
      ))
    } else {
      # Trim/split overlapping annotations when overwrite = TRUE
      
      annotations_to_add <- list()
      annotations_to_remove <- c()
      
      for (i in seq_len(nrow(overlaps))) {
        overlap_annot <- overlaps[i, ]
        annotations_to_remove <- c(annotations_to_remove, overlap_annot$annotationID)
        
        # Check if there's a left remainder
        if (overlap_annot$startsec < startsec) {
          # Keep left part: [old_start, new_start]
          left_remainder <- overlap_annot
          left_remainder$endsec <- startsec
          annotations_to_add[[length(annotations_to_add) + 1]] <- left_remainder
        }
        
        # Check if there's a right remainder
        if (overlap_annot$endsec > endsec) {
          # Keep right part: [new_end, old_end]
          right_remainder <- overlap_annot
          right_remainder$startsec <- endsec
          annotations_to_add[[length(annotations_to_add) + 1]] <- right_remainder
        }
      }
      
      # Remove overlapping annotations
      x@annotations <- x@annotations[
        !(x@annotations$annotationID %in% annotations_to_remove),
      ]
      
      # Add trimmed remainders
      if (length(annotations_to_add) > 0) {
        for (remainder in annotations_to_add) {
          # Assign new annotation ID
          new_id <- if (nrow(x@annotations) > 0) {
            max(x@annotations$annotationID, na.rm = TRUE) + 1
          } else {
            1L
          }
          remainder$annotationID <- new_id
          
          # Ensure remainder has same columns as x@annotations (in case structure changed)
          if (!identical(names(remainder), names(x@annotations))) {
            # Add missing columns
            for (col in setdiff(names(x@annotations), names(remainder))) {
              remainder[[col]] <- NA
            }
            # Reorder to match
            remainder <- remainder[, names(x@annotations), drop = FALSE]
          }
          
          # Add to annotations
          x@annotations <- rbind(x@annotations, remainder)
        }
      }
    }
  }
  
  # ===== ADD NEW ANNOTATION =====
  
  # Get next annotation ID
  new_annot_id <- if (nrow(x@annotations) > 0) {
    max(x@annotations$annotationID, na.rm = TRUE) + 1
  } else {
    1L
  }
  
  # Create new annotation row
  new_row <- data.frame(
    annotationID = as.integer(new_annot_id),
    tierName = as.character(tierName),
    startsec = as.double(startsec),
    endsec = as.double(endsec),
    content = as.character(content),
    content.norm = as.character(content.norm),
    char.orig.bytime.start = NA_integer_,
    char.orig.bytime.end = NA_integer_,
    char.norm.bytime.start = NA_integer_,
    char.norm.bytime.end = NA_integer_,
    char.orig.bytier.start = NA_integer_,
    char.orig.bytier.end = NA_integer_,
    char.norm.bytier.start = NA_integer_,
    char.norm.bytier.end = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # Add any additional columns that exist in x@annotations but not in new_row
  extra_cols <- setdiff(names(x@annotations), names(new_row))
  for (col in extra_cols) {
    new_row[[col]] <- NA
  }
  
  # Reorder columns to match x@annotations
  new_row <- new_row[, names(x@annotations), drop = FALSE]
  
  # Add to annotations
  x@annotations <- rbind(x@annotations, new_row)
  
  # Sort annotations by time and tier
  x@annotations <- x@annotations[order(x@annotations$startsec, x@annotations$tierName), ]
  rownames(x@annotations) <- NULL
  
  # ===== UPDATE METADATA =====

  # Add to history
  x@history[[length(x@history) + 1]] <- list(
    modification = "annotations_add",
    systime = Sys.time(),
    tierName = tierName,
    startsec = startsec,
    endsec = endsec,
    annotationID = new_annot_id,
    overwrite = overwrite,
    overlaps_handled = has_overlaps
  )
  
  return(x)
}
