#' Fill tier section with content
#'
#' Fills annotations in a specific tier and time range with content.
#' By default, only fills empty annotations (does not overwrite existing content).
#'
#' @param x Transcript object.
#' @param tier_pattern Character string; regex pattern to identify target tier(s).
#' @param content Character string; content to insert.
#' @param startsec Numeric; start time in seconds.
#' @param endsec Numeric; end time in seconds.
#' @param overwrite Logical; if \code{TRUE}, overwrites existing content.
#'   If \code{FALSE}, only fills empty annotations. Default \code{FALSE}.
#' @param create_if_missing Logical; if \code{TRUE} and no annotation exists in
#'   the time range, creates a new annotation spanning the entire range.
#'   Default \code{TRUE}.
#' @param perl Logical; use Perl-compatible regex for tier_pattern. Default \code{TRUE}.
#'
#' @return Transcript object with modified annotations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(act)
#' 
#' # Fill status tier with "o0.s0"
#' t <- annotations_fill_tier_section(
#'   x = examplecorpus@transcripts[[1]],
#'   tier_pattern = "(?i)status",
#'   content = "o0.s0",
#'   startsec = 10,
#'   endsec = 20
#' )
#' }
annotations_fill_tier_section <- function(
    x,
    tier_pattern,
    content,
    startsec,
    endsec,
    overwrite = FALSE,
    create_if_missing = TRUE,
    perl = TRUE
) {
  
  if (missing(x)) {
    cli::cli_abort("Transcript object in parameter {.arg x} is missing.")
  }

  if (!methods::is(x, "transcript")) {
    cli::cli_abort("Parameter {.arg x} needs to be a transcript object.")
  }

  if (missing(tier_pattern)) {
    cli::cli_abort("Parameter {.arg tier_pattern} is missing.")
  }

  if (missing(content)) {
    cli::cli_abort("Parameter {.arg content} is missing.")
  }

  if (missing(startsec)) {
    cli::cli_abort("Parameter {.arg startsec} is missing.")
  }

  if (missing(endsec)) {
    cli::cli_abort("Parameter {.arg endsec} is missing.")
  }
  
  # Find matching tier(s)
  matching_tiers <- grepl(tier_pattern, x@tiers$name, perl = perl)
  
  if (!any(matching_tiers)) {
    cli::cli_warn("No tier matching pattern {.val {tier_pattern}} found in transcript.")
    return(x)
  }
  
  # Use first matching tier
  tier_name <- x@tiers$name[matching_tiers][1]
  
  # Find annotations in this tier and time range
  in_tier_section <- x@annotations$tierName == tier_name &
                     x@annotations$startsec >= startsec &
                     x@annotations$endsec <= endsec
  
  if (!any(in_tier_section)) {
    # No annotations exist in this section
    if (create_if_missing) {
      # Create new annotation using annotations_add
      x <- annotations_add(
        x = x,
        tierName = tier_name,
        startsec = startsec,
        endsec = endsec,
        content = content,
        overwrite = overwrite
      )
    }
  } else {
    # Update existing annotations
    for (i in which(in_tier_section)) {
      existing <- x@annotations$content[i]
      
      if (overwrite || is.na(existing) || nchar(trimws(existing)) == 0) {
        x@annotations$content[i] <- content
      }
    }
    
    # Update modification time
    x@modification.systime <- Sys.time()
    
    # Add to history
    x@history[[length(x@history) + 1]] <- list(
      modification = "annotations_fill_tier_section",
      systime = Sys.time(),
      tier_pattern = tier_pattern,
      tier_name = tier_name,
      content = content,
      startsec = startsec,
      endsec = endsec,
      overwrite = overwrite,
      annotations_modified = sum(in_tier_section)
    )
  }
  
  return(x)
}
