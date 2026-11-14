#' Fill tier section with content
#'
#' Fills annotations in a specific tier and time range with content.
#' By default, only fills empty annotations (does not overwrite existing content).
#'
#' Only the first tier whose name matches \code{tier_pattern} is filled; if
#' further tiers match, a warning names them.
#'
#' The annotations that lie within the section are filled. If none lies within
#' it but one annotation covers the whole section, that annotation is split at
#' the bounds of the section and the section gets the content when the
#' annotation is empty or \code{overwrite = TRUE} (the parts before and after
#' the section keep their content); an annotation with content stays as it is
#' when \code{overwrite = FALSE}, with a message. If no annotation lies within
#' or covers the section, a new one is created (\code{create_if_missing}).
#'
#' @param x Transcript object.
#' @param tier_pattern Character string; regex pattern to identify the target
#'   tier. If several tiers match, the first one is used.
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
  
  .assert_transcript(x, arg = "x", missing = missing(x))

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
  if (sum(matching_tiers) > 1) {
    others <- x@tiers$name[matching_tiers][-1]
    cli::cli_warn(c(
      "Pattern {.val {tier_pattern}} matches {sum(matching_tiers)} tiers; only the first, {.val {tier_name}}, is filled.",
      "i" = "Not filled: {.val {others}}."
    ))
  }
  
  # Find annotations in this tier and time range
  in_tier_section <- x@annotations$tierName == tier_name &
                     x@annotations$startsec >= startsec &
                     x@annotations$endsec <= endsec
  covering <- x@annotations$tierName == tier_name &
              x@annotations$startsec <= startsec &
              x@annotations$endsec >= endsec
  overlapping <- x@annotations$tierName == tier_name &
                 !(x@annotations$endsec <= startsec | x@annotations$startsec >= endsec)
  
  if (!any(in_tier_section) && any(covering)) {
    i <- which(covering)[1]
    existing <- x@annotations$content[i]
    empty <- is.na(existing) || nchar(trimws(existing)) == 0
    # with further overlapping annotations (a tier with overlaps) the split
    # would also cut those, so only overwrite = TRUE may do it
    if (overwrite || (empty && sum(overlapping) == 1)) {
      # overwrite = TRUE in annotations_add() only splits at the bounds: the
      # parts before and after the section keep their content
      x <- annotations_add(
        x = x,
        tierName = tier_name,
        startsec = startsec,
        endsec = endsec,
        content = content,
        overwrite = TRUE
      )
    } else if (empty) {
      x <- annotations_add(
        x = x,
        tierName = tier_name,
        startsec = startsec,
        endsec = endsec,
        content = content,
        overwrite = FALSE
      )
    } else {
      cli::cli_inform(c(
        "i" = "Tier {.val {tier_name}}: the annotation [{sprintf('%.2f-%.2f', x@annotations$startsec[i], x@annotations$endsec[i])}] covers the section [{startsec}-{endsec}] and has content; it is kept.",
        " " = "Set {.arg overwrite} to {.val TRUE} to replace it within the section."
      ))
    }
  } else if (!any(in_tier_section)) {
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
    
  }
  
  return(x)
}
