#' Formats time as HH:MM:SS,mmm 
#' 
#' @param t Double; time in seconds.
#' @param digits Integer; number of digits. 
#' @param addHrsMinSec Logical; if \code{TRUE} 'hrs' 'min' 'sec' will be used instead of ':'.
#' @param addSec Logical; if \code{TRUE} time value in seconds will be shown, too.
#' @param format Character or \code{NULL}; a named time format. If set, \code{digits}, \code{addHrsMinSec} and \code{addSec} are ignored. One of \code{"h:mm:ss.s"}, \code{"h:mm:ss.ss"}, \code{"h:mm:ss:ff"} (frames), \code{"mm:ss.ss"} (hours are dropped), \code{"s.s"}, \code{"s.ss"}. Fractions are truncated, not rounded.
#' @param fps Numeric; frames per second for \code{"h:mm:ss:ff"}.
#'
#' @return Character string.
#' @export
#'
#' @examples 
#' library(act)
#' 
#' 
#' helper_time_format(12734.2322345)
#' helper_time_format(2734.2322345)
#' helper_time_format(34.2322345)
#' helper_time_format(0.2322345)
#' 
#' helper_time_format(12734.2322345, addHrsMinSec=TRUE)
#' helper_time_format(2734.2322345, addHrsMinSec=TRUE)
#' helper_time_format(34.2322345, addHrsMinSec=TRUE)
#' helper_time_format(0.2322345, addHrsMinSec=TRUE)
#' 
#' helper_time_format(12734.2322345, digits=3)
#' helper_time_format(2734.2322345, digits=3)
#' helper_time_format(34.2322345, digits=3)
#' helper_time_format(0.2322345, digits=3)
#' 
#' helper_time_format(12734.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_time_format(2734.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_time_format(34.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_time_format(0.2322345, addHrsMinSec=TRUE, digits=3)
#' 
#' helper_time_format(12734.2322345, addHrsMinSec=TRUE, addSec=TRUE)
#' helper_time_format(2734.2322345, addHrsMinSec=TRUE, addSec=TRUE)
#' helper_time_format(34.2322345, addHrsMinSec=TRUE, addSec=TRUE)
#' helper_time_format(0.2322345, addHrsMinSec=TRUE, addSec=TRUE)
#' 
#' helper_time_format(12734.2322345, addHrsMinSec=TRUE, digits=3, addSec=TRUE)
#' helper_time_format(2734.2322345, addHrsMinSec=TRUE, digits=3, addSec=TRUE)
#' helper_time_format(34.2322345, addHrsMinSec=TRUE, digits=3, addSec=TRUE)
#' helper_time_format(0.2322345, addHrsMinSec=TRUE, digits=3, addSec=TRUE)
#' 
#' helper_time_format(83.45, format="h:mm:ss.ss")
#' helper_time_format(83.45, format="h:mm:ss:ff", fps=25)
#' 
helper_time_format <- function (t,
								digits=1,
								addHrsMinSec=FALSE, 
								addSec=FALSE,
								format=NULL,
								fps=25) {
	if (!is.null(format)) {
		return(.time_format_apply(t, format, fps))
	}

	digits <- max(0, as.integer(digits))
	t<-round(t, digits)

	h <- floor(t/3600)
	m <- t-(h*3600)
	m <- floor(m/60)
	s <- t-(h*3600)-(m*60)
	s <- as.integer(s)
	
	if (digits>0) {
		digitsSTR <- substr(format(round(t %% 1, digits), nsmall = digits) ,3,3+digits)
	} else {
		digitsSTR <-""
	}
	
	if (addHrsMinSec) {
        #f <- sprintf("%0.fhrs %02.fmin %02.fsec", h, m, s)
        f <- sprintf("%.0fhrs %02.0fmin %02.0fsec", h, m, s)
        
		if (digitsSTR!="") {
			f <- paste(f," ", digitsSTR, sep="")
		}
		
		if (addSec) {
			f <- paste(f, " (=",round(t, 3)," sec)", sep="")
		}
	} else {
		#f <- sprintf("%02.f:%02.f:%02.f", h, m, s)
		f <- sprintf("%.0f:%02.0f:%02.0f", h, m, s)

		if (digitsSTR!="") {
			f <- paste(f, digitsSTR ,sep=",")
		}
		
		if (addSec) {
			f <- paste(f, " (=",round(t, digits)," sec)", sep="")
		}
	}
	f <- stringr::str_replace_all(f, ",", ".")
	return(f)
}

#' Helper: Time formats
#'
#' Lists the time formats that \link{helper_time_format} accepts as
#' \code{format}.
#'
#' @return Character vector of format names.
#'
#' @export
#'
#' @examples
#' act::helper_time_formats_list()
helper_time_formats_list <- function() {
	c("h:mm:ss.s", "h:mm:ss.ss", "h:mm:ss:ff", "mm:ss.ss", "s.s", "s.ss")
}

.time_format_apply <- function(t, format, fps = 25) {
	format <- as.character(format)[1]
	if (!format %in% helper_time_formats_list()) {
		cli::cli_abort("Unknown time {.arg format} {.val {format}}. Use one of {.val {helper_time_formats_list()}}.")
	}
	t <- as.numeric(t)
	out <- rep(NA_character_, length(t))
	ok <- !is.na(t)
	if (!any(ok)) return(out)
	t <- pmax(0, t[ok])
	if (format == "h:mm:ss:ff") {
		fps <- suppressWarnings(as.numeric(fps)[1])
		if (is.na(fps) || fps <= 0) fps <- 25
		fps_int <- as.integer(round(fps))
		frames <- floor(t * fps + 1e-6)
		sec <- frames %/% fps_int
		out[ok] <- sprintf("%d:%02d:%02d:%02d", as.integer(sec %/% 3600), as.integer((sec %/% 60) %% 60),
		                   as.integer(sec %% 60), as.integer(frames %% fps_int))
		return(out)
	}
	tenths_digits <- if (format %in% c("h:mm:ss.s", "s.s")) 1L else 2L
	unit <- 10^tenths_digits
	units <- floor(t * unit + 1e-6)
	sec <- units %/% unit
	frac <- sprintf(paste0("%0", tenths_digits, "d"), as.integer(units %% unit))
	out[ok] <- switch(format,
		"h:mm:ss.s"  = ,
		"h:mm:ss.ss" = sprintf("%d:%02d:%02d.%s", as.integer(sec %/% 3600), as.integer((sec %/% 60) %% 60),
		                       as.integer(sec %% 60), frac),
		"mm:ss.ss"   = sprintf("%02d:%02d.%s", as.integer((sec %/% 60) %% 60), as.integer(sec %% 60), frac),
		"s.s"        = ,
		"s.ss"       = sprintf("%d.%s", as.integer(sec), frac))
	out
}
