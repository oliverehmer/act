#' Formats time as HH:MM:SS,mmm 
#' 
#' @param t Double; time in seconds.
#' @param digits Integer; number of digits. 
#' @param addHrsMinSec Logical; if \code{TRUE} 'hrs' 'min' 'sec' will be used instead of ':'.
#' @param addTimeInSeconds Logical; if \code{TRUE} time value in seconds will be shown, too.
#'
#' @return Character string.
#' @export
#'
#' @examples 
#' library(act)
#' 
#' 
#' helper_format_time(12734.2322345)
#' helper_format_time(2734.2322345)
#' helper_format_time(34.2322345)
#' helper_format_time(0.2322345)
#' 
#' helper_format_time(12734.2322345, addHrsMinSec=TRUE)
#' helper_format_time(2734.2322345, addHrsMinSec=TRUE)
#' helper_format_time(34.2322345, addHrsMinSec=TRUE)
#' helper_format_time(0.2322345, addHrsMinSec=TRUE)
#' 
#' helper_format_time(12734.2322345, digits=3)
#' helper_format_time(2734.2322345, digits=3)
#' helper_format_time(34.2322345, digits=3)
#' helper_format_time(0.2322345, digits=3)
#' 
#' helper_format_time(12734.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_format_time(2734.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_format_time(34.2322345, addHrsMinSec=TRUE, digits=3)
#' helper_format_time(0.2322345, addHrsMinSec=TRUE, digits=3)
#' 
#' helper_format_time(12734.2322345, addHrsMinSec=TRUE, addTimeInSeconds=TRUE)
#' helper_format_time(2734.2322345, addHrsMinSec=TRUE, addTimeInSeconds=TRUE)
#' helper_format_time(34.2322345, addHrsMinSec=TRUE, addTimeInSeconds=TRUE)
#' helper_format_time(0.2322345, addHrsMinSec=TRUE, addTimeInSeconds=TRUE)
#' 
#' helper_format_time(12734.2322345, addHrsMinSec=TRUE, digits=3, addTimeInSeconds=TRUE)
#' helper_format_time(2734.2322345, addHrsMinSec=TRUE, digits=3, addTimeInSeconds=TRUE)
#' helper_format_time(34.2322345, addHrsMinSec=TRUE, digits=3, addTimeInSeconds=TRUE)
#' helper_format_time(0.2322345, addHrsMinSec=TRUE, digits=3, addTimeInSeconds=TRUE)
#' 
#' 
helper_format_time <- function (t,
								digits=1,
								addHrsMinSec=FALSE, 
								addTimeInSeconds=FALSE) {

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
        f <- sprintf("%0.fhrs %02.fmin %02.fsec", h, m, s)
		
		if (digitsSTR!="") {
			f <- paste(f," ", digitsSTR, sep="")
		}
		
		if (addTimeInSeconds) {
			f <- paste(f, " (=",round(t,3)," sec)", sep="")
		}
	} else {
		f <- sprintf("%02.f:%02.f:%02.f", h, m, s)

		if (digitsSTR!="") {
			f <- paste(f, digitsSTR ,sep=",")
		}
		
		if (addTimeInSeconds) {
			f <- paste(f, " (=",round(t,digits)," sec)", sep="")
		}
	}

	return(f)
}
