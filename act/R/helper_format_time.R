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
helper_format_time <- function (t,
								digits=0,
								addHrsMinSec=FALSE, 
								addTimeInSeconds=FALSE) {
	
	h <- floor(t/3600)
	m <- t-(h*3600)
	m <- floor(m/60)
	s <- t-(h*3600)-(m*60)
	s <- round(s,0)
	digits <- max(0,as.integer(digits))
	if (digits>0) {
		digitsSTR <- substr(as.character(round(t %% 1,3)),3,6)
	} else {
		digitsSTR <-""
	}
	if (addHrsMinSec) {
		if (t<60) {
			f <- paste(s,"sec", sep="")
		} else {
			if (h>0) {
#				f <- sprintf("%02.fhrs %02.fmin %02.fsec", h, m, s)
								f <- sprintf("%0.fhrs %02.fmin %02.fsec", h, m, s)
				
							} else if (m>0) {
				f <- sprintf("%2.fmin %02.fsec", m, s)
			}
	
		}
		
		if (digitsSTR!="") {
			f <- paste(f," ", digitsSTR, sep="")
		}
		
		if (addTimeInSeconds) {
			f <- paste(f, " (=",round(t,3)," sec)", sep="")
		}
	} else {
		
		if (t<60) {
			f <- paste(s,"", sep="")
		} else {
			if (h>0) {
				f <- sprintf("%02.f:%02.f:%02.f", h, m, s)
			} else if (m>0) {
				f <- sprintf("%2.f:%02.f", m, s)
			}
		}
		
		if (digitsSTR!="") {
			f <- paste(f, digitsSTR ,sep=",")
		}
		
		if (addTimeInSeconds) {
			f <- paste(f, " (=",round(t,digits)," sec)", sep="")
		}
	}

	return(f)
	
	#paste(paste(formatC(time %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
	#			,":", formatC(time %/% 60 %% 60, width = 2, format = "d", flag = "0")
	#			,":", formatC(time %% 60, width = 2, format = "d", flag = "0")
	#			, ",", substr(as.character(round(time %% 1,3)),3,6)
	#			,sep='' 
	#)
}
