#' Helper: Resolve the path to the FFmpeg executable
#'
#' Returns the path to the FFmpeg executable using the following cascade:
#' \enumerate{
#'   \item Option \code{act.path.ffmpeg}, if it is set to a concrete path (any value other than the default \code{"ffmpeg"}).
#'   \item \code{Sys.which("ffmpeg")}, if FFmpeg is found in the PATH of the R session.
#'   \item Known installation locations (\code{/opt/homebrew/bin}, \code{/usr/local/bin}, \code{/usr/bin}).
#'   \item The literal \code{"ffmpeg"} as last resort (resolution is left to the shell).
#' }
#'
#' @return Character string; path to the FFmpeg executable or \code{"ffmpeg"}.
#'
#' @seealso \link{helper_ffprobe_path}
#'
#' @export
#'
#' @examples
#' act::helper_ffmpeg_path()
#'
helper_ffmpeg_path <- function() {
	opt <- getOption("act.path.ffmpeg", "")
	if (!is.null(opt) && nzchar(opt) && !identical(opt, "ffmpeg")) {
		return(opt)
	}
	found <- Sys.which("ffmpeg")
	if (nzchar(found)) {
		return(unname(found))
	}
	for (candidate in c("/opt/homebrew/bin/ffmpeg", "/usr/local/bin/ffmpeg", "/usr/bin/ffmpeg")) {
		if (file.exists(candidate)) {
			return(candidate)
		}
	}
	"ffmpeg"
}


#' Helper: Resolve the path to the ffprobe executable
#'
#' Returns the path to the ffprobe executable. ffprobe is installed together
#' with FFmpeg and lives in the same directory. The cascade is:
#' \enumerate{
#'   \item \code{ffprobe} next to the FFmpeg executable resolved by \link{helper_ffmpeg_path}.
#'   \item \code{Sys.which("ffprobe")}, if ffprobe is found in the PATH of the R session.
#'   \item The literal \code{"ffprobe"} as last resort (resolution is left to the shell).
#' }
#'
#' @return Character string; path to the ffprobe executable or \code{"ffprobe"}.
#'
#' @seealso \link{helper_ffmpeg_path}
#'
#' @export
#'
#' @examples
#' act::helper_ffprobe_path()
#'
helper_ffprobe_path <- function() {
	ffmpeg <- helper_ffmpeg_path()
	ffmpeg_dir <- dirname(ffmpeg)
	if (nzchar(ffmpeg_dir) && ffmpeg_dir != ".") {
		file_name <- if (.Platform$OS.type == "windows") "ffprobe.exe" else "ffprobe"
		candidate <- file.path(ffmpeg_dir, file_name)
		if (file.exists(candidate)) {
			return(candidate)
		}
	}
	found <- Sys.which("ffprobe")
	if (nzchar(found)) {
		return(unname(found))
	}
	"ffprobe"
}
