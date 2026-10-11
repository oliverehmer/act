#' Helper: Resolve the path to the FFmpeg executable
#'
#' Returns the path to the FFmpeg executable using the following cascade:
#' \enumerate{
#'   \item Option \code{act.path.ffmpeg}, if it is set to a concrete path (any value other than the default \code{"ffmpeg"}). This path is returned as it is, without a test.
#'   \item The first executable that runs, checked in this order: \code{Sys.which("ffmpeg")} (the PATH of the R session), then the known installation locations (\code{/opt/homebrew/bin}, \code{/opt/homebrew/opt/ffmpeg-full/bin}, \code{/usr/local/bin}, \code{/usr/bin}).
#'   \item The first of these executables that exists, even if it does not run (\link{helper_ffmpeg_check} then reports why).
#'   \item The literal \code{"ffmpeg"} as last resort (resolution is left to the shell).
#' }
#'
#' Whether an executable runs is tested once per session (\code{ffmpeg -version}).
#'
#' @return Character string; path to the FFmpeg executable or \code{"ffmpeg"}.
#'
#' @seealso \link{helper_ffprobe_path}, \link{helper_ffmpeg_check}
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
	.ffmpeg_executable_pick("ffmpeg", Sys.which("ffmpeg"))
}


#' Helper: Resolve the path to the ffprobe executable
#'
#' Returns the path to the ffprobe executable. ffprobe is installed together
#' with FFmpeg and lives in the same directory. The cascade is:
#' \enumerate{
#'   \item \code{ffprobe} next to the FFmpeg executable resolved by \link{helper_ffmpeg_path}.
#'   \item The first executable that runs: \code{Sys.which("ffprobe")}, then the known installation locations (\code{/opt/homebrew/bin}, \code{/opt/homebrew/opt/ffmpeg-full/bin}, \code{/usr/local/bin}, \code{/usr/bin}).
#'   \item The first of these executables that exists, even if it does not run.
#'   \item The literal \code{"ffprobe"} as last resort (resolution is left to the shell).
#' }
#'
#' @return Character string; path to the ffprobe executable or \code{"ffprobe"}.
#'
#' @seealso \link{helper_ffmpeg_path}, \link{helper_ffmpeg_check}
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
	.ffmpeg_executable_pick("ffprobe", Sys.which("ffprobe"))
}


#' Helper: Check whether FFmpeg or ffprobe can run
#'
#' Runs the executable resolved by \link{helper_ffmpeg_path} (or
#' \link{helper_ffprobe_path}) with \code{-version} and reports whether it
#' works. An executable can exist and still not run, e.g. an FFmpeg built for
#' Intel processors on a Mac with Apple silicon that has no Rosetta. The
#' result is remembered for the session.
#'
#' For FFmpeg the check also lists the filters act needs for burnt-in text
#' that are missing: \code{ass} (subtitles, needs libass) and \code{drawtext}
#' (timecode, needs freetype). FFmpeg still counts as working without them
#' (clips, sound and stills without text are possible). On a Mac the plain
#' Homebrew formula \code{ffmpeg} is built without both; install
#' \code{ffmpeg-full} instead (see the README of act).
#'
#' @param executable Character string; \code{"ffmpeg"} (default) or \code{"ffprobe"}.
#'
#' @return \code{TRUE} or \code{FALSE}, with the attributes \code{path}
#'   (the executable that was tested), \code{reason} (why it does not run;
#'   \code{NA} when it runs) and \code{filters_missing} (character vector of
#'   the missing filters, empty when none is missing or when the executable is
#'   ffprobe or does not run).
#'
#' @seealso \link{helper_ffmpeg_path}, \link{helper_ffprobe_path}
#'
#' @export
#'
#' @examples
#' ok <- act::helper_ffmpeg_check()
#' if (!ok) print(attr(ok, "reason"))
#'
helper_ffmpeg_check <- function(executable = c("ffmpeg", "ffprobe")) {
	executable <- match.arg(executable)
	path <- if (identical(executable, "ffmpeg")) helper_ffmpeg_path() else helper_ffprobe_path()
	if (identical(path, executable) && !nzchar(Sys.which(executable))) {
		reason <- if (identical(executable, "ffmpeg"))
			"not found (not in the PATH and not in the usual locations) - install FFmpeg or set option act.path.ffmpeg"
		else "not found (not next to FFmpeg, not in the PATH)"
		return(structure(FALSE, path = path, reason = reason, filters_missing = character(0)))
	}
	res <- .ffmpeg_runs(path)
	missing <- if (isTRUE(res$ok) && identical(executable, "ffmpeg")) .ffmpeg_filters_missing(path) else character(0)
	structure(res$ok, path = path, reason = res$reason, filters_missing = missing)
}


# ===== INTERNAL HELPERS =====

.FFMPEG_EXEC_CACHE <- new.env(parent = emptyenv())

.ffmpeg_executable_pick <- function(name, found) {
	# ffmpeg-full is keg-only in Homebrew: without "brew link" it lives only in its opt folder
	candidates <- c(unname(found), file.path(c("/opt/homebrew/bin", "/opt/homebrew/opt/ffmpeg-full/bin",
		"/usr/local/bin", "/usr/bin"), name))
	candidates <- unique(candidates[nzchar(candidates) & file.exists(candidates)])
	for (candidate in candidates) {
		if (isTRUE(.ffmpeg_runs(candidate)$ok)) return(candidate)
	}
	if (length(candidates)) candidates[1] else name
}

# One -version call per path and session: helper_ffmpeg_path() is called for
# every command that is built.
.ffmpeg_runs <- function(path) {
	hit <- .FFMPEG_EXEC_CACHE[[path]]
	if (!is.null(hit)) return(hit)
	res <- tryCatch({
		out <- processx::run(path, "-version", error_on_status = FALSE, timeout = 10)
		if (identical(out$status, 0L)) list(ok = TRUE, reason = NA_character_)
		else list(ok = FALSE, reason = paste0("exits with status ", out$status, " on -version"))
	}, error = function(e) {
		message <- conditionMessage(e)
		system_error <- stringr::str_match(message, "system error \\d+, ([^)]+)\\)")[, 2]
		reason <- if (!is.na(system_error)) system_error else stringr::str_squish(message)
		if (stringr::str_detect(reason, stringr::fixed("Bad CPU type", ignore_case = TRUE)))
			reason <- paste0(reason, " - this FFmpeg is built for another processor (e.g. an Intel build on a Mac with Apple silicon, without Rosetta); install it again")
		list(ok = FALSE, reason = reason)
	})
	assign(path, res, envir = .FFMPEG_EXEC_CACHE)
	res
}

# The filters act uses for burnt-in text. The Homebrew formula "ffmpeg" has
# neither: the export then fails at the first subtitle or timecode.
.FFMPEG_TEXT_FILTERS <- c("ass", "drawtext")

.ffmpeg_filters_missing <- function(path) {
	key <- paste0("filters|", path)
	hit <- .FFMPEG_EXEC_CACHE[[key]]
	if (!is.null(hit)) return(hit)
	out <- tryCatch(processx::run(path, c("-hide_banner", "-filters"), error_on_status = FALSE, timeout = 10)$stdout,
		error = function(e) NA_character_)
	if (is.na(out)) return(character(0))
	filter_names <- stringr::str_match(strsplit(out, "\n")[[1]], "^\\s*\\S+\\s+(\\S+)\\s")[, 2]
	res <- setdiff(.FFMPEG_TEXT_FILTERS, filter_names)
	assign(key, res, envir = .FFMPEG_EXEC_CACHE)
	res
}
