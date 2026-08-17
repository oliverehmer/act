#' Build a media table from file paths
#'
#' Builds the core media table (as stored in \code{transcript@media}) from a
#' vector of file paths. The generic act columns are set: \code{path},
#' \code{startsec} (0), \code{kind} ("full"), \code{type} ("video"/"audio"/
#' \code{NA} from the file extension). The remaining columns
#' (\code{source.path}, \code{clip.id}, \code{startsec.source},
#' \code{length.sec}, \code{video.width}, \code{video.height}, \code{video.fps})
#' are set to \code{NA} and filled later (e.g. via \code{media_metadata_read}).
#' Higher level packages may add further columns afterwards.
#'
#' @param paths Vector of character strings; media file paths.
#'
#' @return Data.frame; the core media table with one row per path.
#'
#' @seealso \link{media_assign}, \link{media_select}
#'
#' @export
#'
media_build <- function(paths) {
	if (length(paths) == 0) {
		return(.emptyMedia)
	}
	paths <- unname(paths)
	ext  <- tolower(tools::file_ext(paths))
	type <- ifelse(ext %in% tolower(options()$act.media.fileformats.video), "video",
			ifelse(ext %in% tolower(options()$act.media.fileformats.audio), "audio",
				   NA_character_))
	data.frame(
		path 			= paths,
		source.path 	= NA_character_,
		clip.id 		= NA_character_,
		startsec 		= 0,
		startsec.source = NA_character_,
		length.sec 		= NA_real_,
		video.width 	= NA_integer_,
		video.height 	= NA_integer_,
		video.fps 		= NA_real_,
		video.codec 		= NA_character_,
		video.bitrate 		= NA_real_,
		audio.sample.rate 	= NA_integer_,
		audio.channels 		= NA_integer_,
		audio.codec 		= NA_character_,
		audio.bitrate 		= NA_real_,
		container.format 	= NA_character_,
		file.size 			= NA_real_,
		volume.kind 		= NA_character_,
		volume.name 		= NA_character_,
		kind 			= "full",
		type 			= type,
		stringsAsFactors = FALSE)
}


#' Fill the read-derived columns of a media table from the files
#'
#' For each row whose metadata has not been read yet (\code{length.sec} is
#' \code{NA}, unless \code{force = TRUE}), reads the file via
#' \code{media_metadata_read} and fills \code{length.sec}, \code{source.path},
#' \code{clip.id} and the \code{video.*} columns. \code{startsec}/
#' \code{startsec.source} are only overwritten when the file actually carries an
#' offset (tag or timecode), so the \code{0} of full recordings is preserved.
#'
#' @param media Data.frame; a media table as built by \code{media_build}.
#' @param force Logical; if \code{TRUE} re-reads every row.
#'
#' @return Data.frame; the media table with read columns filled.
#'
#' @seealso \link{media_build}, \link{media_metadata_read}, \link{media_assign}
#'
#' @export
#'
media_metadata_fill <- function(media, force = FALSE) {
	if (nrow(media) == 0) return(media)
	if (is.null(media$volume.kind)) media$volume.kind <- NA_character_
	if (is.null(media$volume.name)) media$volume.name <- NA_character_
	need <- if (force) rep(TRUE, nrow(media)) else is.na(media$length.sec)
	for (i in which(need)) {
		r <- tryCatch(media_metadata_read(media$path[i]), error = function(e) NULL)
		if (is.null(r) || nrow(r) == 0) next
		media$length.sec[i]   <- r$length.sec[1]
		media$source.path[i]  <- r$source[1]
		media$clip.id[i]      <- r$clip.id[1]
		media$video.width[i]  <- r$video.width[1]
		media$video.height[i] <- r$video.height[1]
		media$video.fps[i]    <- r$video.fps[1]
		media$video.codec[i]       <- r$video.codec[1]
		media$video.bitrate[i]     <- r$video.bitrate[1]
		media$audio.sample.rate[i] <- r$audio.sample.rate[1]
		media$audio.channels[i]    <- r$audio.channels[1]
		media$audio.codec[i]       <- r$audio.codec[1]
		media$audio.bitrate[i]     <- r$audio.bitrate[1]
		media$container.format[i]  <- r$container.format[1]
		media$file.size[i]         <- r$file.size[1]
		media$volume.kind[i]       <- r$volume.kind[1]
		media$volume.name[i]       <- r$volume.name[1]
		if (!is.na(r$startsec[1])) {
			media$startsec[i]        <- r$startsec[1]
			media$startsec.source[i] <- r$startsec.source[1]
		}
	}
	media
}
