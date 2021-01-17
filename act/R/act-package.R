#' Aligned Corpus Toolkit
#' @description The Aligned Corpus Toolkit (act) is designed for linguists that work with time aligned transcription data. It offers functions to import and export various annotation file formats ('ELAN' .eaf, 'EXMARaLDA .exb and 'Praat' .TextGrid files), create print transcripts in the style of conversation analysis, search transcripts (span searches across multiple annotations, search in normalized annotations, make concordances etc.), export and re-import search results (.csv and 'Excel' .xlsx format), create cuts for the search results (print transcripts, audio/video cuts using 'FFmpeg' and video sub titles in 'Subrib title' .srt format), modify the data in a corpus (search/replace, delete, filter etc.), interact with 'Praat' using 'Praat'-scripts, and exchange data with the 'rPraat' package. The package is itself written in R and may be expanded by other users.
#'  
#' @section act functions:
#' ...
#' @md
#' 
#' @section Package options:
#' The package has numerous options that change the internal workings of the package.
#' Please see \code{act::options_show} and the information given there. 
#'  
#' @docType package
#' @name act
#' 
#' @example inst/examples/act-package.R
#' 
NULL
