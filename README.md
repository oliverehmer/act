# act

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://cranlogs.r-pkg.org/badges/grand-total/act)](https://cran.r-project.org/package=act)
[![CRAN version](https://www.r-pkg.org/badges/version/act)](https://cran.r-project.org/package=act)


## Aligned Corpus Toolkit (act) for R
The `Aligned Corpus Toolkit` (act) is a R package that is designed for linguists that work with time aligned transcription data. It offers functions to 
* import and export various annotation file formats ('ELAN' .eaf, 'EXMARaLDA .exb and 'Praat' .TextGrid files), 
* create print transcripts in the style of conversation analysis, 
* search transcripts (span searches across multiple annotations, search in normalized annotations, make concordances etc.), 
* export and re-import search results (.csv and 'Excel' .xlsx format), 
* create cuts for the search results (print transcripts, audio/video cuts using 'FFmpeg')
* create video sub titles in 'Subrib title' .srt format, 
* modify the data in a corpus (search/replace, delete, filter etc.), 
* interact with 'Praat' using 'Praat'-scripts (e.g. to open a search result in Praat), 
* interact with 'ELAN' (currently opening a search result in ELAN), and 
* exchange data with the 'rPraat' package. 

The package is itself written in R and may be expanded by other users.

## Basic Information
License: GPL-3

Author: Oliver Ehmer

Email: oliver.ehmer@romanistik.uni-freiburg.de

Website: [http://www.oliverehmer.de](http://www.oliverehmer.de)

Package website: [here](http://www.romanistik.uni-freiburg.de/ehmer/digitalhumanities.html).

CRAN site: [here](https://cran.r-project.org/package=act)

## How to cite
Creating the act package took a lot of time and effort. Please cite it when you publish research.

Ehmer, Oliver (2021). act: Aligned Corpus Toolkit. R package version 1.2.2. https://cran.r-project.org/package=act

## Installation
To install the package in R use the following commands.

Install from CRAN:

```
install.packages("act")
```

Install the development version from GitHub:

```
install.packages("remotes")
remotes::install_github("oliverehmer/act")
```

Load the package:
```
library(act)
```

## FFmpeg (audio/video cuts, stills, burnt-in subtitles and timecode)
Cuts, stills and burnt-in text need [FFmpeg](https://ffmpeg.org/) together with `ffprobe`. act finds them in the PATH or in the usual installation folders; a different location can be set with the option `act.path.ffmpeg`.

FFmpeg must include the filters `ass` (subtitles, built with libass) and `drawtext` (timecode, built with freetype). Without them clips and stills without text still work, but every export with subtitles or a timecode fails.

### Mac (Homebrew)
The Homebrew formula `ffmpeg` is built **without** these filters. Install `ffmpeg-full` instead and link it, so that it is the `ffmpeg` in the PATH:

```
brew uninstall ffmpeg
brew install ffmpeg-full
brew link --force ffmpeg-full
```

act also finds `ffmpeg-full` when it is not linked: it prefers an FFmpeg with both filters over one without.

On a Mac with Apple silicon, remove old Intel builds from `/usr/local/bin` (`ffmpeg`, `ffprobe`): without Rosetta they no longer start ("Bad CPU type").

### Windows and Linux
On Windows use a "full" build: `winget install Gyan.FFmpeg`, or the release-full build from [gyan.dev](https://www.gyan.dev/ffmpeg/builds/) with its `bin` folder in the PATH (then restart R). The FFmpeg packages of the common Linux distributions usually include libass and freetype.

### Check
```
ffmpeg -hide_banner -filters | grep -E " (ass|drawtext) "
```
must list both filters. In R, `act::helper_ffmpeg_check()` reports whether FFmpeg runs and which of these filters are missing (attribute `filters_missing`); the attribute `hint` holds the install commands for the running system.

## Supported annotation tool formats
* [Praat `.TextGrid`](https://www.fon.hum.uva.nl/praat/)
* [ELAN `.eaf`](https://archive.mpi.nl/tla/elan)
* [Exmaralda `.exb`](https://exmaralda.org/en/)

## Related R packages 
An example data set including anntoation and media files is available:
* Download a ZIP file at [GitHub] (https://github.com/oliverehmer/act_examplecorpus)

You might be interested in the following R packages, that functionally overlap with the act package.
* `ExmaraldaR` on [GitHub](https://github.com/TimoSchuer/ExmaraldaR)
* `FRelan` on [GitHub](https://github.com/langdoc/FRelan)
* `phonfieldwork` on [CRAN](https://cran.r-project.org/package=phonfieldwork) and [GitHub](https://github.com/ropensci/phonfieldwork)
* `rPraat` on [CRAN](https://cran.r-project.org/package=rPraat) and [GitHub](https://github.com/bbTomas/rPraat)
* `textgRid` on [CRAN](https://cran.r-project.org/package=textgRid)