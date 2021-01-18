# act

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![](http://cranlogs.r-pkg.org/badges/grand-total/act)](https://CRAN.R-project.org/package=act)
[![CRAN version](http://www.r-pkg.org/badges/version/act)](https://cran.r-project.org/package=act)


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

Package website: [here](http://www.romanistik.uni-freiburg.de/ehmer/digitalhumanities.html/).

CRAN site: [here](https://cran.r-project.org/web/packages/act/index.html)

## How to cite
Creating the act package took a lot of time and effort. Please cite it when you publish reasearch.

Ehmer, Oliver (2021). act: Aligned Corpus Toolkit. R package version 1.0. https://CRAN.R-project.org/package=act

## Installation
To install the package in R use the following commands.

Install from CRAN:

```
install.packages("act")
```

Install the development version from GitHub:

```
install.packages("remotes")
remotes::install_github("oliverehmer/act/act")
```
Please note that the double 'act/act' is necessary.

Load the package:
```
library(act)
```

## Supported Annotation tool formats
* [Praat `.TextGrid`](http://www.fon.hum.uva.nl/praat/)
* [ELAN `.eaf`](https://archive.mpi.nl/tla/elan)
* [Exmaralda `.exb`](https://exmaralda.org/en/)

## Related R packages 
You might be interested in the following R packages, that functionally overlap with the act package.
* `ExmaraldaR` on [GitHub](https://github.com/TimoSchuer/ExmaraldaR)
* `FRelan` on [GitHub](https://github.com/langdoc/FRelan)
* `phonfieldwork` on [CRAN](https://cran.r-project.org/web/packages/phonfieldwork/index.html) and [GitHub](https://github.com/ropensci/phonfieldwork)
* `rPraat` on [CRAN](https://cran.r-project.org/package=rPraat) and [GitHub](https://github.com/bbTomas/rPraat)
* `textgRid` on [CRAN](https://cran.r-project.org/package=textgRid)