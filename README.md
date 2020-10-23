
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PoDCall

<!-- badges: start -->

<!-- badges: end -->

PoDCall reads files from Quantasoft containing amplitude values from one
or several runs of ddPCR, sets thresholds for each individual well and
calculates concentrations and normalized concentration for each well.The
resulting threshold table can optionally be written to file
automatically. Optionally, PoDCall also creates plots per channel per
well that are saved as .pdf-files in the same directory as the
corresponding threshold table result file.

## Installation

PoDCall requires some packages to be installed, and if any required
packages are not yet installed, the installation of PoDCall should take
care of it (You will be prompted to install the packages that are
missing).

The released version of PoDCall can (not yet) be installed from
[CRAN](https://CRAN.R-project.org), but for now it can be installed
locally from a source file with a function from the remotes package:

``` r

install.packages("remotes")
remotes::install_local("path/to/package")
```

After installing PoDCall and the required packages, PoDCall can be
loaded with:

``` r

library(PoDCall)
```

## Example / Usage

To use PoDCall, call the function mixturemodel\_ddpcr:

``` r
threshold_table <- podcall_ddpcr(data_directory="path/to/data")
```

Where “path/to/data” (note: trailing “/” in path will cause error) is
the path to the directory that contains amplitude files from a well
plate, in which the files have names that end with wellID\_amplitude.csv

#### Optional arguments

  - B is the number of permutations used by the likelihood ratio test
    (LRT) which decides the number of components in the model fitted
    from the data. Default value B=200

  - Q is a parameter used for calling a droplet an outlier, in case of a
    unimodal distribution. The defalt value is Q=7.5, which has has been
    determined through cell line experiments.

  - refwell is the well used as reference when calculating the shift in
    baseline between wells. By default refwell=1, but can be changed in
    cases where the first well is not suited to be used. (More on this
    in the application note?)

  - If channel 2 is not in use, set ch2 = FALSE to avoid error caused by
    empty channel 2 column. Default is TRUE.

  - The user can choose to let PoDCall save the results table to a
    csv-file by setting results\_to\_file = TRUE. (default:
    reuslts\_to\_file = FALSE)

  - The user can choose to make plots that are written to file by
    setting plots = TRUE. (default: plots = FALSE)

## PoDCall functions

`podcall_ddpcr()` is the main wrapper function that returns a table with
the results of PoDCall to the user, but the functions that read the
amplitude data from file, set thresholds and make plots are also
available to be used individually.

  - `importAmplitudeData()` reads .csv-files with amplitude data
    outputted from QuantaSoft and store the data in a list, one data
    frame per well.

  - `podcallThresholds()` takes a list of dataframes, one for each well,
    and sets individual thresholds for each channel of each well. It
    returns a table with threholds, number of positive droplets,
    concentrations etc.

  - `podcallScatterplots()` takes the threshold and amplitude values
    corresponding to a channel of a well, and returns a scatterplot.

  - `podcallHistogram()` takes the threshold and amplitude values
    corresponding to a channel of a well, and returns a histogram.

  - `podcallMultiplot()` takes a list of dataframes, one per well, and
    returns faceted scatterplots suitable for comparing wells.

For more details about the functions and their arguments, see the help
file by using `?<funcion>`

## PoDCall shiny application

PoDCall does also include a shiny application that launches in a web
browser. To start the app:

``` r
podcallShiny()
```

## Application note

For more details about how the thresholds are set, see the application
note: insert url to publication here
