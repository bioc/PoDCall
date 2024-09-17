
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PoDCall

<!-- badges: start -->

<!-- badges: end -->

# Introduction

PoDCall (Positive Droplet Caller) [M. Jeanmougin et
al](https://doi.org/10.1093/bioinformatics/btac766) is a package that
aims to provide a robust calling of positive droplets in DNA methylation
droplet digital PCR (ddPCR) experiments performed on the Bio-Rad
platform. PoDCall provides functions that reads files exported from
QuantaSoft or QX Manager containing amplitudes from a run of ddPCR (one
96 well plate), sets thresholds for both channels of each individual
well and calculates concentrations and normalized concentration for each
well.The resulting threshold table can optionally be written to file
automatically by the main workflow function. PoDCall also offers
functionality for plotting, both individual wells and multiple well
plots. Plots for individual wells can be made and saved as .pdf-files as
part of the main workflow function, or by calling the various plotting
functions individually.

## Gaussian Mixture Models

DdPCR experiments generate a mixture of droplets, positive droplets
which contain the target that will be amplified, and negative droplets
that does not contain the target and show no amplification. PoDCall
relies on fitting Gaussian mixture models to set thresholds for each
individual well that will be used to classify the droplets as either
positive or negative. For more details on the concepts of PoDCall, see
the application note.

## Input Data

The input data is .csv-files exported from ‘QuantaSoft’ or ‘QX Manager,
and each file contains the amplitude values of droplets from one well of
a 96 well plate. The first two columns of the files should have headers
’Ch1 Amplitude’ and ‘Ch2 Amplitude’. To read in data, use the function
importAmplitudeData, which will read all amplitude files in the
directory given as argument. Each file will be stored as a named data
frame in a list, where the name will be the well ID. For this reason,
all raw data files in the directory given as argument should be from the
same 96 well plate to avoid well coordinate duplicates.

# Installation

PoDCall requires some packages to be installed, and if any required
packages are not yet installed, the installation of PoDCall should take
care of it (you will be prompted to install the packages that are
missing).

The released version of PoDCallcan be installed from
[BIOCONDUCTOR](http://bioconductor.org/), from GitHub or from a source
file.

``` r
## Install PoDCall from Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("PoDCall")

## Install PoDCall from GitHub
install.packages("devtools")
devtools::install_github("HansPetterBrodal/PoDCall")

## Install PoDCall from source file
install.packages("remotes")
remotes::install_local("path/to/PoDCall_x.y.z.tar.gz")
```

After installing PoDCall and the required packages, PoDCall can be
loaded with:

``` r

library(PoDCall)
```

# Example / Usage

One step of setting thresholds includes a random sampling of droplets to
greatly reduce running time. To ensure reproducible results it is
recommended to set a seed using `set.seed()`. To run the full PoDCall
workflow, call the function `podcallDdpcr()`:

``` r
## Run PoDCall
thresholdTable <- podcallDdpcr(dataDirectory="path/to/data/", 
                                software="QuantaSoft")
```

Where “path/to/data/” is the path of the directory that contains
amplitude files from a well plate, in which the files have names that
end with "\_wellID\_amplitude.csv“.”software" is the software that was
used to export the data (amplitude) files and the sample sheet. Must be
either “QuantaSoft” or “QX Manager”. Since the different software
versions format amplitude files and sample sheet differently, it is
important to set the correct value as argument to ensure that data and
sample sheet is read correctly.

## Optional arguments

The following arguments have default values, but can be given other
values if desired. For example to write results to file, which is
disabled by default.

### sampleSheetFile

Path to sample sheet file. Must be a .csv file exported from QuantaSoft
and must include the following columns: Well, Sample, TargetType and
Target. The entries in the column TargetType must be either ‘Ch1Unknown’
or Ch2Unknown, and is used to extract rows with information from either
channel 1 or channel 2. An example file has been included in the
package, which can be found using `system.file("extdata",
"Sample_names.csv", package="PoDCall")`

### B

The number of permutations used by the likelihood ratio test (LRT) which
decides the number of components in the model fitted from the data.
Default value `B=200`.

### Q

A parameter used for calling outliers. Q is multiplied with the
interquartile range of the distribution of amplitude values to determine
if droplets of extreme amplitude values are to be considered outliers.
The default value is `Q=9`, which has been determined through cell line
experiments and testing. A higher Q will generally result in a higher or
more strict threshold. Q provides an option to adjust how thresholds are
set in a systematic and reproducible way. It is recommended to try a few
different values and visually inspect the results.

### refwell

The well used as reference when calculating the shift in baseline
between wells. By default `refwell=1`, but can be changed in cases where
the first well is not suited to be used.

### nrChannels

If there is no control channel used, set nrChannels=1 to indicate only
target channel used. Default value is 2.

### targetChannel

When a multiplexed set up is used (more than a single target channel),
what channel to be analysed is the target channel. Default
targetChannel=1.

### controlChannel

The channel used for control assay. Default controlChannel=2.

### software

The Bio-Rad software the data was exported from. Must be either
“QuantaSoft” or “QX Manager”, the latter being default. This is due to
unequal number of lines that needs to be skipped in the raw amplitude
data files.

### resultsToFile

The user can choose to let PoDCall save the results table as a .csv-file
by setting `resultsToFile=TRUE` (default: `resultsToFile = FALSE`). When
resultsToFile is set to TRUE, a results directory will be created where
the result file will be saved. The results directory will have the same
name as the data directory with "\_results" added:
"path/to/data\_results/

### plots

The user can choose to make plots that are written to file by setting
`plots=TRUE` (default: `plots=FALSE`). Plots will be saved to the
results directory created when `resultsToFile=TRUE`. The results
directory will also be created if only `plots=TRUE`.

### resPath

Optional argument to specify a directory for writing results file(s) to
other than the results directory created by default. Requires
`resultsToFile=TRUE`.

## Threshold table columns

The table that is returned when running `podcall_ddpcr()` contains
columns with more or less self-explanatory column names, and well ID
(well coordinates) as rownames:

### sample\_id

If a sample sheet file is provided, this will have the sample ID from
the sample sheet. Otherwise empty

### target\_ch

The channel used for target assay

### thr\_target

the threshold set for selected target channel

### thr\_ctrl

The threshold set for control channel

### pos\_dr\_target

The number of positive droplets in selected target channel

### pos\_dr\_ctrl

The number of positive droplets in control channel

### tot\_droplets

Number of droplets.

### c\_target

Concentration of target, calculated by the formula
\(-\log\dfrac{\dfrac{\text{neg_drop_tar}}{\text{tot_droplets}}}{0.000851}\)
where neg\_drop\_tar is number of negative droplets in channel 1
(target).

### c\_ctrl

Concentration of control, calculated by the formula
\(-\log\dfrac{\dfrac{\text{neg_drop_ctrl}}{\text{tot_droplets}}}{0.000851}\)
where neg\_drop\_ctrl is number of negative droplets in channel 2
(control).

### c\_norm\_4Plex

Normalized concentration with 4Plex as control, calculated by the
formula \(\dfrac{\text{c_target}}{\text{c_ctrl}}\cdot400\)

### c\_norm\_sg

Normalized concentration with single gene as control, calculated by the
formula \(\dfrac{\text{c_target}}{\text{c_ctrl}}\cdot100\)

### q

The value used for Q

### target\_assay

The assay used for target channel, provided via sample sheet.

### ctrl\_assay

The assay used for control channel, provided via sample sheet.

### ref\_well

The well used as reference well for setting threshold.

# PoDCall functions

`podcallDdpcr()` is the main wrapper function that returns a table with
the results of PoDCall to the user. This function uses a set of
functions that read the amplitude data from file, set thresholds and
make plots. All functions involved can be used individually should the
user only want to use some of the functionality of PoDCall. Also see
help files for more details about the functions and their arguments.

## `importAmplitudeData()`

Reads .csv-files with amplitude data outputted from QuantaSoft or QX
Manager and store the data in a list, one data frame per well. Each
element in the list will be named using it’s well ID (coordinate) of the
96 well plate that the sample belong to.

``` r
## Path to example data files included in PoDCall
path <- system.file("extdata", "Amplitudes/", package="PoDCall")

## Read in data files
dataList <- importAmplitudeData(dataDirectory=path, skipLines=0)
str(dataList)
#> List of 5
#>  $ A04:'data.frame': 18739 obs. of  2 variables:
#>   ..$ Ch1: num [1:18739] 940 971 971 976 985 ...
#>   ..$ Ch2: num [1:18739] 11795 7868 8377 10007 9523 ...
#>  $ B04:'data.frame': 16933 obs. of  2 variables:
#>   ..$ Ch1: num [1:16933] 980 995 1002 1007 1014 ...
#>   ..$ Ch2: num [1:16933] 9524 7999 7686 7799 9510 ...
#>  $ D04:'data.frame': 11713 obs. of  2 variables:
#>   ..$ Ch1: num [1:11713] 1042 1070 1094 1112 1112 ...
#>   ..$ Ch2: num [1:11713] 7826 9934 7698 7605 7743 ...
#>  $ D05:'data.frame': 12642 obs. of  2 variables:
#>   ..$ Ch1: num [1:12642] 1045 1057 1063 1068 1079 ...
#>   ..$ Ch2: num [1:12642] 9722 7752 9103 7716 7738 ...
#>  $ H05:'data.frame': 19638 obs. of  2 variables:
#>   ..$ Ch1: num [1:19638] 1043 1094 1098 1104 1119 ...
#>   ..$ Ch2: num [1:19638] 7231 7063 7161 6863 7416 ...
```

## `importSampleSheet()`

Reads a .csv-file outputted from QuantaSoft or QX Manager to get
information about the samples: Sample name/id, Assay for target and
control.

``` r
## Path to example files included in PoDCall
path <- system.file("extdata", "Sample_names.csv", package="PoDCall")

## Select wells to get information for
well_id <- c("A04", "B04", "D04")

## Read in sample sheet information for selected wells
sampleSheet <- importSampleSheet(sampleSheet=path, well_id=well_id, 
                                software="QuantaSoft")
print(sampleSheet)
#>   well_id sample_id target_assay ctrl_assay
#> 1     A04    SW1463          VIM   new4Plex
#> 2     B04     SW403          VIM   new4Plex
#> 3     D04     SW480          VIM   new4Plex
```

## `podcallThresholds()`

Takes a list of data frames, one for each well, as argument and sets
individual thresholds for each channel of each well. It returns a table
with thresholds, number of positive droplets, concentrations etc. The
number of permutations for likelihood ratio test is by default set to
`B=400` as a compromise between run time and stability of the results.
The parameter for calling outliers is by default set to `Q=9`. Higher Q
means more conservative (higher) thresholds, lower Q will result in over
all lower thresholds.

``` r
## Path to example data files included in PoDCall
path <- system.file("extdata", "Amplitudes/", package="PoDCall")

## Read in data files
ampData <- importAmplitudeData(dataDirectory=path, skipLines=0)

## Calculate thresholds, metrics, concentrations
thresholdTable <- podcallThresholds(plateData=ampData)
print(thresholdTable)
```

## `podcallChannelPlot()`

Takes the threshold and amplitude values corresponding to a channel of a
well as arguments, calls functions that makes scatter plot and histogram
and draws a plot with both.

``` r
## Read in data and threshold table
path <- system.file("extdata", "Amplitudes/", package="PoDCall")
ampData <- importAmplitudeData(dataDirectory=path, skipLines=0)
data("thrTable")
thresholdTable <- thrTable

## Select channel and well to plot
ch <- 1 # target channel
well_id <- names(ampData)[1] # First well in list

## Plot title
plotTitle <- paste0(well_id, ", Ch", ch)

## Create plot
podcallChannelPlot(channelData=ampData[[well_id]][,ch],
                    thr=thresholdTable[well_id, "thr_target"],
                    channel=ch,
                    plotId=plotTitle)
```

<img src="man/figures/README-channel-plot-1.png" width="100%" />

## `podcallScatterplot()`

Takes the threshold and amplitude values corresponding to a channel of a
well as argument and returns a scatter plot.

``` r
## Read in data and threshold table
path <- system.file("extdata", "Amplitudes/", package="PoDCall")
ampData <- importAmplitudeData(dataDirectory=path, skipLines=0)
thresholdTable <- thrTable

## Select channel and well to plot
ch <- 1 # target channel
well_id <- names(ampData)[1] # First well in list

## Plot title
plotTitle <- paste0(well_id, ", Ch", ch)

## Create plot
podcallScatterplot(channelData=ampData[[well_id]][,ch],
                    thr=thresholdTable[well_id, "thr_target"],
                    channel=ch,
                    plotId=plotTitle)
```

<img src="man/figures/README-scatter-plot-1.png" width="100%" />

## `podcallHistogram()`

Takes the threshold and amplitude values corresponding to a channel of a
well as argument, and returns a histogram.

``` r
## Read in data and threshold table
path <- system.file("extdata", "Amplitudes/", package="PoDCall")
ampData <- importAmplitudeData(dataDirectory=path, skipLines=0)
thresholdTable <- thrTable

## Select channel and well to plot
ch <- 1 # target channel
well_id <- names(ampData)[1] # First well in list

## Plot title
plotTitle <- paste0(well_id, ", Ch", ch)

## Create plot
podcallHistogram(channelData=ampData[[well_id]][,ch],
                thr=thresholdTable[well_id, "thr_target"],
                channel=ch,
                plotId=plotTitle)
```

<img src="man/figures/README-plot-histogram-1.png" width="100%" />

## `podcallMultiplot()`

takes a list of data frames with amplitude data, one per well, and their
respective thresholds as arguments and returns faceted scatter plots
suitable for comparing wells.

``` r
## Read in data and threshold table
path <- system.file("extdata", "Amplitudes/", package="PoDCall")
ampData <- importAmplitudeData(dataDirectory=path, skipLines=0)
thresholdTable <- thrTable

## Channel to plot
ch <- "target"

## Create comparison plot
podcallMultiplot(plateData=ampData,
                thresholds=thresholdTable[names(ampData),], 
                channel=ch, colCh=1)
```

<img src="man/figures/README-comparison-plot-1.png" width="100%" />

# PoDCall shiny application

PoDCall does also include an application powered by shiny that launches
in a web browser. The application provides a user friendly and
interactive interface to the functionality of PoDCall. To start the app:

``` r
podcallShiny()
```

# PodCall example data

There are some amplitude files and a sample sheet included in the
package that are intended to be used to run examples and to try out the
functionality of PoDCall. The data files are from a real experiment
performed with cell line samples. There is also a threshold table
computed from the example data included. PoDCall takes a few minutes to
run due to bootstrapping, and this table is used in examples for
functions where threshold is an argument.

## Cell Line Amplitude Data

The cell line amplitude data files can be found in the “extdata”
subdirectory of the package directory and can be found using
`system.file()`:

``` r
## Path to files
path <- system.file("extdata", "Amplitudes/", package="PoDCall")

## List files
list.files(path)
#> [1] "VIM_4Plex_A04_Amplitude.csv" "VIM_4Plex_B04_Amplitude.csv"
#> [3] "VIM_4Plex_D04_Amplitude.csv" "VIM_4Plex_D05_Amplitude.csv"
#> [5] "VIM_4Plex_H05_Amplitude.csv"
```

The control assay used for the samples in the example data files is an
assay developed in-house called 4Plex [H. Pharo et
al](http://dx.doi.org/10.1186/s13148-018-0456-5).

## Calculated Threshold Table

The already calculated threshold table is instantly available when
PoDCall is loaded, and is available as an object called `thrTable`. See
`?thrTable` for help file with documentation on the table.

``` r
## The threshold table
thrTable
#>       sample_id thr_target thr_ctrl pos_dr_target pos_dr_ctrl tot_droplets
#> A04      SW1463       2761     9127          2479       12906        18739
#> B04       SW403       2739     8632           660        7471        16933
#> D04       SW480       2863     8489            44        8348        11713
#> D05 IVDZ_bisulf       2818     8473          1675        6584        12642
#> H05         NTC       2823     7910             0           0        19638
#>     c_target c_ctrl c_norm_4Plex c_norm_sg q target_assay ctrl_assay ref_well
#> A04  166.700 1371.0        48.64     12.16 9          VIM   new4Plex      A04
#> B04   46.720  683.9        27.33     6.831 9          VIM   new4Plex      A04
#> D04    4.423 1466.0        1.207    0.3017 9          VIM   new4Plex      A04
#> D05  167.000  864.4        77.28     19.32 9          VIM   new4Plex      A04
#> H05    0.000    0.0       No DNA    No DNA 9          VIM   new4Plex      A04
```

# Session info

Here is the output of `sessionInfo()` on the system on which this
document was compiled

``` r
sessionInfo()
#> R version 4.1.0 (2021-05-18)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 7 x64 (build 7601) Service Pack 1
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=Norwegian (Bokmål)_Norway.1252 
#> [2] LC_CTYPE=Norwegian (Bokmål)_Norway.1252   
#> [3] LC_MONETARY=Norwegian (Bokmål)_Norway.1252
#> [4] LC_NUMERIC=C                              
#> [5] LC_TIME=Norwegian (Bokmål)_Norway.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] PoDCall_1.9.2
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_1.1.2     xfun_0.29            shinyjs_2.1.0       
#>  [4] purrr_0.3.4          colorspace_2.0-2     vctrs_0.6.5         
#>  [7] generics_0.1.2       htmltools_0.5.2      yaml_2.2.2          
#> [10] utf8_1.2.2           rlang_1.1.4          pillar_1.9.0        
#> [13] later_1.3.0          glue_1.6.1           DBI_1.1.2           
#> [16] bit64_4.0.5          lifecycle_1.0.4      stringr_1.4.0       
#> [19] munsell_0.5.0        gtable_0.3.0         htmlwidgets_1.5.4   
#> [22] LaplacesDemon_16.1.6 evaluate_0.15        labeling_0.4.2      
#> [25] knitr_1.37           tzdb_0.2.0           fastmap_1.1.0       
#> [28] httpuv_1.6.5         parallel_4.1.0       fansi_1.0.2         
#> [31] highr_0.9            Rcpp_1.0.8           xtable_1.8-4        
#> [34] readr_2.1.2          scales_1.3.0         promises_1.2.0.1    
#> [37] DT_0.20              diptest_0.76-0       vroom_1.5.7         
#> [40] farver_2.1.0         bit_4.0.4            mime_0.12           
#> [43] gridExtra_2.3        ggplot2_3.3.5        hms_1.1.1           
#> [46] digest_0.6.29        stringi_1.7.6        rlist_0.4.6.2       
#> [49] dplyr_1.0.8          shiny_1.7.1          grid_4.1.0          
#> [52] cli_3.1.1            tools_4.1.0          magrittr_2.0.2      
#> [55] tibble_3.1.6         crayon_1.5.0         pkgconfig_2.0.3     
#> [58] ellipsis_0.3.2       data.table_1.14.2    assertthat_0.2.1    
#> [61] rmarkdown_2.11       rstudioapi_0.13      R6_2.5.1            
#> [64] mclust_5.4.9         compiler_4.1.0
```
