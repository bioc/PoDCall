## code to prepare `THRESHOLDS` dataset goes here

# Path to data files
dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")

# Path to sample sheet
sampleSheet <- system.file("extdata", "Sample_names.csv", package="PoDCall")

# Set thresholds and calculate results
thrTable <- podcall_ddpcr(dataDirectory=dataPath,
                          sampleSheetFile=sampleSheet)

usethis::use_data(thrTable, compress="xz")
