context("Testing PoDCall::podcallHistogram")

test_that("Whether histogram is created correctly",{

    ## Path to data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataPath)

    ## Read in threshold table calculated from the data
    data("thrTable")
    thresholdTable <- thrTable

    ## Select a well and channel to plot
    well <- names(amplitudeData)[1]
    ch <- 1 # target channel

    ## Create plot
    plot <- podcallHistogram(amplitudeData[[well]][,ch],
                            thr=thresholdTable[well, "thr_target"],
                            channel=ch,
                            plotId=paste0(well, " Ch", ch))

    expect_is(plot, class=c("gg", "ggplot"))
    expect_equal(nrow(plot$data$ddr), nrow(amplitudeData[[well]][,ch]))


})

