context("Testing PoDCall::podcallChannelPlot")

test_that("Whether channel plot is created correctly",{

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
    plot <- podcallChannelPlot(amplitudeData[[well]][,ch],
                                thr=thresholdTable[well, "thr_target"],
                                channel=ch,
                                plotId=paste0(well, " Ch", ch))

    expect_is(plot, class=c("gtable", "gTree", "grob", "gDesc"))

})
