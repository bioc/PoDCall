context("Testing PoDCall::podcallScatterplot")

test_that("Whether scatter plot is created correctly",{

    ## Path to data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataPath, skipLines=0)

    ## Read in threshold table calculated from the data
    data("thrTable")
    thresholdTable <- thrTable

    ## Select a well and channel to plot
    well <- names(amplitudeData)[1]
    ch <- 1 # target channel

    plot <- podcallScatterplot(amplitudeData[[well]][,ch],
                               thr=thresholdTable[well, "thr_target"],
                               channel=ch,
                               plotId=paste0(well, " Ch", ch))

    expect_is(plot, class=c("gg", "ggplot"))
    expect_equal(nrow(plot$data$ddr), nrow(amplitudeData[[well]][,ch]))
    expect_is(plot$data$col, class="factor")
})
