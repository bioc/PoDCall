context("Testing PoDCall::podcallMultiplot")

test_that("Whether comparison plot is created correctly",{

    ## Path to data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataPath)

    ## Select wells and channel to plot
    wells <- names(amplitudeData)[1:3] # First three wells from set
    ch <- 1 # target channel

    ## Get relevant thresholds (see ?thrTable)
    data("thrTable")
    thr <- thrTable[wells, c("thr_target", "thr_ctrl")]

    ## Create plot
    plot <- podcallMultiplot(plateData=amplitudeData[wells],
                             thresholds=thr,
                             channel=ch)

    ## Does function return a plot
    expect_is(plot, class=c("gg", "ggplot"))

    ## Is the number of plotted data points the same as in the data
    nrDatapoints <-
        sum(vapply(amplitudeData[wells], function(x) nrow(x), numeric(1)))
    expect_equal(nrow(plot$data), nrDatapoints)

})
