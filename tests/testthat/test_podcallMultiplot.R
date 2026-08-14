context("Testing PoDCall::podcallMultiplot")

test_that("Whether comparison plot is created correctly",{

    ## Path to data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    ampData <- importAmplitudeData(dataPath, skipLines=0)
    ssPath <- system.file("extdata", "Sample_names.csv", package="PoDCall")
    sampleSheet <- importSampleSheet(sampleSheet=ssPath, well_id=names(ampData),
                                     software="QuantaSoft")

    ## Select wells and channel to plot
    wells <- names(ampData)[1:3] # First three wells from set

    ## Get relevant thresholds (see ?thrTable)
    data("thrTable")
    thr <- thrTable[wells, c("thr_target", "thr_ctrl")]

    ## Create plot
    plot <- podcallMultiplot(plateData=ampData[wells],
                             thresholds=thr,
                             channel="target",
                             sampleID=sampleSheet$sample_id[1:3],
                             colCh=1)

    ## Does function return a plot
    expect_is(plot, class=c("gg", "ggplot"))

    ## Is the number of plotted data points the same as in the data
    nrDatapoints <-
        sum(vapply(ampData[wells], function(x) nrow(x), numeric(1)))
    expect_equal(nrow(plot$data), nrDatapoints)

})
