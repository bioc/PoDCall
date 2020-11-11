context("Testing PoDCall::podcallThresholds")

test_that("Whether threshold table is returned correctly",{

    ## Path to data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataPath)

    ## Read sample sheet
    thresholdTable <- podcallThresholds(plateData=amplitudeData, B=100)

    expect_is(thresholdTable, class="data.frame")
    expect_equal(nrow(thresholdTable), length(amplitudeData))
    expect_equal(rownames(thresholdTable), names(amplitudeData))

    ## tot_droplets in returned table should be equal to total number of data
    ## points in data files
    totDr <- vapply(amplitudeData, function(x) nrow(x), numeric(1))
    expect_equal(sum(totDr), sum(thresholdTable[,"tot_droplets"]))
    expect_equal(rownames(thresholdTable), names(amplitudeData))

})
