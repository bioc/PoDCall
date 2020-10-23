context("Testing PoDCall::importSampleSheet")

test_that("Whether sample sheet file is imported correctly",{
    ## Path to sample sheet file
    sampleSheetPath <- system.file("extdata", "Sample_names.csv",
                                    package="PoDCall")
    ## Path to corresponding data files
    dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataPath)

    ## Read sample sheet
    sampleSheet <- importSampleSheet(sampleSheet=sampleSheetPath,
                                    well_id=names(amplitudeData))

    ## Test returned object
    expect_is(sampleSheet, class="data.frame")
    expect_equal(colnames(sampleSheet),
                c("well_id", "sample_id", "target_assay", "ctrl_assay"))
    expect_equal(sampleSheet[,"well_id"], names(amplitudeData))

})
