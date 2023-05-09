context("Testing PoDCall::importAmplitudeData")

test_that("Whether importAmplitudeData return expected sized list",{
    path <- system.file("extdata", "Amplitudes", package="PoDCall")
    amplitudeData <- importAmplitudeData(dataDirectory=path, skipLines=0)

    list.files(path, pattern="_Amplitude.csv")

    expect_equal(length(amplitudeData),
                length(list.files(path, pattern="_Amplitude.csv")))
    expect_is(amplitudeData, class="list")

    })
