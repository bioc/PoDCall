# The function importAmplitudeData reads amplitude files exported from
# Quantasoft and returns the data as a list. Each element of the returned list
# corresponds to the data from one file (= one well), and names the element with
# the well-ID corresponding to the file

#' importAmplitudeData
#'
#' @param dataDirectory Path to directory containing Quantasoft amplitude files
#'     from one 96 well plate. Since well coordinates are used as identifiers,
#'     files in this directory should all be from the same 96 well plate.
#'     Furthermore, there should be no other files than the amplitude files from
#'     a well plate in the directory.
#' @param skipLines Number of lines to skip in amplitude data files. Must be 0
#'     or 4 depending on software used to export data. 0 for QuantaSoft, 4 for
#'     QXmanager.
#' @param nrChannels Number of channels/dyes used. Default nrChannels=2
#' @param targetChannel The channel nr used as target channel (default=1)
#' @param controlChannel The channel nr used as control channel (default=2)
#'
#' @return The function returns a list of dataframes named with the well ID
#'     and contains the amplitude values from the corresponding well.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' # Path to example data files included in PoDCall
#' path <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' # Read in data files
#' dataList <- importAmplitudeData(dataDirectory=path, skipLines=0)
#'
importAmplitudeData <- function(dataDirectory,
                                skipLines=c(0,4),
                                nrChannels=c(1,2)[2],
                                targetChannel=c(1,2,3,4,5,6)[1],
                                controlChannel=c(1,2,3,4,5,6)[2]){

    ## Check arguments
    if(!is.character(dataDirectory)) stop("dataDirectory must be character")
    if(!(skipLines %in% c(0,4))) stop("skipLines must be 0 or 4")
    if(!is.numeric(nrChannels)) stop("'nrChannels' must be numeric")
    if(!(nrChannels %in% seq_len(2)))
        stop("'nrChannels' must be at least 1 and maximum 2")
    if(!(targetChannel %in% seq_len(6))) stop("Invalid target channel")
    if(!(controlChannel %in% seq_len(6))) stop("Invalid control channel")
    if(targetChannel == controlChannel) stop("Target and control can not be
                                            the same channel")

    ## List raw amplitude files
    amplitudeFiles <- list.files(path=dataDirectory, pattern="_Amplitude.csv")

    ## Give warning if dataDirectory is empty
    if(length(amplitudeFiles) == 0) {
        warning("No amplitude files, check the data directory")
    }

    ## Vector with well id
    wellID <- unlist(
        lapply(strsplit(amplitudeFiles, split="_"),
                function(x) x[length(x) -1]))

    ## Full paths to amplitude files
    filenames <- file.path(dataDirectory, amplitudeFiles)

    tarCh <- targetChannel
    ctrlCh <- controlChannel

    ## Read files and store in list
    plateData <- lapply(filenames, function(x){
        wellData <- utils::read.csv(x, header=TRUE, sep=",", skip=skipLines
                            )[c(tarCh, ctrlCh)]
        colnames(wellData) <- c("Ch1", "Ch2", "Ch3",
                                "Ch4", "Ch5", "Ch6")[c(tarCh, ctrlCh)]
        ## Check data
        if(TRUE %in% is.nan(wellData[, 1]) |
            TRUE %in% is.na(wellData[, 1]) |
            TRUE %in% (wellData[, 1] < 0))
            warning(paste0("Check values of channel ", tarCh, " in file: ", x))
        if(TRUE %in% is.nan(wellData[, 2]) |
            TRUE %in% is.na(wellData[, 2]) |
            TRUE %in% (wellData[, 2] < 0))
            warning(paste0("Check values of channel ", ctrlCh, " in file: ", x))
        return(wellData)
    })
    ## Name list elements with well ID
    names(plateData) <- wellID

    ## Remove empty dataframes (read from empty amplitude files) to avoid error
    plateData <- plateData[vapply(plateData, nrow, numeric(1)) > 0]

    return(plateData)
}
