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
#' dataList <- importAmplitudeData(dataDirectory=path)
#'
importAmplitudeData <- function(dataDirectory){

    if(!is.character(dataDirectory)) stop("dataDirectory must be character")

    ###LIST THE RAW AMPLITUDE FILES
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
    filenames <- paste(dataDirectory, amplitudeFiles, sep="/")

    ## Read files and store in list
    plateData <- lapply(filenames, function(x){
        wellData <- utils::read.csv(x, header=TRUE, sep=",",
                            col.names=c("Ch1", "Ch2", "cluster"))[seq_len(2)]
        if(TRUE %in% is.nan(wellData[, 1]) | TRUE %in% is.na(wellData[, 1]) |
           TRUE %in% (wellData[, 1] < 0))
            warning(paste0("Check values of channel 1 in file: ", x))
        if(TRUE %in% is.nan(wellData[, 2]) | TRUE %in% is.na(wellData[, 2]) |
           TRUE %in% (wellData[, 2] < 0))
            warning(paste0("Check values of channel 2 in file: ", x))
        return(wellData)
    })
    ## Name list elements with well ID
    names(plateData) <- wellID

    ## Remove empty dataframes (read from empty amplitude files) to avoid error
    plateData <- plateData[vapply(plateData, nrow, numeric(1)) > 0]

    return(plateData)
}
