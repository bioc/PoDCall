#' @title Positive Droplet Calling for ddPCR
#'
#' @description Wrapper function that provide a complete workflow for the
#'     functionality of PoDCall. It takes path to amplitude files and sample
#'     sheet (optional), and parameters for setting threshold as input.
#'     Calls functions that read in data from files, sets threshold for each
#'     channel per well, calculates concentrations and optionally makes
#'     scatter plot and histogram for each channel per well.
#'     Results are returned as a table, optionally written to file.
#'     Plots will be written to file in a results directory if argument plots is
#'     set to TRUE.
#'
#' @param dataDirectory Path to directory containing QuantaSoft amplitude files
#'     from one 96 well plate. Since well coordinates are used as identifiers,
#'     files in this directory should all be from the same 96 well plate.
#'     Furthermore, there can be no other files than the amplitude files from
#'     a well plate in the directory.
#' @param sampleSheetFile File (optional) containing sample information from
#'     ddPCR experiment. This file must be a comma separated file containing the
#'     following columns: Well, Sample, TargetType and Target.
#' @param B The number of permutations used for the Likelihood Ratio Test
#'     (default=200)
#' @param Q A parameter for calling outliers (default=9)
#' @param refwell reference well to calculate the shift in baseline (default=1)
#' @param ch2 Logical argument to denote channel 2 amplitudes (default=TRUE)
#' @param resultsToFile Should results be written to file(.csv)? (default=FALSE)
#' @param plots Should plots be created and written to file? (default=FALSE)
#'
#' @return The function returns a table (data frame) with thresholds,
#'     droplet counts, concentration and normalized concentration.
#'     The table is optionally written to a .csv-file and plots for both
#'     channels per well can be written to files.
#'
#' @export
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom stats na.omit
#' @importFrom utils write.table tail
#'
#' @usage podcallDdpcr(dataDirectory,
#'                     sampleSheetFile=NULL,
#'                     B=200,
#'                     Q=9,
#'                     refwell=1,
#'                     ch2=TRUE,
#'                     resultsToFile=FALSE,
#'                     plots=FALSE)
#'
#' @examples
#'
#' ## Paths to data and sample sheet
#' dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
#' ssPath <- system.file("extdata", "Sample_names.csv", package="PoDCall")
#'
#' ## Run PodCall
#' podcallResults <- podcallDdpcr(dataDirectory=dataPath,
#'                                 sampleSheetFile=ssPath,
#'                                 B=100)
#'
podcallDdpcr <- function(dataDirectory, sampleSheetFile=NULL, B=200, Q=9,
                        refwell=1, ch2=TRUE, resultsToFile=FALSE, plots=FALSE){

    ## Check arguments
    checkArgumentsDdpcr(dataDirectory, sampleSheetFile, ch2,
                        resultsToFile, plots)

    ## If only one channel is used in the experiment
    nrChannels <- ifelse(ch2 == FALSE, 1, 2)

    ## Remove possible trailing "/" from path
    pathString <- strsplit(dataDirectory, split=NULL)
    if(utils::tail(pathString[[1]][length(pathString[[1]])]) == "/"){
        dataDirectory <- paste(pathString[[1]][-length(pathString[[1]])],
                                collapse = "")}
    ## Generate result directory
    if(resultsToFile | plots){resDir <- paste(dataDirectory,"results/", sep="_")
                                dir.create(resDir, showWarnings=TRUE)}

    ############################### READ IN DATA ###############################
    ## Read in amplitude data from file(s) and store as list.
    plateData <- importAmplitudeData(dataDirectory)

    ## Add sample sheet information
    sampleSheet <- importSampleSheet(sampleSheet=sampleSheetFile,
                                    well_id=names(plateData))

    ############################## SET THRESHOLDS ##############################
    thrRes <- data.frame(sample_id=sampleSheet[,"sample_id"],
                        podcallThresholds(plateData=plateData,
                                        nchannels=nrChannels,
                                        B=B, Q=Q, refwell, updateProgress=NULL),
                        q=rep(Q, length(plateData)),
                        target_assay=sampleSheet[,"target_assay"],
                        ctrl_assay=sampleSheet[,"ctrl_assay"],
                        stringsAsFactors=FALSE)

    ## Set file name for result-file
    resFilename <- paste(strsplit(dataDirectory,
                                    "/")[[1]][length(strsplit(dataDirectory,
                                                            "/")[[1]])],
                        "_thresholds.csv", sep="")

    ## Write results to result-file
    if(resultsToFile){
        utils::write.table(data.frame("Well_ID"=names(plateData), thrRes),
                            file=paste(resDir, resFilename, sep=""),
                            row.names=FALSE, quote=FALSE, sep=",")
    }
    ################################ MAKE PLOTS ################################
    if(plots){
        invisible(mapply(function(x, i) {
            invisible(vapply(seq_len(nrChannels), function(k) {
                ## Plot title
                id <- paste(i, ", ", thrRes[i, "sample_id"], ", ",
                            thrRes[i, c("target_assay", "ctrl_assay")[k]])
                ## File name, plot
                outputname <- paste(i, thrRes[i, "sample_id"],
                                    c("target.pdf", "control.pdf")[k], sep="_")
                ## Write scatter plot and histogram to file
                grDevices::pdf(paste(resDir, outputname,sep=""))
                podcallChannelPlot(channelData=stats::na.omit(x[, k]),
                                    thr=thrRes[i, c("thr_target",
                                                    "thr_ctrl")[k]],
                                    channel=k, plotId=id)
                grDevices::dev.off()
                return(k)}, FUN.VALUE=numeric(1)))
        }, x=plateData, i=names(plateData)))
    }
    ############################## RETURN RESULTS ##############################
    return(thrRes)
}

## Internal functions

## Function that checks the arguments to podcallDdpcr()
checkArgumentsDdpcr <- function(dataDirectory, sampleSheetFile, ch2,
                            resultsToFile, plots){

    ## Check arguments
    if(!is.character(dataDirectory)) stop("dataDirectory must be character")
    if(!is.character(sampleSheetFile) & !is.null(sampleSheetFile))
        stop("sampleSheetFile must be character")
    if(!is.logical(ch2)) stop("ch2 must be logical")
    if(!is.logical(resultsToFile)) stop("resultsToFile must be logical")
    if(!is.logical(plots)) stop("plots must be logical")

    return(NULL)
}
