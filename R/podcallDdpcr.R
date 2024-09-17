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
#' @param targetChannel The channel nr used as target channel (default=1)
#' @param controlChannel The channel nr used as control channel (default=2)
#' @param nrChannels If single channel target and no control channel, set to 1,
#'     if control channel is used, set to 2 (default=2)
#' @param software The software data was exported from, either QuntaSoft or
#'     QXmanager. Needs to be specified to ensure correct reading of data and
#'     sample sheet due to difference in formatting. (defult="QX Manager")
#' @param resultsToFile Should results be written to file(.csv)? (default=FALSE)
#' @param plots Should plots be created and written to file? (default=FALSE)
#' @param resPath Optional argument to provide results directory path
#'     (default=NULL)
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
#'                     targetChannel=1,
#'                     controlChannel=2,
#'                     nrChannels=2,
#'                     software=c("QuantaSoft", "QX Manager")[2],
#'                     resultsToFile=FALSE,
#'                     plots=FALSE,
#'                     resPath=NULL)
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
#'                                 B=100, software="QuantaSoft")
#'
podcallDdpcr <- function(dataDirectory, sampleSheetFile=NULL, B=200, Q=9,
                        refwell=1, nrChannels=c(1,2)[2],
                        targetChannel=c(1,2,3,4,5,6)[1],
                        controlChannel=c(1,2,3,4,5,6)[2],
                        software=c("QuantaSoft", "QX Manager")[2],
                        resultsToFile=FALSE, plots=FALSE, resPath=NULL){

    ## Check arguments
    checkArgumentsDdpcr(dataDirectory, sampleSheetFile, nrChannels, software,
                        resultsToFile, plots)

    ## Remove possible trailing "/" from path
    pathString <- strsplit(dataDirectory, split=NULL)
    if(utils::tail(pathString[[1]][length(pathString[[1]])]) == "/"){
        dataDirectory <- paste(pathString[[1]][-length(pathString[[1]])],
                                collapse = "")}
    ## Generate result directory if path not provided
    if(is.null(resPath) & (resultsToFile | plots)){
        resDir <- paste(dataDirectory,"results/", sep="_")
                                dir.create(resDir, showWarnings=TRUE)
    }else {resDir <- resPath}

    ############################### READ IN DATA ###############################
    ## Read in amplitude data from file(s) and store as list.
    if(software == "QuantaSoft") skiplines <- 0
    if(software == "QX Manager") skiplines <- 4

    plateData <- importAmplitudeData(dataDirectory, skipLines=skiplines,
                                     nrChannels=nrChannels)

    ## Add sample sheet information
    sampleSheet <- importSampleSheet(sampleSheet=sampleSheetFile,
                                    well_id=names(plateData),
                                    software=software)

    ############################## SET THRESHOLDS ##############################
    thrRes <- data.frame(sample_id=sampleSheet[,"sample_id"],
                        podcallThresholds(plateData=plateData,
                                        nrChannels=nrChannels,
                                        targetChannel=targetChannel,
                                        controlChannel=controlChannel,
                                        B=B, Q=Q, refwell, updateProgress=NULL),
                        q=rep(Q, length(plateData)),
                        target_assay=sampleSheet[,"target_assay"],
                        ctrl_assay=sampleSheet[,"ctrl_assay"],
                        ref_well=names(plateData)[refwell],
                        stringsAsFactors=FALSE)

    ## Set file name for result-file
    resFilename <- paste(strsplit(dataDirectory,
                                    "/")[[1]][length(strsplit(dataDirectory,
                                                            "/")[[1]])],
                        "_thresholds.csv", sep="")

    ## Write results to result-file
    if(resultsToFile){
        utils::write.table(data.frame("Well_ID"=names(plateData), thrRes),
                            file=file.path(resDir, resFilename),
                            row.names=FALSE, quote=FALSE, sep=",")
    }
    ################################ MAKE PLOTS ################################
    if(plots){
        channels <- seq_len(nrChannels)
        invisible(mapply(function(x, i) {
            invisible(vapply(channels, function(k){
                ## Plot title
                id <- paste(i, ", ", c(paste0("Ch", targetChannel),"CtrlCh")[k],
                            ", ", thrRes[i, "sample_id"], ", ",
                            thrRes[i, c("target_assay", "ctrl_assay")[k]])
                ## File name, plot
                outputname <- paste(i, thrRes[i, "sample_id"],
                                    c(paste0("Ch", targetChannel, ".pdf"),
                                        "control.pdf")[k], sep="_")
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
checkArgumentsDdpcr <- function(dataDirectory, sampleSheetFile, nrChannels,
                                software, resultsToFile, plots){

    ## Check arguments
    if(!is.character(dataDirectory)) stop("'dataDirectory' must be character")
    if(!is.character(sampleSheetFile) & !is.null(sampleSheetFile))
        stop("'sampleSheetFile' must be character")
    if(!is.numeric(nrChannels)) stop("'nrChannels' must be numeric")
    if(!(nrChannels %in% seq_len(6)))
        stop("'nrChannels' must be at least 1 and maximum 6")
    if(!(software %in% c("QuantaSoft", "QX Manager")))
        stop("'software' must be 'QuantaSoft' or 'QX Manger'")
    if(!is.logical(resultsToFile)) stop("'resultsToFile' must be logical")
    if(!is.logical(plots)) stop("'plots' must be logical")

    return(NULL)
}
