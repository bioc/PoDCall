#' @title Positive Droplet Calling for ddPCR
#'
#' @description Wrapper function that provide a complete workflow for the
#'     functionality of PoDCall. It takes path to amplitude files and sample
#'     sheet (optional), and parameters for setting threshold as input.
#'     Calls functions that read in data from files, sets threshold for each
#'     channel per well, calculates concentrations and optionally makes
#'     scatterplot and histogram for each channel per well.
#'     Results are returned as a table, optionally written to file.
#'     Plots will only be made if user chooses to save results to file, and will
#'     be written to file in a results directory.
#'
#' @param dataDirectory Path to directory containing Quantasoft amplitude files
#'     from one 96 well plate. Since well coordinates are used as identifiers,
#'     files in this directory should all be from the same 96 well plate.
#'     Furthermore, there can be no other files than the amplitude files from
#'     a well plate in the directory.
#' @param sampleSheetFile File (optional) containing sample information from
#'     ddPCR experiment. This file must be a comma separated file containing the
#'     following columns: Well, Sample, TargetType and Target.
#' @param B The number of permutations used for the Likelihood Ratio Test
#'     (default=400)
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
#' @importFrom gridExtra grid.arrange
#' @importFrom utils write.table tail
#'
#' @examples
#' \dontrun{
#' # Paths to data and sample sheet
#' dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
#' ssPath <- system.file("extdata", "Sample_names.csv", package="PoDCall")
#'
#' # Run PodCall
#' podcallResults <- podcall_ddpcr(dataDirectory=dataPath,
#'                                  sampleSheetFile=ssPath)
#'}
podcall_ddpcr <- function(dataDirectory,
                        sampleSheetFile=NULL,
                        B=400,
                        Q=9,
                        refwell=1,
                        ch2=TRUE,
                        resultsToFile=FALSE,
                        plots=FALSE){

    ## If only one channel is used in the experiment
    nrChannels <- ifelse(ch2 == FALSE, 1, 2)

    ## Remove possible trailing "/" from path
    pathString <- strsplit(dataDirectory, split=NULL)
    if(utils::tail(pathString[[1]][length(pathString[[1]])]) == "/"){
        dataDirectory <- paste(pathString[[1]][-length(pathString[[1]])],
                                collapse = "")
    }

    ## Generate result directory
    if(resultsToFile == TRUE | plots == TRUE){
        resDir <- paste(dataDirectory,"results/", sep="_")
        dir.create(resDir, showWarnings=TRUE)
    }

    ############################### READ IN DATA ###############################
    ## Read in amplitude data from file(s) and store as list.
    ## Each elemt of the list corresponds to one well.
    plateData <- importAmplitudeData(dataDirectory)

    ## Create vectors to hold sample information
    sample_id <- character(length(plateData))
    target_assay <- character(length(plateData))
    ctrl_assay <- character(length(plateData))

    ## Get info from sample sheet if provided
    if(!is.null(sampleSheetFile)){
        sampleSheet <- importSampleSheet(sampleSheet=sampleSheetFile,
                                        names(plateData))

        sample_id <- sampleSheet[, "sample_id"]
        target_assay <- sampleSheet[, "target_assay"]
        ctrl_assay <- sampleSheet[, "ctrl_assay"]
    }

    ## Create vector with Q value, one entry per amplitude file
    q <- rep(Q, length(plateData))

    ############################## SET THRESHOLDS ##############################
    thrRes <- data.frame(sample_id,
                        podcallThresholds(plateData=plateData,
                                        nchannels=nrChannels,
                                        B=B,
                                        Q=Q,
                                        refwell,
                                        updateProgress=NULL),
                        q,
                        target_assay,
                        ctrl_assay,
                        stringsAsFactors=FALSE)

    ## Define filename for result-file
    resFilename <- paste(strsplit(dataDirectory,
                                    "/")[[1]][length(strsplit(dataDirectory,
                                                            "/")[[1]])],
                        "_thresholds.csv", sep="")

    ## Write results to result-file
    if(resultsToFile == TRUE){
        utils::write.table(data.frame("Well_ID"=names(plateData), thrRes),
                            file=paste(resDir, resFilename, sep=""),
                            row.names=FALSE, quote=FALSE, sep=",")
    }

    ################################ MAKE PLOTS ################################
    if(resultsToFile == FALSE & plots == TRUE){
        message("resultsToFile must be set to TRUE to make plots")
    }
    if(resultsToFile == TRUE & plots == TRUE){
        ## Loop over list elements corresponding to wells
        for(f in seq_len(length(plateData))){

            ## Get amplitude values for well f
            data <- plateData[[f]]

            ## Loop over channels of well f
            for(k in seq_len(nrChannels)){

                ## Amplitude values for well f, channel k
                dd <- stats::na.omit(data[, k])

                ## Make scatterplot
                scatterplot <- podcallScatterplot(channelData=dd,
                                                    thr=thrRes[f, "thr_target"],
                                                    channel=k)
                ## Make histogram
                histogram <- podcallHistogram(channelData=dd,
                                                thr=thrRes[f, "thr_ctrl"],
                                                channel=k)

                ## Set information about channel to be used in plot title
                if (k == 1){
                    ## Plot title
                    id <- paste(names(plateData)[f], ", ",
                                thrRes[f, "sample_id"], ", ",
                                thrRes[f, "target_assay"])
                    ## Plot filename
                    outputname <- paste(names(plateData)[f],
                                        thrRes[f, "sample_id"],
                                        "target.pdf",
                                        sep="_")
                }
                else {
                    ## Plot title
                    id <- paste(names(plateData)[f], ", ",
                                thrRes[f, "sample_id"], ", ",
                                thrRes[f, "ctrl_assay"])
                    ## Plot filename
                    outputname <- paste(names(plateData)[f],
                                        thrRes[f, "sample_id"],
                                        "control.pdf",
                                        sep="_")
                }

                ## Write scatterplot and histogram to file
                grDevices::pdf(paste(resDir, outputname,sep=""))
                gridExtra::grid.arrange(scatterplot, histogram,
                                        nrow=2,
                                        top=id)
                grDevices::dev.off()
            }
        }
    }

    ############################## RETURN RESULTS ##############################

    return(thrRes)
}
