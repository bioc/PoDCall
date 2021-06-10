#
#' @title podcallThresholds
#'
#' @description Function sets threshold per channel per well and calculates
#'     concentrations. Results are returned as a data frame.
#'
#' @param plateData List of data frames with amplitude data from a 96 well plate
#' @param nchannels Number of channels used in the experiment (default=2)
#' @param B Number of permutations for the Likelihood Ratio Test (LRT)
#'     (default=200)
#' @param Q Parameter for outlier calling (default=9)
#' @param refWell reference well to calculate the shift in baseline (default=1)
#' @param updateProgress function to update progress bar in shiny app
#'     (default=NULL)
#'
#' @return A table with results and metrics, one row per well.
#' @export
#'
#' @importFrom diptest dip.test
#' @importFrom mclust densityMclust mclustBootstrapLRT hc hcE
#' @importFrom stats median quantile
#' @importFrom purrr map
#' @importFrom LaplacesDemon Modes
#'
#' @usage podcallThresholds(plateData,
#'                         nchannels=c(1,2)[2],
#'                         B=200,
#'                         Q=9,
#'                         refWell=1,
#'                         updateProgress=NULL)
#'
#' @examples
#' ## Path to example data
#' dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' ## Read in example data
#' dataList <- importAmplitudeData(dataDirectory=dataPath)
#'
#' ## Set thresholds
#' thresholds <- podcallThresholds(plateData=dataList,
#'                                 B=100)
#'
podcallThresholds <- function(plateData, nchannels=c(1,2)[2], B=200, Q=9,
                            refWell=1, updateProgress=NULL){

    ## Check arguments
    if(!is.list(plateData)) stop("plateData must be a list")
    if(nchannels > 2 | nchannels < 1) stop("invalid number of channels")

    ## Global threshold channel 1 (target channel)
    if(sum(is.na(plateData[[refWell]][1]))<nrow(plateData[[refWell]])){
    message("Setting global threshold channel 1")
    targetExtract <- purrr::map(plateData, 1)
    targetModeReference <- getMode(targetExtract[[refWell]])
    targetAmpScaled <- lapply(targetExtract, function(X){
        delta <- targetModeReference-getMode(X); return(X+delta)})
    targetAmpScaled <- sort(na.omit(unlist(targetAmpScaled)))

    ## Progress output for shiny app
    if(is.function(updateProgress)){text <- paste0("Global threshold channel 1")
        updateProgress(detail=text, value=1/3)
    }
    ## Global threshold target channel
    targetRes <- globalThresholding(scaledAmplitudeDist=targetAmpScaled,
                                    Q=Q, B=B, init=FALSE)
    }else {
        targetRes <- NA
        targetModeReference <- NA
    }

    ## Global threhold channel 2 (control channel)
    message("Setting global threshold channel 2")
    if(is.na(plateData[[1]][1,2]) == FALSE){
        refExtract <- lapply(purrr::map(plateData, 2), function(X){
            return(sort(na.omit(X)))})
        refModeReference <- getMode(refExtract[[refWell]])

        ## Progress output for shiny app
        if(is.function(updateProgress)){
            text <- paste0("Global threshold channel 2")
            updateProgress(detail=text, value=4/5)
        }
        ## Global threshold control channel
        refRes <-
            globalThresholding(scaledAmplitudeDist=refExtract[[refWell]],
                                Q=Q, B=B, init=TRUE)
    }
    #######################  Well-specific thresholding ########################
    ## Populate result data frame
    thrRes <- fillThrTable(plateData, targetRes, targetModeReference,
                            refRes, refModeReference)

    return(thrRes)
}
############################## INTERNAL FUNCTIONS ##############################

## unimodal thresholding
thr.unimodal <- function(component, Q){
    outliers <- get.outliers(component, Q=Q)$outliers
    ifelse(length(outliers) > 0,
            dens <- mclust::densityMclust(component, G=1, modelNames="E",
                                        initialization=list(noise=outliers)),
            dens <- mclust::densityMclust(component, G=1, modelNames="E"))
    thr <- ifelse(length(outliers) > 0,
                    ceiling(component[min(which(dens$classification == 0 &
                                            component > get.outliers(component,
                                            upper=TRUE, Q=Q)$cutoff)-1)]),
                    max(component))

    message("Global threshold set to: ", thr)

    return(list(threshold=thr))
}

## Determine outliers
get.outliers <- function(component, upper=TRUE, Q){
    qq <- stats::quantile(component)
    IQR <- qq[4] - qq[2]
    out_limHigh <- qq[4] + Q * IQR
    out_limLow <- qq[2] - Q * IQR
    ifelse(upper == TRUE,
            return(list(outliers=which(component > out_limHigh),
                        cutoff=out_limHigh)),
            return(list(outliers=which(component < out_limLow),
                        cutoff=out_limLow)))
}

## Multimodal thresholding
thr.multimodal <- function(component, densityEstimate){
    modVec <- NULL
    for(i in sort(unique(densityEstimate$classification))){
        modVec[i] <-
            stats::median(component[densityEstimate$classification == i])
    }

    mod1 <- sort(modVec[which(modVec>getMode(component))])[1]
    mod2 <- sort(modVec[which(modVec>getMode(component))])[2]

    #set the threshold as the median of the 2 components
    thr <- mean(c(mod1,mod2))

    message("Global threshold set to: ", thr)
    return(list(threshold=thr, firstMod=mod1))
}

## Get mode
getMode <- function(num){
    return(suppressWarnings(sort(LaplacesDemon::Modes(num)$modes)[[1]]))
}

## Global thresholding
globalThresholding <- function(scaledAmplitudeDist, Q, B, init){
    if (diptest::dip.test(scaledAmplitudeDist)$p.value > 0.05) {
        res <- thr.unimodal(scaledAmplitudeDist, Q=Q); comp <- 1
    }else {
        if(length(scaledAmplitudeDist) > 8000) {
            scaledAmplitudeDistSubset <-
                sort(sample(x=scaledAmplitudeDist, size=8000))}
        else{scaledAmplitudeDistSubset <- scaledAmplitudeDist}
        if(init == TRUE){
            hc <- mclust::hc(scaledAmplitudeDistSubset, use="VARS",
                            modelName="E")
            bBIC <- mclustBootstrapLRT(scaledAmplitudeDistSubset,
                                        modelName="E", nboot=B,
                                        initialization=list(hcPairs=hc))
        }
        else{
            bBIC <- mclustBootstrapLRT(scaledAmplitudeDistSubset,
                                        modelName="E", nboot=B)
        }
        comp <- suppressWarnings(max(which(bBIC$p.value < 0.05))) + 1
        if(is.infinite(comp)) {comp <- 1}
        if(comp == 1) {res <- thr.unimodal(scaledAmplitudeDist, Q = Q)}
        if(comp > 1) {
            res <- thr.multimodal(scaledAmplitudeDist,
                                    densityEstimate=mclust::densityMclust(
                                    scaledAmplitudeDist,
                                    G=comp,
                                    modelNames="E"))}
    }
    return(res)
}

## Set individual thresholds per well and populate threshold table
fillThrTable <- function(plateData, targetRes, targetModeReference,
                        refRes, refModeReference){

    ## Create data frame to hold the results
    thrRes <- data.frame(matrix(0, nrow=length(plateData), ncol=8),
                        row.names=names(plateData))
    colnames(thrRes) <- c("thr_target", "thr_ctrl", "pos_dr_target",
                        "pos_dr_ctrl", "tot_droplets", "c_target", "c_ctrl",
                        "c_norm")

    ## Populate result data frame
    thrRes[, "thr_target"] <- vapply(plateData, function(x){
        ifelse(is.na(targetRes), NA,
            {delta <- targetModeReference-getMode(sort(na.omit(x[,"Ch1"])))
            return(ceiling(targetRes$threshold - delta))})}, numeric(1))

    thrRes[, "thr_ctrl"] <- vapply(plateData, function(x){
        ifelse(is.na(x[1, "Ch2"]), NA,
                {delta <- refModeReference-getMode(sort(na.omit(x[,"Ch2"])))
                return(ceiling(refRes$threshold - delta))})}, numeric(1))

    thrRes[, "pos_dr_target"] <- mapply(function(x, well_id){
        sum(na.omit(x[,"Ch1"]) >= thrRes[well_id, "thr_target"])},
        x=plateData, well_id=names(plateData))

    thrRes[, "pos_dr_ctrl"] <- mapply(function(x, well_id){
        sum(na.omit(x[,"Ch2"]) >= thrRes[well_id, "thr_ctrl"])},
        x=plateData, well_id=names(plateData))

    thrRes[, "tot_droplets"] <-
        vapply(plateData, function(x) nrow(x), numeric(1))

    cst <- 0.000851 # Constant from QuantaSoft (Bio-Rad)
    thrRes[, "c_target"] <-
        signif(-log((thrRes[, "tot_droplets"]-thrRes[, "pos_dr_target"])/
                        thrRes[, "tot_droplets"])/cst, digits=4)
    thrRes[, "c_ctrl"] <-
        signif(-log((thrRes[, "tot_droplets"]-thrRes[, "pos_dr_ctrl"])/
                        thrRes[, "tot_droplets"])/cst, digits=4)
    thrRes[, "c_norm"] <-
        ifelse(thrRes[, "c_ctrl"] == 0, "No DNA",
                signif((thrRes[, "c_target"]/thrRes[, "c_ctrl"])*400,
                        digits=4))

    return(thrRes)
}
