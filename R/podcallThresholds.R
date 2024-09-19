#
#' @title podcallThresholds
#'
#' @description Function sets threshold per channel per well and calculates
#'     concentrations. Results are returned as a data frame.
#'
#' @param plateData List of data frames with amplitude data from a 96 well plate
#' @param nrChannels If single channel target and no control channel, set to 1,
#'     if control channel is used, set to 2 (default=2)
#' @param B Number of permutations for the Likelihood Ratio Test (LRT)
#'     (default=200)
#' @param Q Parameter for outlier calling (default=9)
#' @param refWell Reference well to calculate the shift in baseline (default=1)
#' @param targetChannel The channel nr used as target channel (default=1)
#' @param controlChannel The channel nr used as control channel (default=2)
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
#'                         nrChannels=c(1,2)[2],
#'                         B=200,
#'                         Q=9,
#'                         refWell=1,
#'                         targetChannel=c(1,2,3,4,5,6)[1],
#'                         controlChannel=c(1,2,3,4,5,6)[2],
#'                         updateProgress=NULL)
#'
#' @examples
#' ## Path to example data
#' dataPath <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' ## Read in example data
#' dataList <- importAmplitudeData(dataDirectory=dataPath, skipLines=0)
#'
#' ## Set thresholds
#' thresholds <- podcallThresholds(plateData=dataList,
#'                                 B=100)
#'
podcallThresholds <- function(plateData, nrChannels=c(1,2)[2],
                                B=200, Q=9, refWell=1,
                                targetChannel=c(1,2,3,4,5,6)[1],
                                controlChannel=c(1,2,3,4,5,6)[2],
                                updateProgress=NULL){

    ## Check arguments
    if(!is.list(plateData)) stop("plateData must be a list")
    if(nrChannels > 2 | nrChannels < 1) stop("invalid number of channels")
    if(targetChannel == controlChannel) stop("Target and control can not be
                                                the same channel")

    ## Global threshold target channel
    if(sum(is.na(plateData[[refWell]][1]))<nrow(plateData[[refWell]])){
    message("Setting global threshold target channel")
    targetExtract <- purrr::map(plateData, 1)
    targetModeReference <- getMode(targetExtract[[refWell]])
    targetAmpScaled <- lapply(targetExtract, function(X){
        delta <- targetModeReference-getMode(X); return(X+delta)})
    targetAmpScaled <- sort(na.omit(unlist(targetAmpScaled)))

    ## Progress output for shiny app
    if(is.function(updateProgress)){text <-
            paste0("Global threshold target channel")
        updateProgress(detail=text, value=1/3)
    }
    ## Global threshold target channel
    targetRes <- globalThresholding(scaledAmplitudeDist=targetAmpScaled,
                                    Q=Q, B=B, init=FALSE)
    }else {
        targetRes <- NA
        targetModeReference <- NA
    }

    ## Global threshold channel 2 (control channel)
    if(is.na(plateData[[1]][1,2]) == FALSE){
        message("Setting global threshold control channel")
        refExtract <- lapply(purrr::map(plateData, 2), function(X){
            return(sort(na.omit(X)))})
        refModeReference <- getMode(refExtract[[refWell]])

        ## Progress output for shiny app
        if(is.function(updateProgress)){
            text <- paste0("Global threshold control channel")
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
                            refRes, refModeReference, targetChannel)

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
        print("Unimodal distribution")
    }else { print("Multimodal distribution")
        if(length(scaledAmplitudeDist) > 8000) {
            scaledAmplitudeDistSubset <-
                sort(sample(x=scaledAmplitudeDist, size=8000))
            assign("sampled_droplets",
                    scaledAmplitudeDistSubset, envir=.GlobalEnv)}
        else{scaledAmplitudeDistSubset <- scaledAmplitudeDist}
        if(init == TRUE){
            hc <- mclust::hc(scaledAmplitudeDistSubset, use="VARS",
                            modelName="E")
            bBIC <- mclust::mclustBootstrapLRT(scaledAmplitudeDistSubset,
                                                modelName="E", nboot=B,
                                                initialization=list(hcPairs=hc))
        }
        else{
            bBIC <- mclust::mclustBootstrapLRT(scaledAmplitudeDistSubset,
                                                modelName="E", nboot=B)
        }
        comp <-
            suppressWarnings(max(which(bBIC$p.value[seq_len(2)] < 0.05))) + 1

        if(is.infinite(comp)) {comp <- 1}
        if(comp == 1) {res <- thr.unimodal(scaledAmplitudeDist, Q = Q)}
        if(comp > 1) {
            res <- thr.multimodal(scaledAmplitudeDist,
                                  densityEstimate=mclust::densityMclust(
                                      scaledAmplitudeDist,
                                      G=comp,
                                      modelNames="E")
                                  )
            }
        }
    return(res)
}

## Set individual thresholds per well and populate threshold table
fillThrTable <- function(plateData, targetRes, targetModeReference,
                        refRes, refModeReference, targetChannel){

    ## Create data frame to hold the results
    thrRes <- data.frame(matrix(0, nrow=length(plateData), ncol=10),
                        row.names=names(plateData))
    colnames(thrRes) <- c("target_ch","thr_target", "thr_ctrl", "pos_dr_target",
                        "pos_dr_ctrl", "tot_droplets", "c_target", "c_ctrl",
                        "c_norm_4Plex", "c_norm_sg")
    thrRes[,"target_ch"] <- paste0("Ch", targetChannel)

    ## Populate result data frame
    thrRes[, "thr_target"] <- vapply(plateData, function(x){
        ifelse(is.na(targetRes), NA,
            {delta <- targetModeReference-getMode(sort(na.omit(x[, 1])))
            return(ceiling(targetRes$threshold - delta))})}, numeric(1))

    thrRes[, "thr_ctrl"] <- vapply(plateData, function(x){
        ifelse(is.na(x[1, 2]), NA,
                {delta <- refModeReference-getMode(sort(na.omit(x[, 2])))
                return(ceiling(refRes$threshold - delta))})}, numeric(1))

    thrRes[, "pos_dr_target"] <- mapply(function(x, well_id){
        sum(na.omit(x[, 1]) >= thrRes[well_id, "thr_target"])},
        x=plateData, well_id=names(plateData))

    thrRes[, "pos_dr_ctrl"] <- mapply(function(x, well_id){
        sum(na.omit(x[, 2]) >= thrRes[well_id, "thr_ctrl"])},
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
    thrRes[, "c_norm_4Plex"] <-
        ifelse(thrRes[, "c_ctrl"] == 0, "No DNA",
                signif((thrRes[, "c_target"]/thrRes[, "c_ctrl"])*400,
                        digits=4))

    thrRes[, "c_norm_sg"] <-
        ifelse(thrRes[, "c_ctrl"] == 0, "No DNA",
               signif((thrRes[, "c_target"]/thrRes[, "c_ctrl"])*100,
                      digits=4))

    return(thrRes)
}
