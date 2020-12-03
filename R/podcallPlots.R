#' @title podcallScatterplot
#'
#' @description Function that make a scatterplot of amplitude values from one
#'     channel of a well with threshold indicated by a horizontal line.
#'
#' @param channelData Amplitude values from one channel of a well.
#' @param thr The threshold set for \code{channel} of a well.
#' @param channel The channel the amplitude values belong to.
#' @param plotId A character string with title for the plot
#'
#' @return A scatterplot of all droplets from a channel from a well with a line
#'     indicating the set threshold.
#' @export
#'
#' @importFrom ggplot2 ggplot aes labs qplot geom_point geom_hline geom_vline
#'     theme element_blank scale_color_manual
#'
#' @examples
#'
#' # Get path to data
#' path <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' # Read in data
#' data <- importAmplitudeData(path)
#' data("thrTable")
#'
#' # Get name of first list element and use as well ID
#' well_id <- names(data)[1]
#'
#' # Set channel to plot
#' channel <- 1
#'
#' # Get threshold for well_id and channel 1 (see ?thrTable)
#' thr <- thrTable[well_id, "thr_target"]
#'
#' scatterplot <- podcallScatterplot(channelData=data[[well_id]][[channel]],
#'                                 thr,
#'                                 channel)
#'
podcallScatterplot <- function(channelData, thr, channel, plotId=NULL){

    ## Check arguments
    if(!is.numeric(channelData)) stop("channelData must be numeric")
    if(!(channel %in% c(1, 2))) stop("invalid channel number")
    if(!is.numeric(thr)) stop("thr must be numeric")
    if(length(thr)>1) stop("thr must be a single value")
    if(!is.character(plotId) & !is.null(plotId)) stop("plotId must be
                                                        character")

    ## Randomize order of amplitude values to make nice plot
    ddr <- sample(channelData)
    ## Create dataframe with randomized amplitude values
    ddDf <- data.frame(ddr)

    # Add coloring factor based on threshold for calling positive droplets
    ddDf$col <- cut(ddDf$ddr, breaks=c(-Inf, thr, Inf))

    ## Colors to be used for target channel and control channel, respectively
    chCol <- c("dodgerblue3", "forestgreen")

    # Scatter plot of amplitude values
    scatterplot <- ggplot(ddDf, aes(x=seq_len(length(ddr)), y=ddr, color=col)) +
        geom_point(size=0.5) +
        labs(title=plotId, x="Events", y="Amplitude", color=NULL) +
        geom_hline(yintercept = thr, col="magenta") +
        theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                legend.position="none") +
        scale_color_manual(labels = c("neg", "pos"),
                            values=c("gray50", chCol[channel]))

    ## Return scatterplot for channel k, well f
    return(scatterplot)
}

################################################################################

#' @title podcallHistogram
#'
#' @description Function that make a histogram of amplitude values from one
#'     channel of a well with threshold indicated by a vertical line.
#'
#' @param channelData Amplitude values from one channel of a well.
#' @param thr The threshold set for \code{channel} of a well.
#' @param channel The channel the amplitude values belong to.
#' @param plotId A character string with title for the plot
#'
#' @return A histogram of amplitude values from a channel from a well with a
#'     line indicating the set threshold.
#' @export
#'
#' @importFrom ggplot2 geom_vline geom_histogram
#'
#' @examples
#'
#' # Get path to data
#' path <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' # Read in data
#' data <- importAmplitudeData(path)
#' data("thrTable")
#' 
#' # Get name of first list element and use as well ID
#' well_id <- names(data)[1]
#'
#' # Set channel to plot
#' channel <- 1
#'
#' # Get threshold for well_id and channel 1 (see ?thrTable)
#' thr <- thrTable[well_id, "thr_target"]
#'
#' histogram <- podcallHistogram(channelData=data[[well_id]][[channel]],
#'                             thr,
#'                             channel)
#'
podcallHistogram <- function(channelData, thr, channel, plotId=NULL){

    ## Check arguments
    if(!is.numeric(channelData)) stop("channelData must be numeric")
    if(channel > 2 | channel < 1) stop("invalid channel number")
    if(!is.numeric(thr)) stop("thr must be numeric")
    if(length(thr)>1) stop("thr must be a single value")
    if(!is.character(plotId) & !is.null(plotId)) stop("plotId must be
                                                        character")

    ## Randomize order of amplitude values to make nice plot
    ddr <- sample(channelData)
    ## Create dataframe with randomized amplitude values
    ddDf <- data.frame(ddr)

    ## Colors to be used for target channel and control channel, respectively
    chCol <- c("dodgerblue3", "forestgreen")

    ## Histogram of amplitude values
    histogram <- ggplot(ddDf, aes(x=ddr))+
        geom_histogram(binwidth=5,
                        fill=I(chCol[channel]))+
        labs(title=plotId, x="Amplitude value", y="Frequency")+
        geom_vline(xintercept=thr, col="magenta")

    ## Return histogram for channel k, well f
    return(histogram)
}

################################################################################

#' @title podcallChannelPLot
#'
#' @description Function that calls podcallScatterplot and podcallHistogram and
#'     draws a plot with both scatter plot and histogram.
#'
#' @param channelData Amplitude values from one channel of a well.
#' @param thr The threshold set for \code{channel} of a well.
#' @param channel The channel the amplitude values belong to.
#' @param plotId A character string with title for the plot
#'
#' @return A gtable with scatterplot and histogram
#' @export
#'
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' ## Get path to data
#' path <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' ## Read in data
#' data <- importAmplitudeData(path)
#'
#' ## Get name of first list element and use as well ID
#' well_id <- names(data)[1]
#'
#' ## Set channel to plot
#' channel <- 1
#'
#' ## Get threshold for well_id and channel 1 (see ?thrTable)
#' thr <- thrTable[well_id, "thr_target"]
#'
#' podcallChannelPlot(channelData=data[[well_id]][[channel]], thr, channel)
#'
podcallChannelPlot <- function(channelData, thr, channel, plotId=NULL){

    ## Check arguments
    if(!is.numeric(channelData)) stop("channelData must be numeric")
    if(channel > 2 | channel < 1) stop("invalid channel number")
    if(!is.numeric(thr)) stop("thr must be numeric")
    if(length(thr)>1) stop("thr must be a single value")
    if(!is.character(plotId) & !is.null(plotId)) stop("plotId must be
                                                        character")

    ## Make scatterplot
    scatterplot <- podcallScatterplot(channelData=channelData,
                                        thr=thr,
                                        channel=channel)
    ## Make histogram
    histogram <- podcallHistogram(channelData=channelData,
                                    thr=thr,
                                    channel=channel)


    return(gridExtra::grid.arrange(grobs=list(scatterplot, histogram),
                                    nrow=2, top=plotId))

}


