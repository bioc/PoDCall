#' @title podcallMultiplot
#'
#' @description A function that returns faceted scatterplots for multiple wells
#'     suitable for comparison of wells.
#'
#' @param plateData A list containing data frames with amplitude values from
#'     selected wells that is to be compared. One data frame per well.
#' @param thresholds A data.frame containing the thresholds for the selected wells
#' @param channel What channel to plot,  'target' channel or 'ontrol' channel.
#' @param sampleID The sample IDs for the selected wells.
#' @param colCh The channel from the instrument to plot. Controls color of plot.
#'
#' @return Faceted scatterplot with line indicating threshold. One facet per
#'     selected well.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_blank geom_hline
#'     scale_color_manual facet_wrap vars
#' @importFrom rlist list.stack
#'
#' @examples
#'
#' ## Set path to data
#' path <- system.file("extdata", "Amplitudes/", package="PoDCall")
#'
#' ## Read in data files
#' data <- importAmplitudeData(path, skipLines=0)
#' data("thrTable")
#'
#' ## Create plot using threshold from thrTable, see ?thrTable
#' plot <- podcallMultiplot(plateData=data,
#'                         thresholds=thrTable[names(data), ],
#'                         channel="target")
#'
podcallMultiplot <- function(plateData, thresholds,
                            channel=c("target", "control"),
                            sampleID=NULL,
                            colCh=c(1,2,3,4,5,6)){

    checkArgumentsMultiplot(plateData, thresholds, channel, sampleID)

    if(channel == "target")
        ch <- 1
    if(channel == "control")
        ch <- 2

    ## Check for data in channel 2
    if(ch == 2){
        if(any(is.na(thresholds)))
            warning("Missing thresholds for for channel 2")#; return(NULL)
    }

    #paste0(names(plateData),"_", sampleID)

    ## Get channel data, add columns with well ID and breaks to color droplets
    plateCh <-
        mapply(function(x, i, j)
        {data.frame(wellID=i, plotID=paste0(i,"_",j),
                    Amplitudes=x[,c(1, 2)[ch]],
                    col=cut(x[,c(1, 2)[ch]],
                            breaks=c(-Inf,
                                    thresholds[i, c("thr_target",
                                                    "thr_ctrl")[ch]],
                                    Inf),
                            labels=c("(-Inf, thr]", "[thr, Inf)")))},
        x=plateData, i=names(plateData), j=sampleID, SIMPLIFY=FALSE)

    ## Stack elements of channel list to create long format data frame
    plateChStacked <- rlist::list.stack(plateCh)

    ## Add Well ID to threshold data frame
    thrDf <- data.frame(thresholds, wellID=names(plateCh))

    ## Select data and plotting variables based on channel selection from user
    rows <- sample(nrow(plateChStacked))
    dd <- plateChStacked[rows, ]
    thrDfCh <- thrDf[, c(c("thr_target", "thr_ctrl")[ch], "wellID")]
    colnames(thrDfCh)[1] <- "thrCh"
    chCol <- c("dodgerblue3", "forestgreen", "orangered1",
               "red1", "deeppink3", "cyan2") # Plot color for channels

    ## Work-around for "no visible binding for global variable NOTE
    Amplitudes <- NULL; wellID <- NULL; thrCh <- NULL

    # Lag en named vector: wellID -> sampleID (én unik entry per wellID)
    wellID_labels <- dd %>%
        dplyr::distinct(wellID, plotID) %>%
        { setNames(as.character(.$plotID), as.character(.$wellID)) }

    ## Faceted scatter plot of amplitude values for the selected wells
    multiplot <-
        ggplot(data=dd, aes(x=seq_len(nrow(dd)), y=Amplitudes, group=wellID,
                        color=col)) +
        geom_point(size=1) +
        labs(x="Event", y="Amplitude", color=NULL)+
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position="none")+
        geom_hline(data=thrDfCh, aes(yintercept=thrCh), col="magenta")+
        scale_color_manual(labels=c("neg", "pos"),
                            values=c("gray50", chCol[colCh]))+
        facet_wrap(~ wellID, ncol=10,
                   labeller=as_labeller(wellID_labels))

    return(multiplot)
}

## Internal functions

## Check arguments to podcallMultiplot()
checkArgumentsMultiplot <- function(plateData, thresholds, channel, sampleID){

    ## Check arguments
    if(!is.list(plateData)) stop("plateData must be a list")
    if(length(plateData) != nrow(thresholds)) stop("Number of elements in
    plateData must be equal to number of rows in thresholds")
    if(!(channel %in% c("target", "control"))) stop("invalid channel")
    if(!is.data.frame(thresholds)) stop("thresholds must be a data.frame")
    if(!("thr_target" %in% colnames(thresholds))) stop("thresholds must contain
                                                        column 'thr_target'")
    if(!("thr_ctrl" %in% colnames(thresholds))) stop("thresholds must contain
                                                        column 'thr_ctrl'")
    if(length(sampleID) != length(plateData)) stop("number of sample IDs must
                                                be same as number of selected
                                                wells")

    return(NULL)
}
