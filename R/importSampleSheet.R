#' importSampleSheet
#'
#' @description Function that takes a path to a .csv-file containing information
#'     about the samples that correspond to the uploaded amplitude files. This
#'     file must contain the following columns: Well, Sample, TargetType
#'     and Target. A character vector with well IDs must also be provided,
#'     which is used to match rows in sample sheet to amplitude files
#'
#' @param sampleSheet Path to sample sheet file containing information about
#'     samples.
#' @param well_id Character vector with well IDs corresponding to uploaded
#'     amplitude files.
#' @param software Name (character) of software data and sample sheet was
#'     exported from. Must be either 'QuantaSoft' or 'QX Manager'. Be careful
#'     to use correct spelling.
#'
#' @importFrom readr read_csv
#'
#' @return A matrix with columns for sample ID, target assay and control assay.
#'
#' @export
#'
#' @examples
#'## Path to example sample sheet included in PoDCall
#'path <- system.file("extdata", "Sample_names.csv", package="PoDCall")
#'
#'## Select wells to get information for
#'well_id <- c("A04", "B04", "D04")
#'
#'## Get information for selected wells
#'sampleSheet <- importSampleSheet(sampleSheet=path, well_id=well_id,
#'                                  software="QuantaSoft")
#'
importSampleSheet <- function(sampleSheet=NULL, well_id=NULL,
                                software=c("QuantaSoft", "QX Manager")[2]){
    ## Check arguments
    if(!is.character(sampleSheet) & !is.null(sampleSheet))
        stop("'sampleSheet' must be character")
    if(!is.character(well_id))
        stop("'well_id' must be character")
    if(!(software %in% c("QuantaSoft", "QX Manager")))
        stop("'software' must be either 'QuantaSoft' or 'QX Manager'")

    if(software == "QuantaSoft")
        sampleTable <- importSampleSheetQS(sampleSheet, well_id)

    if(software == "QX Manager")
        sampleTable <- importSampleSheetQXM(sampleSheet, well_id)

    return(sampleTable)
}


###############################################################################
## Internal functions

## Import sample sheet exported from QuantaSoft
importSampleSheetQS <- function(sampleSheet=NULL, well_id=NULL){

    ## Create vectors to hold sample information
    sample_id <- character(length(well_id))
    target_assay <- character(length(well_id))
    ctrl_assay <- character(length(well_id))

    if(!is.null(sampleSheet)){
        ## Read in table from file,
        ssTable <- suppressMessages(
            suppressWarnings(readr::read_csv(sampleSheet, col_names=TRUE),
                            classes=c("warning", "message")))

        if("Well" %in% colnames(ssTable) == FALSE){
            message("Column 'Well' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("Sample" %in% colnames(ssTable) == FALSE){
            message("Column 'Sample' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("TargetType" %in% colnames(ssTable) == FALSE){
            message("Column 'TargetType' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("Target" %in% colnames(ssTable) == FALSE){
            message("Column 'Target' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else{## Make data frame with relevant columns
            ssDf <- data.frame(ssTable[,c("Well", "Sample",
                                          "TargetType", "Target")],
                               stringsAsFactors=FALSE)
            ## Order by well
            ssDfOrdered <- ssDf[order(ssDf$Well),]
            ## Get columns with relevant information
            ssDfCh1 <-
                ssDfOrdered[which(ssDfOrdered$TargetType == "Ch1Unknown"),]
            ctrlAssayAll <-
                ssDfOrdered[which(ssDfOrdered$TargetType == "Ch2Unknown"),
                            "Target"]
            ## Get rows corresponding with amplitude files
            ssRows <- match(well_id, ssDfCh1$Well)
            sample_id <- ssDfCh1[ssRows, "Sample"]
            target_assay <- ssDfCh1[ssRows, "Target"]
            ctrl_assay <- ctrlAssayAll[ssRows]
        }
    }
    return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
}

## Import sample sheet exported from QX Manager
importSampleSheetQXM <- function(sampleSheet=NULL, well_id=NULL){

    ## Create vectors to hold sample information
    sample_id <- character(length(well_id))
    target_assay <- character(length(well_id))
    ctrl_assay <- character(length(well_id))

    if(!is.null(sampleSheet)){
        ## Read in table from file,
        ssTable <- suppressMessages(
            suppressWarnings(readr::read_delim(sampleSheet, delim=";",
                                               col_names=TRUE),
                            classes=c("warning", "message")))

        if("Well" %in% colnames(ssTable) == FALSE){
            message("Column 'Well' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("Sample description 1" %in% colnames(ssTable) == FALSE){
            message("Column 'Sample description 1' missing from provided sample
                    sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("DyeName(s)" %in% colnames(ssTable) == FALSE){
            message("Column 'DyeName(s)' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else if("Target" %in% colnames(ssTable) == FALSE){
            message("Column 'Target' missing from provided sample sheet")
            return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
        }else{## Make data frame with relevant columns
            ssDf <- data.frame(ssTable[,c("Well", "Sample description 1",
                                          "DyeName(s)", "Target")],
                               stringsAsFactors=FALSE)
            ## Order by well
            ssDfOrdered <- ssDf[order(ssDf$Well),]
            ## Get columns with relevant information
            ssDfCh1 <-
                ssDfOrdered[which(ssDfOrdered$DyeName.s. == "FAM"),]
            ctrlAssayAll <-
                ssDfOrdered[which(ssDfOrdered$DyeName.s. == "VIC"),
                            "Target"]
            ## Get rows corresponding with amplitude files
            ssRows <- match(well_id, ssDfCh1$Well)
            sample_id <- ssDfCh1[ssRows, "Sample.description.1"]
            target_assay <- ssDfCh1[ssRows, "Target"]
            ctrl_assay <- ctrlAssayAll[ssRows]
        }
    }
    return(data.frame(well_id, sample_id, target_assay, ctrl_assay))
}
