#' @title PoDCall Example Threshold Table
#'
#' @description A \code{data.frame} that contains the results of running PodCall
#' with the amplitude data files included in the package. For testing and
#' running of examples. See vignette for more detailed description about
#' columns.
#'
#' @format A \code{data.frame} with 12 columns, which are:
#' \describe{
#' \item{sample_id}{Sample ID}
#' \item{thr_target}{Threshold channel 1 (target assay)}
#' \item{thr_ctrl}{Threshold channel 2 (control assay)}
#' \item{pos_dr_target}{Positive droplets target}
#' \item{pos_dr_ctrl}{Positive droplets control}
#' \item{tot_droplets}{Total droplets}
#' \item{c_target}{Concentration target}
#' \item{c_ctrl}{Concentration control}
#' \item{c_norm}{Normalized concentration}
#' \item{q}{Parameter Q for calling outliers}
#' \item{target_assay}{Target assay}
#' \item{ctrl_assay}{Control assay}
#' }
#'
#' @source In-house cell-line experiment.
#'
#'
"thrTable"
