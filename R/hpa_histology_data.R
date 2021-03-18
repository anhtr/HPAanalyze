#' HPA histology dataset
#'
#' Dataset downloaded with \code{hpaDownload('histology', version = 'latest')}.
#' This should be the most updated dataset at the time of generation. Check
#' metadata for more information.
#'
#' @format A list of 3 tibbles \describe{ \item{normal_tissue}{Normal tissue IHC
#'   data} \item{pathology}{Cancer IHC data}
#'   \item{subcellular_location}{Subcellular location IF data} }
#'
#' @seealso \code{\link{hpaDownload}}
#'
#' @examples
#'   # load data
#'   data("hpa_histology_data")
#'
#'   # access data frames
#'   normal_tissue_data <- hpa_histology_data$normal_tissue
#'   cancer_data <- hpa_histology_data$pathology
#'   subcell_location_data <- hpa_histology_data$subcellular_location
#'   
#'   # see metadata
#'   hpa_histology_data$metadata

"hpa_histology_data"