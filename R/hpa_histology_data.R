#' HPA histology dataset version 18
#' 
#' Dataset downloaded with \code{hpaDownload('histology', version = 'v18')}.
#' This dataset is kept for the sake of backward compability. Please use
#' `hpa_histology_data` for the most updated built-in dataset.
#' 
#' @format A list of 3 tibbles
#' \describe{
#'     \item{normal_tissue}{Normal tissue IHC data}
#'     \item{pathology}{Cancer IHC data}
#'     \item{subcellular_location}{Subcellular location IF data}
#' }
#'
#' @details Links to original data:
#' \itemize{
#'     \item {https://v18.proteinatlas.org/download/normal_tissue.tsv.zip}
#'     \item {https://v18.proteinatlas.org/download/pathology.tsv.zip}
#'     \item {https://v18.proteinatlas.org/download/subcellular_location.tsv.zip}
#' }
#' 
#' @seealso
#' \code{\link{hpaDownload}}
#' \code{\link{hpa_histology_data}}
#' 
#' @examples 
#'   # load data
#'   data("hpa_downloaded_histology_v18")
#'   
#'   # access data frames
#'   normal_tissue_data <- hpa_downloaded_histology_v18$normal_tissue
#'   cancer_data <- hpa_downloaded_histology_v18$pathology
#'   subcell_location_data <- hpa_downloaded_histology_v18$subcellular_location

"hpa_downloaded_histology_v18"

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