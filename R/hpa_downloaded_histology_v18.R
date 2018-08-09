#' HPA histology dataset
#' 
#' Dataset downloaded with \code{hpaDownload('histology', version = 'v18')}.
#' 
#' @seealso Links to original data:
#' \itemize{
#'     \item {https://v18.proteinatlas.org/download/normal_tissue.tsv.zip}
#'     \item {https://v18.proteinatlas.org/download/pathology.tsv.zip}
#'     \item {https://v18.proteinatlas.org/download/subcellular_location.tsv.zip}
#' }
#' 
#' @format A list of 3 tibbles
#' \describe{
#'     \item{normal_tissue}{Normal tissue IHC data}
#'     \item{pathology}{Cancer IHC data}
#'     \item{subcellular_location}{Subcellular location IF data}
#' }
#' 
#' @examples 
#'   # load data
#'   data("hpa_downloaded_histology_v18")
#'   
#'   # access tibbes
#'   normal_tissue_data <- hpa_downloaded_histology_v18$normal_tissue
#'   cancer_data <- hpa_downloaded_histology_v18$pathology
#'   subcell_location_data <- hpa_downloaded_histology_v18$subcellular_location

"hpa_downloaded_histology_v18"