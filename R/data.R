#' HPA histology dataset
#' 
#' Dataset downloaded with \code{hpa_download('histology')}
#' 
#' @format A list of 3 tibbles
#' \describe{
#'     \item{normal_tissue}{Normal tissue IHC data}
#'     \item{pathology}{Cancer IHC data}
#'     \item{subcellular_location}{Subcellular location IF data}
#'     }

"hpa_downloaded_histology_v18"

#' HPA RNA dataset
#' 
#' Dataset downloaded with \code{hpa_download('rna')}
#' 
#' @format A list of 2 tibbles
#' \describe{
#'     \item{rna_tissue}{Normal tissue RNAseq data}
#'     \item{rna_cell_line}{Cell line RNA seq data}
#'     }

"hpa_downloaded_rna_v18"