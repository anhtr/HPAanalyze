## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  warning=FALSE,
  error=FALSE,
  eval=FALSE
)

## ----library, message=FALSE, warning=FALSE, error=FALSE-----------------------
#  library(BiocStyle)
#  library(HPAanalyze)
#  library(dplyr)
#  library(jsonlite)

## -----------------------------------------------------------------------------
#  data <- hpaDownload(downloadList = "histology", version = "example")
#  data_subset <-
#    hpaSubset(data,
#              targetGene = c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1'))

## -----------------------------------------------------------------------------
#  data_json <- lapply(data_subset, jsonlite::toJSON)
#  
#  str(data_json)
#  
#  # List of 3
#  #  $ normal_tissue       : 'json' chr "[{\"ensembl\":\"ENSG00000026508\",\"gene\":\"CD44\",\"tissue\":\"adrenal gland\",\"cell_type\":\"glandular cell"| __truncated__
#  #  $ pathology           : 'json' chr "[{\"ensembl\":\"ENSG00000026508\",\"gene\":\"CD44\",\"cancer\":\"breast cancer\",\"high\":1,\"medium\":6,\"low\"| __truncated__
#  #  $ subcellular_location: 'json' chr "[{\"ensembl\":\"ENSG00000026508\",\"gene\":\"CD44\",\"reliability\":\"Enhanced\",\"enhanced\":\"Golgi apparatus"| __truncated__

## -----------------------------------------------------------------------------
#  for (i in seq_along(data_json)) {
#    write(data_json[[i]],
#          file = paste0("hpa_data_", names(data_json[i]), ".json"))
#  }

## -----------------------------------------------------------------------------
#  ## The function (note that you don't need to put .json into the file name)
#  hpaExportJSON <- function(data, fileName) {
#    data_json <- lapply(data, jsonlite::toJSON)
#    for (i in seq_along(data_json)) {
#      write(data_json[[i]],
#            file = paste0(fileName, "_", names(data_json[i]), ".json"))
#    }
#  }
#  
#  ## Export data subset
#  hpaExportJSON(data_subset, fileName = "hpa_data")

