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
#  library(xml2)

## -----------------------------------------------------------------------------
#  ## same as hpaXmlGet("ENSG00000134057")
#  CCNB1xml <- xml2::read_xml("data/ENSG00000134057.xml")

## -----------------------------------------------------------------------------
#  CCNB1_parsed <- hpaXml(CCNB1xml)

## -----------------------------------------------------------------------------
#  hpaXmlProtClass(CCNB1xml)
#  hpaXmlTissueExprSum(CCNB1xml)
#  hpaXmlAntibody(CCNB1xml)
#  hpaXmlTissueExpr(CCNB1xml)

## -----------------------------------------------------------------------------
#  saveRDS(CCNB1_parsed, "data/CCNB1_parsed.rds")

