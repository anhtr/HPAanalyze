## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  warning=FALSE,
  error=FALSE,
  crop = NULL
)

## ----library, message=FALSE, warning=FALSE, error=FALSE-----------------------
library(BiocStyle)
library(HPAanalyze)
library(dplyr)

## ----hpaVis_eg----------------------------------------------------------------
hpaVis(targetGene = c("GCH1", "PTS", "SPR", "DHFR"),
       targetTissue = c("cerebellum", "cerebral cortex", "hippocampus"),
       targetCancer = c("glioma"))

## ----hpaVis_eg2, eval = FALSE-------------------------------------------------
#  hpaVis()
#  
#  # No data provided. Use version 18.
#  # targetGene variable not specified, default to TP53, RB1, MYC, KRAS and EGFR.
#  # targetTissue variable not specified, default to breast.
#  # targetCellType variable not specified, visualize all.
#  # targetCancer variable not specified, default to breast cancer
#  # Use hpaListParam() to list possible values for target variables.
#  

## ----hpaVis_eg3---------------------------------------------------------------
hpaVis(visType = "Patho",
       targetGene = c("GCH1", "PTS", "SPR", "DHFR"),
       targetCancer = c("glioma", "breast cancer"))

## ----hpaVisPatho_eg-----------------------------------------------------------
hpaVisPatho(targetGene = c("GCH1", "PTS", "SPR", "DHFR"))

## ----doc_ex1, eval = FALSE----------------------------------------------------
#  ?hpaVis # the easy umbrella to visualize protein expression levels
#  ?hpaVisTissue # in normal tissue
#  ?hpaVisSubcell # in subcellular compartments
#  ?hpaVisPatho # in cancers

## ----listParam_ex, eval = FALSE-----------------------------------------------
#  hpaListParam()

## ----listParam_ex2, echo= FALSE-----------------------------------------------
hpaListParam() %>% glimpse()

## ----eval=FALSE---------------------------------------------------------------
#  EGFR <- hpaXml(inputXml='ENSG00000146648')
#  names(EGFR)
#  
#  #> [1] "ProtClass"     "TissueExprSum" "Antibody"      "TissueExpr"

## ----doc_ex2, eval = FALSE----------------------------------------------------
#  ?hpaXmlGet # import the xml file as "xml_document"
#  ?hpaXmlProtClass
#  ?hpaXmlTissueExprSum
#  ?hpaXmlAntibody
#  ?hpaXmlTissueExpr

