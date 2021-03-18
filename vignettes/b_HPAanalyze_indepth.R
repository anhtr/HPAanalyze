## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  warning=FALSE,
  message=FALSE,
  error=FALSE,
  crop = NULL
)

## ----library------------------------------------------------------------------
library(BiocStyle)
library(HPAanalyze)
library(tibble)
library(dplyr)
library(ggplot2)

## ----echo=FALSE, fig.cap="HPAanalyze workflow.", out.width = '100%'-----------
knitr::include_graphics("figures/workflow.png")

## ----downloadedData, eval=FALSE-----------------------------------------------
#  # this gives you the latest everything, which is nice to keep but not really necessary
#  downloadedData <- hpaDownload(downloadList='all')
#  summary(downloadedData)
#  
#  #>                          Length Class  Mode
#  #> normal_tissue             6     tbl_df list
#  #> pathology                11     tbl_df list
#  #> subcellular_location     11     tbl_df list
#  #> rna_tissue                5     tbl_df list
#  #> rna_cell_line             5     tbl_df list
#  #> transcript_rna_tissue     4     tbl_df list
#  #> transcript_rna_cell_line  4     tbl_df list

## ----histology----------------------------------------------------------------
downloadedData <- hpaDownload(downloadList='histology', version='example')
# version = "example" will load the HPA v18 datasets came with this package. That's sufficient for normal usage, and save you some time.

## ----normal_tissue------------------------------------------------------------
tibble::glimpse(downloadedData$normal_tissue, give.attr=FALSE)

## ----pathology----------------------------------------------------------------
tibble::glimpse(downloadedData$pathology, give.attr=FALSE)

## ----subcellular_location-----------------------------------------------------
tibble::glimpse(downloadedData$subcellular_location, give.attr=FALSE)

## ----rna1, warning=FALSE, message=FALSE---------------------------------------
downloadedData <- hpaDownload(downloadList='rna', version='v18')

tibble::glimpse(downloadedData$rna_tissue, give.attr=FALSE)

## ----rna2, warning=FALSE, message=FALSE---------------------------------------
tibble::glimpse(downloadedData$rna_cell_line, give.attr=FALSE)

## ----isoform1, eval=FALSE-----------------------------------------------------
#  downloadedData <- hpaDownload(downloadList='isoform', version='v18')
#  # version = "v18" is an example of how you may download different versions of the HPA datasets. Just change the number. Note that not all versions are available from the HPA website.
#  
#  tibble::glimpse(downloadedData$transcript_rna_tissue, give.attr=FALSE)
#  
#  #> Observations: 27,535,996
#  #> Variables: 4
#  #> $ ensembl    <chr> "ENSG00000000003", "ENSG00000000003", "ENSG0000000...
#  #> $ transcript <chr> "ENST00000373020", "ENST00000494424", "ENST0000049...
#  #> $ tissue     <chr> "adipose tissue.V1", "adipose tissue.V1", "adipose...
#  #> $ value      <dbl> 27.3577003, 0.0000000, 1.9341500, 1.6059300, 0.000...

## ----isoform2, eval=FALSE-----------------------------------------------------
#  tibble::glimpse(downloadedData$transcript_rna_cell_line, give.attr=FALSE)
#  
#  #> Observations: 20,972,183
#  #> Variables: 4
#  #> $ ensembl    <chr> "ENSG00000000003", "ENSG00000000003", "ENSG0000000...
#  #> $ transcript <chr> "ENST00000373020", "ENST00000494424", "ENST0000049...
#  #> $ cell_line  <chr> "A-431.C35", "A-431.C35", "A-431.C35", "A-431.C35"...
#  #> $ value      <dbl> 29.406799, 0.000000, 0.992916, 0.398387, 0.239204,...

## ----list_param, eval=FALSE---------------------------------------------------
#  ## If you use the output from hpaDownload()
#  downloadedData <- hpaDownload(downloadList='all')
#  str(hpaListParam(downloadedData))
#  
#  #> List of 6
#  #>  $ normal_tissue       : chr [1:58] "adrenal gland" "appendix" "bone marrow" "breast" ...
#  #>  $ normal_cell         : chr [1:82] "glandular cells" "lymphoid tissue" "hematopoietic cells" "adipocytes" ...
#  #>  $ cancer              : chr [1:20] "breast cancer" "carcinoid" "cervical cancer" "colorectal cancer" ...
#  #>  $ subcellular_location: chr [1:32] "Cytosol" "Mitochondria" "Aggresome" "Plasma membrane" ...
#  #>  $ normal_tissue_rna   : chr [1:37] "adipose tissue" "adrenal gland" "appendix" "bone marrow" ...
#  #>  $ cell_line_rna       : chr [1:64] "A-431" "A549" "AF22" "AN3-CA" ...

## ----list_param_2-------------------------------------------------------------
## If you use leave the argument blank
str(hpaListParam())

## ----subset1, message=FALSE, warning=FALSE------------------------------------
downloadedData <- hpaDownload(downloadList='histology', version='example')
sapply(downloadedData, nrow)

## ----subset2, message=FALSE, warning=FALSE------------------------------------
geneList <- c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1', 'IDH2', 'CYCS')
tissueList <- c('breast', 'cerebellum', 'skin 1')
cancerList <- c('breast cancer', 'glioma', 'melanoma')
cellLineList <- c('A-431', 'A549', 'AF22', 'AN3-CA')

subsetData <- hpaSubset(data=downloadedData,
                         targetGene=geneList,
                         targetTissue=tissueList,
                         targetCancer=cancerList,
                         targetCellLine=cellLineList)
sapply(subsetData, nrow)

## ----eval=FALSE---------------------------------------------------------------
#  hpaExport(subsetData, fileName='subset.xlsx', fileType='xlsx')

## ----visData, echo=FALSE, warning=FALSE, message=FALSE------------------------
downloadedData <- hpaDownload('histology', 'example')

## ----hpaVis_eg----------------------------------------------------------------
hpaVis(targetGene = c("GCH1", "PTS", "SPR", "DHFR"),
       targetTissue = c("cerebellum", "cerebral cortex", "hippocampus"),
       targetCancer = c("glioma"))

## ----visTissue----------------------------------------------------------------
geneList <- c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1', 'IDH2', 'CYCS')
tissueList <- c('breast', 'cerebellum', 'skin 1')

hpaVisTissue(downloadedData,
             targetGene=geneList,
             targetTissue=tissueList)

## ----visPatho-----------------------------------------------------------------
geneList <- c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1', 'IDH2', 'CYCS')
cancerList <- c('breast cancer', 'glioma', 'lymphoma', 'prostate cancer')
colorGray <- c('slategray1', 'slategray2', 'slategray3', 'slategray4')

hpaVisPatho(downloadedData,
            targetGene=geneList,
            targetCancer=cancerList,
            color=colorGray)

## ----visSubcell---------------------------------------------------------------
geneList <- c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1', 'IDH2', 'CYCS')

hpaVisSubcell(downloadedData,
              targetGene=geneList,
              customTheme=TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::ylab('Subcellular locations') +
    ggplot2::xlab('Protein') +
    ggplot2::theme(axis.text.x=element_text(angle=45, hjust=1))  +
    ggplot2::theme(legend.position="none") +
    ggplot2::coord_equal()

## ----eval=FALSE---------------------------------------------------------------
#  EGFR <- hpaXml(inputXml='ENSG00000146648')
#  names(EGFR)
#  
#  #> [1] "ProtClass"     "TissueExprSum" "Antibody"      "TissueExpr"

## ----XmlGet, eval=FALSE-------------------------------------------------------
#  EGFRxml <- hpaXmlGet('ENSG00000146648')

## ----XmlProtClass, eval=FALSE-------------------------------------------------
#  hpaXmlProtClass(EGFRxml)
#  
#  #> # A tibble: 40 x 4
#  #>    id    name                                   parent_id source
#  #>    <chr> <chr>                                  <chr>     <chr>
#  #>  1 Ez    Enzymes                                <NA>      <NA>
#  #>  2 Ec    ENZYME proteins                        Ez        ENZYME
#  #>  3 Et    Transferases                           Ec        ENZYME
#  #>  4 Ki    Kinases                                Ez        UniProt
#  #>  5 Kt    Tyr protein kinases                    Ki        UniProt
#  #>  6 Ma    Predicted membrane proteins            <NA>      MDM
#  #>  7 Md    Membrane proteins predicted by MDM     <NA>      MDM
#  #>  8 Me    MEMSAT3 predicted membrane proteins    <NA>      MEMSAT3
#  #>  9 Mf    MEMSAT-SVM predicted membrane proteins <NA>      MEMSAT-SVM
#  #> 10 Mg    Phobius predicted membrane proteins    <NA>      Phobius
#  #> # ... with 30 more rows

## ----XmlTissueExprSum, eval=FALSE---------------------------------------------
#  hpaXmlTissueExprSum(EGFRxml)
#  
#  #> $summary
#  #> [1] "Cytoplasmic and membranous expression in several tissues, most abundant in placenta."
#  #>
#  #> $img
#  #>            tissue
#  #> 1 cerebral cortex
#  #> 2      lymph node
#  #> 3           liver
#  #> 4           colon
#  #> 5          kidney
#  #> 6          testis
#  #> 7        placenta
#  #>                                                                imageUrl
#  #> 1 http://v18.proteinatlas.org/images/18530/41191_B_7_5_rna_selected.jpg
#  #> 2 http://v18.proteinatlas.org/images/18530/41191_A_7_8_rna_selected.jpg
#  #> 3 http://v18.proteinatlas.org/images/18530/41191_A_7_4_rna_selected.jpg
#  #> 4 http://v18.proteinatlas.org/images/18530/41191_A_9_3_rna_selected.jpg
#  #> 5 http://v18.proteinatlas.org/images/18530/41191_A_9_5_rna_selected.jpg
#  #> 6 http://v18.proteinatlas.org/images/18530/41191_A_6_6_rna_selected.jpg
#  #> 7 http://v18.proteinatlas.org/images/18530/41191_A_1_7_rna_selected.jpg

## ----XmlAntibody, eval=FALSE--------------------------------------------------
#  hpaXmlAntibody(EGFRxml)
#  
#  #> # A tibble: 5 x 4
#  #>   id        releaseDate releaseVersion RRID
#  #>   <chr>     <chr>       <chr>          <chr>
#  #> 1 CAB000035 2006-03-13  1.2            <NA>
#  #> 2 HPA001200 2008-02-15  3.1            AB_1078723
#  #> 3 HPA018530 2008-12-03  4.1            AB_1848044
#  #> 4 CAB068186 2014-11-06  13             AB_2665679
#  #> 5 CAB073534 2015-10-16  14             <NA>

## ----XmlTissueExpr1, eval = FALSE---------------------------------------------
#  tissueExpression <- hpaXmlTissueExpr(EGFRxml)
#  summary(tissueExpression)
#  
#  #>      Length Class  Mode
#  #> [1,] 18     tbl_df list
#  #> [2,] 18     tbl_df list
#  #> [3,] 18     tbl_df list
#  #> [4,] 18     tbl_df list
#  #> [5,] 18     tbl_df list

## ----XmlTissueExpr2, eval = FALSE---------------------------------------------
#  tissueExpression[[1]]
#  
#  #> # A tibble: 327 x 18
#  #>    patientId age   sex   staining intensity quantity location imageUrl
#  #>    <chr>     <chr> <chr> <chr>    <chr>     <chr>    <chr>    <chr>
#  #>  1 1653      53    Male  <NA>     <NA>      <NA>     <NA>     http://~
#  #>  2 1721      60    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #>  3 1725      57    Male  <NA>     <NA>      <NA>     <NA>     http://~
#  #>  4 4         25    Male  <NA>     <NA>      <NA>     <NA>     http://~
#  #>  5 512       34    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #>  6 2664      74    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #>  7 2665      88    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #>  8 1391      54    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #>  9 1447      45    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #> 10 1452      44    Fema~ <NA>     <NA>      <NA>     <NA>     http://~
#  #> # ... with 317 more rows, and 10 more variables: snomedCode1 <chr>,
#  #> #   snomedCode2 <chr>, snomedCode3 <chr>, snomedCode4 <chr>,
#  #> #   snomedCode5 <chr>, tissueDescription1 <chr>, tissueDescription2 <chr>,
#  #> #   tissueDescription3 <chr>, tissueDescription4 <chr>,
#  #> #   tissueDescription5 <chr>

