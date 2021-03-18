## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  warning=FALSE,
  error=FALSE,
  eval=FALSE,
  crop = NULL
)

## ----library, message=FALSE, warning=FALSE, error=FALSE-----------------------
#  library(BiocStyle)
#  library(HPAanalyze)
#  library(dplyr)
#  library(tibble)
#  library(readr)
#  library(tidyr)

## ----echo=FALSE, eval=TRUE, fig.cap="The 'Fields >>' button.", out.width = '100%'----
knitr::include_graphics("figures/query_fields.png")

## ----echo=FALSE, eval=TRUE, fig.cap="Build your query with the drop-down menus.", out.width = '100%'----
knitr::include_graphics("figures/query_dropdown.png")

## ----echo=FALSE, eval=TRUE, fig.cap="Click the 'Search' button.", out.width = '100%'----
knitr::include_graphics("figures/query_search.png")

## ----echo=FALSE, eval=TRUE, fig.cap="Copy the link to the tsv file.", out.width = '100%'----
knitr::include_graphics("figures/query_tsvlink.png")

## -----------------------------------------------------------------------------
#  ## The link to your query tsv
#  my_hpa_query <- "https://www.proteinatlas.org/search/protein_class%3ACD+markers+AND+normal_expression%3ACerebral+cortex%3BAny%3BNot+detected%2CLow+AND+prognostic%3AGlioma%3BUnfavourable?format=tsv"
#  
#  ## Create a temporary file as destination for the download
#  temp <- tempfile("query", fileext=c(".tsv.gz"))
#  
#  ## Download to the temporary file
#  download.file(url=my_hpa_query, destfile = temp, method = "curl", mode = "wb")
#  
#  ## read the file into a data frame
#  query_df <- readr::read_tsv(temp)
#  
#  ## Unlink the temp file
#  unlink(temp)

## -----------------------------------------------------------------------------
#  tibble::glimpse(query_df)
#  
#  #> Observations: 6
#  #> Variables: 22
#  #> $ Gene                        <chr> "CD81", "NRP1", "PRNP", "SDC1", "THY...
#  #> $ `Gene synonym`              <chr> "TAPA-1, TAPA1, TSPAN28", "CD304, NR...
#  #> $ Ensembl                     <chr> "ENSG00000110651", "ENSG00000099250"...
#  #> $ `Gene description`          <chr> "CD81 molecule", "Neuropilin 1", "Pr...
#  #> $ Chromosome                  <dbl> 11, 10, 20, 2, 11, 17
#  #> $ Position                    <chr> "2376177-2397419", "33177492-3333626...
#  #> $ `Protein class`             <chr> "CD markers, Disease related genes, ...
#  #> $ Evidence                    <chr> "Evidence at protein level", "Eviden...
#  #> $ Antibody                    <chr> "CAB002507, HPA007234", "CAB004511, ...
#  #> $ `Reliability (IH)`          <chr> "Supported", "Approved", "Enhanced",...
#  #> $ `Reliability (Mouse Brain)` <lgl> NA, NA, NA, NA, NA, NA
#  #> $ `Reliability (IF)`          <chr> "Supported", "Uncertain", "Approved"...
#  #> $ `Subcellular location`      <chr> "Plasma membrane", "Mitochondria", "...
#  #> $ `Prognostic p-value`        <chr> "Glioma:5.12e-5 (unfavourable), Panc...
#  #> $ `RNA cancer category`       <chr> "Expressed in all", "Expressed in al...
#  #> $ `RNA tissue category`       <chr> "Expressed in all", "Expressed in al...
#  #> $ `RNA TS`                    <lgl> NA, NA, NA, NA, NA, NA
#  #> $ `RNA TS TPM`                <chr> NA, NA, NA, "esophagus: 250.7;skin: ...
#  #> $ `TPM max in non-specific`   <chr> "seminal vesicle: 2273.0", "placenta...
#  #> $ `RNA cell line category`    <chr> "Cell line enhanced", "Cell line enh...
#  #> $ `RNA CS`                    <lgl> NA, NA, NA, NA, NA, NA
#  #> $ `RNA CS TPM`                <chr> "ASC diff: 2031.3", "U-87 MG: 437.4"...

## -----------------------------------------------------------------------------
#  ## since the query give you the latest HPA version, get the latest datasets to match
#  latest_datasets <- hpaDownload()
#  
#  hpaVis(data = latest_datasets,
#         targetGene = query_df$Gene,
#         targetTissue = "cerebral cortex",
#         targetCancer = "glioma")

## -----------------------------------------------------------------------------
#  ## Download and import the xml files for proteins of interest
#  query_xml_list <- lapply(query_df$Ensembl, hpaXmlGet)
#  
#  ## Extract protein classes as a list of data frame
#  query_protein_classes <- lapply(query_xml_list, hpaXmlProtClass)
#  names(query_protein_classes) <- query_df$Gene # name list items
#  
#  ## Turn the list into a data frame
#  query_protein_classes_df <-
#      tidyr::unnest(tibble::enframe(query_protein_classes, name = "protein"))
#  
#  glimpse(query_protein_classes_df)
#  
#  #> Observations: 122
#  #> Variables: 5
#  #> $ protein   <chr> "CD81", "CD81", "CD81", "CD81", "CD81", "CD81", "CD81"...
#  #> $ id        <chr> "Cd", "Ja", "Jf", "Ma", "Md", "Me", "Mf", "Mg", "Mh", ...
#  #> $ name      <chr> "CD markers", "Transporters", "Accessory Factors Invol...
#  #> $ parent_id <chr> NA, NA, "Ja", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
#  #> $ source    <chr> "UniProt", "TCDB", "TCDB", "MDM", "MDM", "MEMSAT3", "M...
#  
#  
#  ## Which proteins in our list are also potential drug targets?
#  filter(query_protein_classes_df, name == "Potential drug targets")
#  
#  #> # A tibble: 2 x 5
#  #>   protein id    name                   parent_id source
#  #>   <chr>   <chr> <chr>                  <chr>     <chr>
#  #> 1 CD81    Pd    Potential drug targets <NA>      HPA
#  #> 2 PRNP    Pd    Potential drug targets <NA>      HPA

