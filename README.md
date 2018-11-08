# HPAanalyze

`HPAanalyze` is an R package for retreiving and performing exploratory data analysis from The Human Protein Atlas program (HPA). It provides functionality for importing data tables and xml files from HPA, exporting and visualizing data, as well as download all staining images of interest. The package is free, open source, and available via Github. `HPAanalyze` intergrates into the R workflow via the `tidyverse` philosophy and data structures, and can be used in combination with Bioconductor packages for easy analysis of HPA data.  

## Background

The Human Protein Atlas (HPA) is a comprehensive resource for exploration of human proteome which contains a vast amount of proteomics and transcriptomics data generated from antibody-based tissue micro-array profiling and RNA deep-sequencing. 

The program has generated protein expression profiles in human normal tissues with cell type-specific expression patterns, cancer and cell lines via an innovative immunohistochemistry-based approach. These profiles are accompanied by a large collection of high quality histological staining images, annotated with clinical data and quantification. The database also includes classification of protein into both functional classes (such as transcription factors or kinases) and project-related classes (such as candidate genes for cancer). Starting from version 4.0, the HPA includes subcellular location profiles generated based on confocal images of immunofluorescent stained cells. Together, these data provide a detailed picture of protein expression in human cells and tissues, facilitating tissue-based diagnostis and research.

Data from the HPA are freely available via proteinatlas.org, allowing scientists to access and incorporate the data into their research. Previously, the R package *hpar* has been created for fast and easy programmatic access of HPA data. Here, we introduce *HPAanalyze*, an R package aims to simplify exploratory data analysis from those data, as well as provide other complementary functionality to *hpar*.

## Implementation

*HPAanalyze* is and R package with a MIT licence and is designed for easy retreiving and performing exploratory data analysis from HPA. It provides functionality for importing data tables and xml files from HPA, exporting and visualizing data, as well as download all staining images of interest. 

### The different HPA data formats

The Human Protein Atlas project provides data via two main mechanisms: *Full datasets* in the form of downloadable compressed tab-separated files (.tsv) and *individual entries* in XML, RDF and TSV formats. The full downloadable datasets includes normal tissue, pathology (cancer), subcellular location, RNA gene and RNA isoform data. For individual entries, the XML format is the most comprehensive, providing information on the target protein, antibodies, summary for each tissue and detailed data from each sample including clinical data, IHC scoring and image download links.

### *HPAanalyze* overview

*HPAanalyze* is designed to fullfill 3 main tasks: (1) Import, subsetting and export downloadable datasets; (2) Visualization of downloadable datasets for exploratory analysis; and (3) Working with the individual XML files. This package aims to serve researchers with little programming experience, but also allow power users to use the imported data as desired. Please see vignettes for details.

#### Obtaining *HPAanalyze*

The stable version of *HPAanalyze* is available via Bioconductor:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("HPAanalyze")
```

The development version of *HPAanalyze* is available on Github can be installed with: 
 
```
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("trannhatanh89/HPAanalyze")
```

#### Full dataset import, subsetting and export

The `hpaDownload()` function downloads full datasets from HPA and imports them into R as a list of tibbles, the standard object of *tidyverse*, which can subsequently be subset with `hpaSubset()` and export into .xmlx files with `hpaExport()`. The standard object allow the imported data to be further processed in a traditional R workflow. The ability to quickly subset and export data gives researchers the option to use other non-R downstream tools, such as GraphPad for creating publication-quality graphics, or share a subset of data containing only proteins of interest.

#### Visualization

The `hpaVis` function family take the output of `hpaDownload()` (or `hpaSubset()`) provides quick visualization of the data, with the intention of aiding exploratory analysis. Nevertheless, the standard `ggplot` object output of these functions give users the option to further customize the plots for publication. All `hpaVis` functions share the same syntax for arguments: subsetting, specifying colors and opting to use custom themes.

The first release of the *HPAanalyze* package includes three functions: `hpaVisTissue()` for the *normal tissue*, `hpaVisPatho()` for the *pathology/cancer*, and `hpaVisSubcell()` for the *subcellular location* datasets.

#### Individual xml import and image downloading

The `hpaXml` function family import and extract data from individual XML entries from HPA. The `hpaXmlGet()` function downloads and imports data as  "xml_document"/"xml_node" object, which can subsequently be processed by other `hpaXml` functions. The XML format from HPA contains a wealth of information that may not be covered by this package. However, users can extract any data of interest from the imported XML file using the xml2 package.

In the first release, *HPAanalyze* includes four functions for data extraction from HPA XML files: `hpaXmlProtClass()` for protein class information, `hpaTissueExprSum()` for summary of protein expression in tissue, `hpaXmlAntibody()` for a list of antibody used to stain for the protein of interest, and `hpaTissueExpr()` for a detailed data from each sample including clinical data and IHC scoring.

`hpaTissueExprSum` and `hpaTissueExpr` provide download links to download relevant staining images, with the former function also gives the options to automate the downloading process.


