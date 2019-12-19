#######################
## Download datasets ##
#######################

#' Download datasets
#' 
#' Download the latest version of HPA datasets and import them in R. It is
#' recommended to only download the datasets you need, as some of them may be
#' very big.
#' 
#' @param downloadList A vector or string indicate which datasets to download.
#'   Possible value:
#'   \itemize{
#'     \item \code{'Normal tissue'}
#'     \item \code{'Pathology'}
#'     \item \code{'Subcellular location'}
#'     \item \code{'RNA tissue'}
#'     \item \code{'RNA cell line'}
#'     \item \code{'RNA transcript tissue'}
#'     \item \code{'RNA transcript cell line'}
#'     \item \code{'all'}: download everything
#'     \item \code{'histology'}: same as \code{c('Normal tissue', 'Pathology',
#'     'Subcellular location')}
#'     \item \code{'rna'}: same as \code{c('RNA tissue', 'RNA cell line')}
#'     \item \code{'isoform'}: same as \code{c('RNA transcript tissue', 'RNA
#'     transcript cell line')}
#'   }
#'   See \url{https://www.proteinatlas.org/about/download} for more information.
#'   
#' @param version A string indicate which version to be downloaded. Possible
#'   value:
#'   \itemize{
#'     \item \code{'hpar'}: Load data from the 'hpar' package. Does not contain
#'     isoform data.
#'     \item \code{'latest'}: Download latest version. Require Internet
#'     connection.
#'     \item \code{'example'}: Load the example dataset from 'HPA
#'     analyze' ('hpa_downloaded_histology_v18'). Does not contain rna or
#'     isoform data.
#'     \item \code{'v?'} with '?' is a integer: Download a specific version of
#'     the dataset. For example: 'v18' download version 18. Currently support
#'     version 17 and above. Require Internet connection.
#'   }
#' 
#' @family downloadable datasets functions
#' 
#' @return This function will return a list of data frames corresponding to
#'   requested datasets.
#'  
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   summary(downloadedData)
#'   
#' @import hpar
#' @importFrom readr read_tsv
#' @importFrom utils download.file data
#' @export
#' 

hpaDownload <- function(downloadList='histology', version='latest') {
    
    ## generate a list of item to download
    downloadList <- switch(downloadList,
                            all = c('Normal tissue', 
                                      'Pathology',
                                      'Subcellular location',
                                      'RNA tissue',
                                      'RNA cell line',
                                      'RNA transcript tissue',
                                      'RNA transcript cell line'),       
                            histology = c('Normal tissue', 
                                            'Pathology',
                                            'Subcellular location'),  
                            rna = c('RNA tissue',
                                      'RNA cell line'),
                            isoform = c('RNA transcript tissue',
                                          'RNA transcript cell line')
                           )
    
    # initiate the list to be returned
    loadedData <- list(normal_tissue = NA,
                       pathology = NA,
                       subcellular_location = NA,
                       rna_tissue = NA,
                       rna_cell_line = NA)
    
    # create list of colnames
    normalTissueColnames <- c('ensembl', 'gene', 'tissue', 
                              'cell_type', 'level', 'reliability')

    pathologyColnames <- c('ensembl', 'gene', 'cancer', 'high', 
                           'medium', 'low', 'not_detected', 
                           'prognostic_favorable', 
                           'unprognostic_favorable', 
                           'prognostic_unfavorable', 
                           'unprognostic_unfavorable')

    pathologyColnamesHpar <- c('ensembl', 'gene', 'cancer', 'high', 
                               'low', 'medium', 'not_detected')

    subcellularLocationColnames <- c('ensembl', 'gene', 'reliability', 
                                     'main_location', 'additional_location',
                                     'extracellular_location',
                                     'enhanced', 'supported', 'approved', 
                                     'uncertain', 'single_cell_var_intensity', 
                                     'single_cell_var_spatial', 
                                     'cell_cycle_dependency', 'go_id')

    subcellularLocationColnamesHpar <- c('ensembl', 'gene', 'reliability', 
                                         'enhanced', 'supported', 'approved', 
                                         'uncertain', 'single_cell_var_intensity', 
                                         'single_cell_var_spatial', 
                                         'cell_cycle_dependency', 'go_id')

    rnaTissueColnames <- c('ensembl', 'gene', 'tissue', 'value', 'unit')

    rnaCellLineColnames <- c('ensembl', 'gene', 'cell_line','value', 'unit')

    transcriptRnaTissueColnames <- c('ensembl', 'transcript', 'tissue', 'value')

    transcriptRnaCellLineColnames <- c('ensembl', 'transcript', 'cell_line', 'value')
    
    hpaNormalTissue <- hpaCancer <- hpaSubcellularLoc <- NULL
    rnaGeneTissue <- rnaGeneCellLine <- NULL

    # conditionally retrieve hpa data
    switch(version,
           hpar = { # load 'hpar' data
             if(!('package:hpar' %in% search())) {
               attachNamespace('hpar')
               # stop('Please load "hpar" package')
             }
             
             if('Normal tissue' %in% downloadList) {# load 'normal_tissue'
               data('hpaNormalTissue',
                    envir=environment())
               normal_tissue <- hpaNormalTissue
               colnames(normal_tissue) <- normalTissueColnames
               loadedData$normal_tissue <- normal_tissue
             }
             
             if('Pathology' %in% downloadList) {# load `pathology`
               data('hpaCancer',
                    envir=environment())
               pathology <- hpaCancer
               colnames(pathology) <- pathologyColnames
               loadedData$pathology <- pathology
             }
             
             if('Subcellular location' %in% downloadList) {# load 'subcellular_location'
               data('hpaSubcellularLoc',
                    envir=environment())
               subcellular_location <- hpaSubcellularLoc
               colnames(subcellular_location) <- subcellularLocationColnamesHpar
               subcellular_location$gene <- as.character(subcellular_location$gene)
               subcellular_location$go_id <- as.character(subcellular_location$go_id)
               loadedData$subcellular_location <- subcellular_location
             }
             
             if('RNA tissue' %in% downloadList) {# load 'rna_tissue'
               data('rnaGeneTissue',
                    envir=environment())
               rna_tissue <- rnaGeneTissue
               colnames(rna_tissue) <- rnaTissueColnames
               loadedData$rna_tissue <- rna_tissue
             }
             
             if('RNA cell line' %in% downloadList) { # load 'rna_cell_line'
               data('rnaGeneCellLine',
                    envir=environment())
               rna_cell_line <- rnaGeneCellLine
               colnames(rna_cell_line) <- rnaCellLineColnames
               loadedData$rna_cell_line <- rna_cell_line
             }   
           },
           
           example = {# load example data
             data('hpa_downloaded_histology_v18', 
                  package='HPAanalyze',
                  envir=environment())
             if('Normal tissue' %in% downloadList) {# load 'normal_tissue'
               loadedData$normal_tissue <- HPAanalyze::hpa_downloaded_histology_v18$normal_tissue
             }
             
             if('Pathology' %in% downloadList) {# load `pathology`
               loadedData$pathology <- HPAanalyze::hpa_downloaded_histology_v18$pathology
             }
             
             if('Subcellular location' %in% downloadList) {# load 'subcellular_location'
               loadedData$subcellular_location <- HPAanalyze::hpa_downloaded_histology_v18$subcellular_location
             }        
           },
           
           {   # download data from the internet
             
             # generate a vector of urls for download
             downloadUrls <- version_to_download_urls(version)
             
             # Check if the term is requested or not (all by default)
             # then download the file
             # then unzip and make a data frame out of data
             # then assign the data frame to the named list
             if('Normal tissue' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['normal_tissue'],
                             destfile=temp)
               normal_tissue <- data.frame(read_tsv(unz(temp, 'normal_tissue.tsv')))
               unlink(temp)
               colnames(normal_tissue) <- normalTissueColnames
               loadedData$normal_tissue <- normal_tissue
             }
             
             if('Pathology' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['pathology'],
                             destfile=temp)
               pathology <- data.frame(read_tsv(unz(temp, 'pathology.tsv')))
               unlink(temp)
               colnames(pathology) <- pathologyColnames
               loadedData$pathology <- pathology
             }
             
             if('Subcellular location' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['subcellular_location'],
                             destfile=temp)
               subcellular_location <- data.frame(read_tsv(unz(temp, 'subcellular_location.tsv')))
               unlink(temp)
               colnames(subcellular_location) <- subcellularLocationColnames
               loadedData$subcellular_location <- subcellular_location
             }
             
             if('RNA tissue' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['rna_tissue'],
                             destfile=temp)
               rna_tissue <- data.frame(read_tsv(unz(temp, 'rna_tissue.tsv')))
               unlink(temp)
               colnames(rna_tissue) <- rnaTissueColnames
               loadedData$rna_tissue <- rna_tissue
             }
             
             if('RNA cell line' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['rna_cell_line'],
                             destfile=temp)
               rna_cell_line <- data.frame(read_tsv(unz(temp, 'rna_celline.tsv')))
               unlink(temp)
               colnames(rna_cell_line) <- rnaCellLineColnames
               loadedData$rna_cell_line <- rna_cell_line
             }    
             
             if('RNA transcript tissue' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['transcript_rna_tissue'],
                             destfile=temp)
               transcript_rna_tissue <- data.frame(read_tsv(unz(temp, 'transcript_rna_tissue.tsv')))
               unlink(temp)
               transcript_rna_tissue <- matrix_melt(df1 = transcript_rna_tissue, 
                                                    key = c('ensgid', 'enstid'), 
                                                    indName = "tissue", 
                                                    valName = "value")
               colnames(transcript_rna_tissue) <- transcriptRnaTissueColnames
               loadedData$transcript_rna_tissue <- transcript_rna_tissue
             }
             
             if('RNA transcript cell line' %in% downloadList) {
               temp <- tempfile()
               download.file(url=downloadUrls['transcript_rna_cell_line'],
                             destfile=temp)
               transcript_rna_cell_line <- data.frame(read_tsv(unz(temp, 'transcript_rna_celline.tsv')))
               unlink(temp)
               transcript_rna_cell_line <- matrix_melt(df1 = transcript_rna_cell_line, 
                                                       key = c('ensgid', 'enstid'), 
                                                       indName = "cell_line", 
                                                       valName = "value")
               colnames(transcript_rna_cell_line) <- transcriptRnaCellLineColnames
               loadedData$transcript_rna_cell_line <- transcript_rna_cell_line
             }
             
             rm(temp)
           }
    )
    
    # remove empty objects from list
    loadedData <- loadedData[!is.na(loadedData)]

    return(loadedData)
}

#################
## Subset data ##
#################

#' Subset downloaded data
#'
#' \code{hpaSubset()} subsets data by gene name, tissue, cell type, cancer and/or
#' cell line. The input is the list object generated by \code{hpaDownload()} or
#' as the output of another \code{hpaSubset()}. Use \code{hpaListParam()} to see
#' the list of available parameters for a specific list object. Will not work on
#' isoform data.
#'
#' @param data Input the list object generated by \code{hpaDownload()} or
#'   \code{hpaSubset()}
#' @param targetGene Vector of strings of HGNC gene symbols. It will be used to
#'   subset every dataset in the list object. You can also mix HGNC gene symbols
#'   and ensemnl ids (start with ENSG) and they will be converted to HGNC gene
#'   symbols.
#' @param targetTissue Vector of strings of normal tissues. Will be used to
#'   subset the \code{normal_tissue} and \code{rna_tissue} dataset.
#' @param targetCellType Vector of strings of normal cell types. Will be used to
#'   subset the \code{normal_tissue} dataset.
#' @param targetCancer Vector of strings of cancer types. Will be used to subset
#'   the \code{pathology} dataset.
#' @param targetCellLine Vector of strings of cell lines. Will be used to subset
#'   the \code{rna_cell_line} dataset.
#'
#' @return \code{hpaSubset} will return a list of data frames as the result of
#'   subsetting, depending on the input data.
#'
#' @family downloadable datasets functions
#' 
#' @rdname hpaListParam
#'
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   geneList <- c('TP53', 'EGFR')
#'   tissueList <- c('breast', 'cerebellum', 'skin 1')
#'   cancerList <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subsetData <- hpaSubset(data=downloadedData,
#'                           targetGene=geneList,
#'                           targetTissue=tissueList,
#'                           targetCancer=cancerList)
#'
#' @export

hpaSubset <- function(data=NULL,
                      targetGene=NULL,
                      targetTissue=NULL,
                      targetCellType=NULL,
                      targetCancer=NULL,
                      targetCellLine=NULL) {
    
    # Check if data is provided or not
    if (is.null(data)) {
        message('No data provided. Use version 18.')
        data = HPAanalyze::hpa_downloaded_histology_v18
    }
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$normal_tissue <- 
                data$normal_tissue[data$normal_tissue$gene %in% targetGene,]
        }
        
        if(!is.null(targetTissue)) {
            data$normal_tissue <-
                data$normal_tissue[data$normal_tissue$tissue %in% targetTissue,]
        }
        
        if(!is.null(targetCellType)) {
            data$normal_tissue <-
                data$normal_tissue[data$normal_tissue$cell_type %in% targetCellType,]
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$pathology <-
                data$pathology[data$pathology$gene %in% targetGene,]
        }
        
        if(!is.null(targetCancer)) {
            data$pathology <-
                data$pathology[data$pathology$cancer %in% targetCancer,]
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$subcellular_location <-
                data$subcellular_location[data$subcellular_location$gene %in% targetGene,]
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$rna_tissue <-
                data$rna_tissue[data$rna_tissue$gene %in% targetGene,]
        }
        
        if(!is.null(targetTissue)) {
            data$rna_tissue <-
                data$rna_tissue[data$rna_tissue$tissue %in% targetTissue,]
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$rna_cell_line <-
                data$rna_cell_line[data$rna_cell_line$gene %in% targetGene,]
        }
        
        if(!is.null(targetCellLine)) {
            data$rna_cell_line <-
                data$rna_cell_line[data$rna_cell_line$cell_line %in% targetCellLine,]
        }       
    }
    
    return(data)
}


#########################
## List available data ##
#########################

#' List available data
#'
#' \code{hpaListParam()} list available variables in downloaded data that can be
#' used as parameters to subset the data via \code{hpaSubset()}. This function
#' work with the data object generated by \code{hpaDownload()} or a previous
#' call of \code{hpaSubset()}.
#'
#' @return The output of \code{hpaListParam()} is a list of vectors containing
#'   all subset parameter for the downloaded data.
#'
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   params <- hpaListParam(data=downloadedData)
#'   params$normal_tissue
#'
#' @importFrom stats na.omit
#' @export

hpaListParam <- function(data=NULL) {
    
    # Check if data is provided or not
    if (is.null(data)) {
        message('No data provided. Use version 18.')
        data = HPAanalyze::hpa_downloaded_histology_v18
    }
    
    availData <- list(normal_tissue = NULL, 
                      normal_cell = NULL,
                      cancer = NULL,
                      subcellular_location = NULL,
                      normal_tissue_rna = NULL,
                      cell_line_rna = NULL)
    
    if('normal_tissue' %in% names(data)) {
        availData$normal_tissue <- unique(data$normal_tissue[['tissue']])
        availData$normal_cell <- unique(data$normal_tissue[['cell_type']])
    }
    
    if('pathology' %in% names(data)) {
        availData$cancer <- unique(data$pathology[['cancer']])
    }
    
    if('subcellular_location' %in% names(data)) {
        availData$subcellular_location <- as.vector(
               na.omit(
                  unique(
                     unlist(
                        strsplit(
                           unique(data$subcellular_location[['approved']]),
                        ';')
                     )
                  )
               )
           )

    }
    
    if('rna_tissue' %in% names(data)) {
        availData$normal_tissue_rna <- unique(data$rna_tissue[['tissue']])
    }
    
    if('rna_cell_line' %in% names(data)) {
        availData$cell_line_rna <- unique(data$rna_cell_line[['cell_line']])
    }
    
    # remove empty vectors from list
    availData <- Filter(length, availData)

    return(availData)
}

#################
## Export data ##
#################

#' Export the subset data
#'
#' Export the list object generated by \code{hpaSubset()} into xlsx format. Due
#' to the size of some HPA datasets, as well as the limitation of the output
#' format, exporting the full datasets generated by \code{hpaDownload()} is not
#' recommended.
#'
#' @param data Input the list object generated by \code{hpaSubset()}
#' @param fileName A string indicate the desired output file name. Do not
#'   include file extension such as \code{'.xlsx'}.
#' @param fileType The format as which the data will be exported. Choose one of
#'   these options: \code{'xlsx'}, \code{'csv'} and \code{'tsv'}.
#'
#' @return 
#' \itemize{
#'   \item \code{'xlsx'}: return one .xlsx file named \code{'fileName.xlsx'}.
#'   One individual sheet for each dataset in the input list object.
#'   \item \code{'csv'}: return .csv files, one for each dataset in the input
#'   list object, named \code{'fileName_datasetName.csv'}
#'   \item \code{'tsv'}: return .tsv files, one for each dataset in the input
#'   list object, named \code{'fileName_datasetName.tsv'}
#' }
#'
#' @family downloadable datasets functions
#'
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   geneList <- c('TP53', 'EGFR')
#'   tissueList <- c('breast', 'cerebellum', 'skin 1')
#'   cancerList <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subsetData <- hpaSubset(data=downloadedData,
#'                            targetGene=geneList,
#'                            targetTissue=tissueList,
#'                            targetCancer=cancerList)
#'   hpaExport(data=subsetData,
#'             fileName='TP53_EGFR_in_tissue_cancer',
#'             fileType='xlsx')
#'
#' @importFrom openxlsx write.xlsx
#' @importFrom utils write.csv write.table
#' @export

hpaExport <- function(data, fileName, fileType='xlsx') {
    if(fileType == 'xlsx') {
        write.xlsx(data, file = paste0(fileName, ".xlsx"))
    }
    
    if(fileType == 'csv') {
        for (i in 1:length(data)) {
            write.csv(data[[i]], 
                      file = paste0(fileName, "_", names(data[i]), ".csv"))
        }
    }
    
    if(fileType == 'tsv') {
        for (i in 1:length(data)) {
            write.table(data[[i]], 
                        file = paste0(fileName, "_", names(data[i]), ".tsv"),
                        sep = "\t")
        }
    }
}
