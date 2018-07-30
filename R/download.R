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
#' @return This function will return a list of tibbles corresponding to
#'   requested datasets.
#'  
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   summary(downloadedData)
#'   
#' @import dplyr
#' @import hpar
#' @importFrom readr read_tsv
#' @importFrom magrittr %<>%
#' @importFrom utils download.file data
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @export
#' 

hpaDownload <- function(downloadList='histology', version='latest') {
    
    ## generate a list of item to download
    if(downloadList == 'all') {
        downloadList <- c('Normal tissue', 
                          'Pathology',
                          'Subcellular location',
                          'RNA tissue',
                          'RNA cell line',
                          'RNA transcript tissue',
                          'RNA transcript cell line')        
    } else if(downloadList == 'histology') {
        downloadList <- c('Normal tissue', 
                          'Pathology',
                          'Subcellular location')  
    } else if(downloadList == 'rna') {
        downloadList <- c('RNA tissue',
                          'RNA cell line')  
    } else if(downloadList == 'isoform') {
        downloadList <- c('RNA transcript tissue',
                          'RNA transcript cell line')
    }
    
    #initiate the list to be returned
    loadedData <- list()
    
    #create list of colnames
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
                                     'enhanced', 'supported', 'approved', 
                                     'uncertain', 'single_cell_var_intensity', 
                                     'single_cell_var_spatial', 
                                     'cell_cycle_dependency', 'go_id')
    rnaTissueColnames <- c('ensembl', 'gene', 'tissue', 'value', 'unit')
    rnaCellLineColnames <- c('ensembl', 'gene', 'cell_line',
                             'value', 'unit')
    transcriptRnaTissueColnames <- c('ensembl', 'transcript', 
                                     'tissue', 'value')
    transcriptRnaCellLineColnames <- c('ensembl', 'transcript', 
                                       'cell_line', 'value')
    
    if (version == 'hpar') {# load 'hpar' data
        if(!('package:hpar' %in% search())) {
            attachNamespace('hpar')
            # stop('Please load "hpar" package')
        }
        
        if('Normal tissue' %in% downloadList) {# load 'normal_tissue'
            data('hpaNormalTissue',
                 envir=environment())
            normal_tissue <- as_tibble(hpaNormalTissue)
            colnames(normal_tissue) <- normalTissueColnames
            loadedData$normal_tissue <- normal_tissue
        }
        
        if('Pathology' %in% downloadList) {# load `pathology`
            data('hpaCancer',
                 envir=environment())
            pathology <- as_tibble(hpaCancer) %>%
                select(-Total.patients) %>%
                spread(Level, Count.patients)
            colnames(pathology) <- pathologyColnamesHpar
            loadedData$pathology <- pathology
        }
        
        if('Subcellular location' %in% downloadList) {# load 'subcellular_location'
            data('hpaSubcellularLoc',
                 envir=environment())
            subcellular_location <- as_tibble(hpaSubcellularLoc)
            colnames(subcellular_location) <- subcellularLocationColnames
            subcellular_location$gene %<>% as.character
            subcellular_location$go_id %<>% as.character
            loadedData$subcellular_location <- subcellular_location
        }
        
        if('RNA tissue' %in% downloadList) {# load 'rna_tissue'
            data('rnaGeneTissue',
                 envir=environment())
            rna_tissue <- as_tibble(rnaGeneTissue)
            colnames(rna_tissue) <- rnaTissueColnames
            loadedData$rna_tissue <- rna_tissue
        }
        
        if('RNA cell line' %in% downloadList) { # load 'rna_cell_line'
            data('rnaGeneCellLine',
                 envir=environment())
            rna_cell_line <- as_tibble(rnaGeneCellLine)
            colnames(rna_cell_line) <- rnaCellLineColnames
            loadedData$rna_cell_line <- rna_cell_line
        }   
    
    } else if (version == 'example') {# load example data
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
        
    } else {# download data from the internet
    
        # generate a vector of urls for download
        downloadUrls <- version_to_download_urls(version)
        
        # Check if the term is requested or not (all by default)
        # then download the file
        # then unzip and make a tibble out of data
        # then add the tibble to the list
        if('Normal tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['normal_tissue'],
                          destfile=temp)
            normal_tissue <- read_tsv(unz(temp, 'normal_tissue.tsv'))
            unlink(temp)
            colnames(normal_tissue) <- normalTissueColnames
            loadedData$normal_tissue <- normal_tissue
        }
        
        if('Pathology' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['pathology'],
                          destfile=temp)
            pathology <- read_tsv(unz(temp, 'pathology.tsv'))
            unlink(temp)
            colnames(pathology) <- pathologyColnames
            loadedData$pathology <- pathology
        }
        
        if('Subcellular location' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['subcellular_location'],
                          destfile=temp)
            subcellular_location <- read_tsv(unz(temp, 'subcellular_location.tsv'))
            unlink(temp)
            colnames(subcellular_location) <- subcellularLocationColnames
            loadedData$subcellular_location <- subcellular_location
        }
        
        if('RNA tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['rna_tissue'],
                          destfile=temp)
            rna_tissue <- read_tsv(unz(temp, 'rna_tissue.tsv'))
            unlink(temp)
            colnames(rna_tissue) <- rnaTissueColnames
            loadedData$rna_tissue <- rna_tissue
        }
        
        if('RNA cell line' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['rna_cell_line'],
                          destfile=temp)
            rna_cell_line <- read_tsv(unz(temp, 'rna_celline.tsv'))
            unlink(temp)
            colnames(rna_cell_line) <- rnaCellLineColnames
            loadedData$rna_cell_line <- rna_cell_line
        }    
        
        if('RNA transcript tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['transcript_rna_tissue'],
                          destfile=temp)
            transcript_rna_tissue <- read_tsv(unz(temp, 'transcript_rna_tissue.tsv'))
            unlink(temp)
            transcript_rna_tissue %<>% gather(key='tissue',
                                              value='value',
                                              -'ensgid', -'enstid')
            colnames(transcript_rna_tissue) <- transcriptRnaTissueColnames
            loadedData$transcript_rna_tissue <- transcript_rna_tissue
        }
        
        if('RNA transcript cell line' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['transcript_rna_cell_line'],
                          destfile=temp)
            transcript_rna_cell_line <- read_tsv(unz(temp, 'transcript_rna_celline.tsv'))
            unlink(temp)
            transcript_rna_cell_line %<>% gather(key='cell_line',
                                                 value='value',
                                                 -'ensgid', -'enstid')
            colnames(transcript_rna_cell_line) <- transcriptRnaCellLineColnames
            loadedData$transcript_rna_cell_line <- transcript_rna_cell_line
        }
        
        rm(temp)
    }
    
    return(loadedData)
}


#########################
## List available data ##
#########################

#' List available data
#'
#' List available variables in downloaded data that can be used as parameters to
#' subset the data via \code{hpaSubset()}. This function work with the data
#' object generated by \code{hpaDownload()}.
#'
#' @param data Input the list object generated by \code{hpaDownload()}
#'
#' @return The output of this function is a list of vectors containing all
#'   subset parameter for the downloaded data.
#'
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   params <- hpaListParam(data=downloadedData)
#'   params$normal_tissue
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export

hpaListParam <- function(data) {
    availData <- list()
    
    if('normal_tissue' %in% names(data)) {
        availData$normal_tissue <- unique(data$normal_tissue[['tissue']])
        availData$normal_cell <- unique(data$normal_tissue[['cell_type']])
    }
    
    if('pathology' %in% names(data)) {
        availData$cancer <- unique(data$pathology[['cancer']])
    }
    
    if('subcellular_location' %in% names(data)) {
        availData$subcellular_location <- unique(data$subcellular_location[['approved']])%>%
            strsplit(';') %>% unlist() %>% 
            unique() %>% na.omit() %>% as.vector()
    }
    
    if('rna_tissue' %in% names(data)) {
        availData$normal_tissue_rna <- unique(data$rna_tissue[['tissue']])
    }
    
    if('rna_cell_line' %in% names(data)) {
        availData$cell_line_rna <- unique(data$rna_cell_line[['cell_line']])
    }
    
    return(availData)
}


#################
## Subset data ##
#################

#' Subset downloaded data
#'
#' Subset data by gene name, tissue, cell type, cancer and/or cell line. The
#' input is the list object generated by \code{hpaDownload()} or as the output
#' of another \code{hpaSubset()}. Use \code{hpaListParam()} to see the list of
#' available parameters for a specific list object. Will not work on isoform
#' data.
#'
#' @param data Input the list object generated by \code{hpaDownload()} or
#'   \code{hpaSubset()}
#' @param targetGene Vector of strings of HGNC gene symbols. Will be used to
#'   subset every dataset in the list object.
#' @param targetTissue Vector of strings of normal tissues. Will be used to
#'   subset the \code{normal_tissue} and \code{rna_tissue} dataset.
#' @param targetCellType Vector of strings of normal cell types. Will be used to
#'   subset the \code{normal_tissue} dataset.
#' @param targetCancer Vector of strings of cancer types. Will be used to subset
#'   the \code{pathology} dataset.
#' @param targetCellLine Vector of strings of cell lines. Will be used to subset
#'   the \code{rna_cell_line} dataset.
#'
#' @return This function will return a list of tibbles as the result of
#'   subsetting.
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
#' @import dplyr
#' @importFrom magrittr %<>%
#' @export

hpaSubset <- function(data,
                      targetGene=NULL,
                      targetTissue=NULL,
                      targetCellType=NULL,
                      targetCancer=NULL,
                      targetCellLine=NULL) {
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$normal_tissue %<>% filter(gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$normal_tissue %<>% filter(tissue %in% targetTissue)
        }
        
        if(!is.null(targetCellType)) {
            data$normal_tissue %<>% filter(cell_type %in% targetCellType)
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$pathology %<>% filter(gene %in% targetGene)
        }
        
        if(!is.null(targetCancer)) {
            data$pathology %<>% filter(cancer %in% targetCancer)
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$subcellular_location %<>% filter(gene %in% targetGene)
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$rna_tissue %<>% filter(gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$rna_tissue %<>% filter(tissue %in% targetTissue)
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$rna_cell_line %<>% (gene %in% targetGene)
        }
        
        if(!is.null(targetCellLine)) {
            data$rna_cell_line %<>% filter(cell_line %in% targetCellLine)
        }       
    }
    
    return(data)
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
#' @param fileName A string indicate the desired output file name
#' @param fileType For compability of future development. Currently the only
#'   option is \code{'xlsx'}
#'
#' @return This function will create an xlsx file which one individual
#'   spreadsheet for each datasets in the input list object.
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
#'             fileName='TP53_EGFR_in_tissue_cancer.xlsx',
#'             fileType='xlsx')
#'
#' @importFrom XLConnect loadWorkbook createSheet writeWorksheet saveWorkbook
#' @export

hpaExport <- function(data, fileName, fileType='xlsx') {
    if(fileType == 'xlsx') {
        wb <- loadWorkbook(filename=fileName, create=TRUE)
        createSheet(wb, name=names(data))
        sheetIndex=0
        for (x in data) {
            sheetIndex=sheetIndex + 1
            writeWorksheet(wb, data=x, sheet=names(data)[sheetIndex])
        }
        saveWorkbook(wb)
    }
}
