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
#'     \item \code{'latest'}: Download latest version. Require Internet
#'     connection.
#'     \item \code{'example'}: Load the example dataset from 'HPA
#'     analyze' ('hpa_histology_data'). Does not contain rna or
#'     isoform data.
#'   }
#' 
#' @family downloadable datasets functions
#' 
#' @return This function will return a list of tibbles corresponding to
#'   requested datasets.
#'   
#' @seealso
#' \code{\link{hpaDownload}}
#' \code{\link{hpa_histology_data}}
#'  
#' @examples
#'   downloadedData <- hpaDownload(downloadList='all', version='example')
#'   summary(downloadedData)
#'   
#' @import dplyr
#  @import hpar
#' @importFrom utils download.file data read.delim2
#' @importFrom stats reshape
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
    subcellularLocationColnames <- c('ensembl', 'gene', 'reliability',
                                     "main_location", "additional_location", 
                                     "extracellular_location",
                                     'enhanced', 'supported', 'approved', 
                                     'uncertain', 'single_cell_var_intensity', 
                                     'single_cell_var_spatial', 
                                     'cell_cycle_dependency', 'go_id')
    rnaTissueColnames <- c('ensembl', 'gene', 'tissue', 'nx')
    rnaCellLineColnames <- c('ensembl', 'gene', 'cell_line',
                             'tpm', 'ptpm', "nx")
    transcriptRnaTissueColnames <- c('ensembl', 'transcript', 
                                     'tissue', 'tpm')
    transcriptRnaCellLineColnames <- c('ensembl', 'transcript', 
                                       'cell_line', 'tpm')
    
    if (version == 'example') {# load example data
        data('hpa_histology_data', 
             package='HPAanalyze',
             envir=environment())
        if('Normal tissue' %in% downloadList) {# load 'normal_tissue'
            loadedData$normal_tissue <- HPAanalyze::hpa_histology_data$normal_tissue
        }
        
        if('Pathology' %in% downloadList) {# load `pathology`
            loadedData$pathology <- HPAanalyze::hpa_histology_data$pathology
        }
        
        if('Subcellular location' %in% downloadList) {# load 'subcellular_location'
            loadedData$subcellular_location <- HPAanalyze::hpa_histology_data$subcellular_location
        }
        
    } else {# download data from the internet

        # Check if the term is requested or not (all by default)
        # then download the file
        # then unzip and make a tibble out of data
        # then add the tibble to the list
        if('Normal tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url = "https://www.proteinatlas.org/download/normal_tissue.tsv.zip",
                          destfile = temp)
            # normal_tissue <- read_tsv(unz(temp, 'normal_tissue.tsv'))
            normal_tissue <-
                read.delim2(
                    unz(temp, 'normal_tissue.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            colnames(normal_tissue) <- normalTissueColnames
            loadedData$normal_tissue <- normal_tissue
        }
        
        if('Pathology' %in% downloadList) {
            temp <- tempfile()
            download.file(url = "https://www.proteinatlas.org/download/pathology.tsv.zip",
                          destfile = temp)
            # pathology <- read_tsv(unz(temp, 'pathology.tsv'))
            pathology <-
                read.delim2(
                    unz(temp, 'pathology.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            colnames(pathology) <- pathologyColnames
            
            # use correct column type
            pathology <- pathology %>%
                mutate_at(c('prognostic_favorable', 
                            'unprognostic_favorable', 
                            'prognostic_unfavorable', 
                            'unprognostic_unfavorable'), as.numeric)
            
            loadedData$pathology <- pathology
        }
        
        if('Subcellular location' %in% downloadList) {
            temp <- tempfile()
            download.file(url="https://www.proteinatlas.org/download/subcellular_location.tsv.zip",
                          destfile=temp)
            # subcellular_location <- read_tsv(unz(temp, 'subcellular_location.tsv'))
            subcellular_location <-
                read.delim2(
                    unz(temp, 'subcellular_location.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            
            if (ncol(subcellular_location) == 14) {
                colnames(subcellular_location) <- subcellularLocationColnames
            } else if (ncol(subcellular_location) == 11) {
                colnames(subcellular_location) <- subcellularLocationColnames_legacy
            }
            
            loadedData$subcellular_location <- subcellular_location
        }
        
        if('RNA tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url="https://www.proteinatlas.org/download/rna_tissue_consensus.tsv.zip",
                          destfile=temp)
            # rna_tissue <- read_tsv(unz(temp, 'rna_tissue.tsv'))
            rna_tissue <-
                read.delim2(
                    unz(temp, 'rna_consensus.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            colnames(rna_tissue) <- rnaTissueColnames
            loadedData$rna_tissue <- rna_tissue
        }
        
        if('RNA cell line' %in% downloadList) {
            temp <- tempfile()
            download.file(url="https://www.proteinatlas.org/download/rna_celline.tsv.zip",
                          destfile=temp)
            # rna_cell_line <- read_tsv(unz(temp, 'rna_celline.tsv'))
            rna_cell_line <-
                read.delim2(
                    unz(temp, 'rna_celline.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            colnames(rna_cell_line) <- rnaCellLineColnames
            loadedData$rna_cell_line <- rna_cell_line
        }    
        
        if('RNA transcript tissue' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['transcript_rna_tissue'],
                          destfile=temp)
            # transcript_rna_tissue <- read_tsv(unz(temp, 'transcript_rna_tissue.tsv'))
            transcript_rna_tissue <-
                read.delim2(
                    unz(temp, 'transcript_rna_tissue.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            
            ## Old version used tidyr::gather
            # transcript_rna_tissue <- tidyr::gather(transcript_rna_tissue,
            #                                 key='tissue',
            #                                 value='value',
            #                                 -'ensgid', -'enstid')
            
            ## New version use stats::reshape
            transcript_rna_tissue <-
                stats::reshape(
                    transcript_rna_tissue,
                    direction = "long",
                    varying = list(3:ncol(transcript_rna_tissue)),
                    v.names = "value",
                    timevar = "tissue",
                    times = c(colnames(transcript_rna_tissue[, 3:ncol(transcript_rna_tissue)]))
                ) %>%
                subset(select = -id)
            
            colnames(transcript_rna_tissue) <- transcriptRnaTissueColnames
            loadedData$transcript_rna_tissue <- transcript_rna_tissue
        }
        
        if('RNA transcript cell line' %in% downloadList) {
            temp <- tempfile()
            download.file(url=downloadUrls['transcript_rna_cell_line'],
                          destfile=temp)
            # transcript_rna_cell_line <- read_tsv(unz(temp, 'transcript_rna_celline.tsv'))
            transcript_rna_cell_line <-
                read.delim2(
                    unz(temp, 'transcript_rna_celline.tsv'),
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    strip.white = TRUE,
                    sep="\t",
                    na.strings = c("", " ")
                ) %>% as_tibble()
            unlink(temp)
            
            ## Old version used tidyr::gather
            # transcript_rna_cell_line <- tidyr::gather(transcript_rna_cell_line,
            #                                    key='cell_line',
            #                                    value='value',
            #                                    -'ensgid', -'enstid')
            
            ## New version use stats::reshape
            transcript_rna_cell_line <-
                stats::reshape(
                    transcript_rna_cell_line,
                    direction = "long",
                    varying = list(3:ncol(transcript_rna_cell_line)),
                    v.names = "value",
                    timevar = "cell_line",
                    times = c(colnames(transcript_rna_cell_line[, 3:ncol(transcript_rna_cell_line)]))
                ) %>%
                subset(select = -id)
            
            colnames(transcript_rna_cell_line) <- transcriptRnaCellLineColnames
            loadedData$transcript_rna_cell_line <- transcript_rna_cell_line
        }
        
        rm(temp)
    }
    
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
#' @return \code{hpaSubset} will return a list of tibbles as the result of
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
#' @import dplyr
#' @export

hpaSubset <- function(data=NULL,
                      targetGene=NULL,
                      targetTissue=NULL,
                      targetCellType=NULL,
                      targetCancer=NULL,
                      targetCellLine=NULL) {
    
    # Check if data is provided or not
    if (is.null(data)) {
        message(paste0('No data provided. Use version ', 
                       hpa_histology_data$metadata$HPAversion,
                       "."))
        data = HPAanalyze::hpa_histology_data
    }
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$normal_tissue <- 
                filter(data$normal_tissue, gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$normal_tissue <-
                filter(data$normal_tissue, tissue %in% targetTissue)
        }
        
        if(!is.null(targetCellType)) {
            data$normal_tissue <-
                filter(data$normal_tissue, cell_type %in% targetCellType)
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$pathology <-
                filter(data$pathology, gene %in% targetGene)
        }
        
        if(!is.null(targetCancer)) {
            data$pathology <-
                filter(data$pathology, cancer %in% targetCancer)
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$subcellular_location <-
                filter(data$subcellular_location, gene %in% targetGene)
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$rna_tissue <-
                filter(data$rna_tissue, gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$rna_tissue <-
                filter(data$rna_tissue, tissue %in% targetTissue)
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(targetGene)) {
            targetGene <- gene_ensembl_convert(targetGene, "gene")
            data$rna_cell_line <-
                filter(data$rna_cell_line, gene %in% targetGene)
        }
        
        if(!is.null(targetCellLine)) {
            data$rna_cell_line <-
                filter(data$rna_cell_line, cell_line %in% targetCellLine)
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
#' @import dplyr
#' @importFrom stats na.omit
#' @export

hpaListParam <- function(data=NULL) {
    
    # Check if data is provided or not
    if (is.null(data)) {
        message(paste0('No data provided. Use version ', 
                       hpa_histology_data$metadata$HPAversion,
                       "."))
        data = HPAanalyze::hpa_histology_data
    }
    
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
#'             fileName='TP53_EGFR_in_tissue_cancer.xlsx',
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
