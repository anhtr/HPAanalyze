#######################
## Download datasets ##
#######################

#' Download datasets
#' 
#' Download the latest version of HPA datasets and import them in R. It is
#' recommended to only download the datasets you need, as some of them may be
#' very big.
#' 
#' @param downloadList A vector or string indicate which datasets to download. Possible value:
#'   \itemize{
#'     \item \code{'Normal tissue'}
#'     \item \code{'Pathology'}
#'     \item \code{'Subcellular location'}
#'     \item \code{'RNA tissue'}
#'     \item \code{'RNA cell line'}
#'     \item \code{'RNA transcript tissue'}
#'     \item \code{'RNA transcript cell line'}
#'     \item \code{'all'}: download everything
#'     \item \code{'histology'}: same as \code{c('Normal tissue', 'Pathology', 'Subcellular location')}
#'     \item \code{'rna'}: same as \code{c('RNA tissue', 'RNA cell line')}
#'     \item \code{'isoform'}: same as \code{c('RNA transcript tissue', 'RNA transcript cell line')}
#'   }
#'   See \url{https://www.proteinatlas.org/about/download} for more information.
#'   
#' @return This function will return a list of tibbles corresponding to requested datasets.
#'  
#' @examples
#'   data("hpa_downloaded_histology_v18")
#'   summary(hpa_downloaded_histology_v18)
#'   
#'   \dontrun{
#'   
#'   ## download rna expression data
#'   rna_data <- hpaDownload('rna')
#'   
#'   ## download normal tissue and subcellular location data
#'   data <- hpaDownload(c('Normal tissue', 'Subcellular location'))
#'   }
#'      
#' @import dplyr
#' @import readr
#' @import tidyr
#' @importFrom utils download.file
#' @export
#' 

hpaDownload <- function(downloadList = 'histology') {
    
    #generate a list of item to download
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
    
    hpa_downloaded_data <- list() #initiate the list to be returned
    
    # Check if the term is requested or not (all by default)
    # then download the file
    # then unzip and make a tibble out of data
    # then add the tibble to the list
    if('Normal tissue' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/normal_tissue.tsv.zip',
                      destfile = temp)
        normal_tissue <- read_tsv(unz(temp, 'normal_tissue.tsv'))
        unlink(temp)
        colnames(normal_tissue) <- c('ensembl', 'gene', 'tissue', 
                                     'cell_type', 'level', 'reliability')
        hpa_downloaded_data$normal_tissue <- normal_tissue
    }
    
    if('Pathology' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/pathology.tsv.zip',
                      destfile = temp)
        pathology <- read_tsv(unz(temp, 'pathology.tsv'))
        unlink(temp)
        colnames(pathology) <- c('ensembl', 'gene', 'cancer', 'high', 'medium', 
                                 'low', 'not_detected', 'prognostic_favorable', 
                                 'unprognostic_favorable', 'prognostic_unfavorable', 
                                 'unprognostic_unfavorable')
        hpa_downloaded_data$pathology <- pathology
    }
    
    if('Subcellular location' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/subcellular_location.tsv.zip',
                      destfile = temp)
        subcellular_location <- read_tsv(unz(temp, 'subcellular_location.tsv'))
        unlink(temp)
        colnames(subcellular_location) <- c('ensembl', 'gene', 'reliability', 'enhanced', 
                                            'supported', 'approved', 'uncertain', 
                                            'single_cell_var_intensity', 
                                            'single_cell_var_spatial', 
                                            'cell_cycle_dependency', 'go_id')
        hpa_downloaded_data$subcellular_location <- subcellular_location
    }
    
    if('Subcellular location' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/subcellular_location.tsv.zip',
                      destfile = temp)
        subcellular_location <- read_tsv(unz(temp, 'subcellular_location.tsv'))
        unlink(temp)
        colnames(subcellular_location) <- c('ensembl', 'gene', 'reliability', 'enhanced', 
                                            'supported', 'approved', 'uncertain', 
                                            'single_cell_var_intensity', 
                                            'single_cell_var_spatial', 
                                            'cell_cycle_dependency', 'go_id')
        hpa_downloaded_data$subcellular_location <- subcellular_location
    }
    
    if('RNA tissue' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/rna_tissue.tsv.zip',
                      destfile = temp)
        rna_tissue <- read_tsv(unz(temp, 'rna_tissue.tsv'))
        unlink(temp)
        colnames(rna_tissue) <- c('ensembl', 'gene', 'tissue',
                                  'value', 'unit')
        hpa_downloaded_data$rna_tissue <- rna_tissue
    }
    
    if('RNA cell line' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/rna_celline.tsv.zip',
                      destfile = temp)
        rna_cell_line <- read_tsv(unz(temp, 'rna_celline.tsv'))
        unlink(temp)
        colnames(rna_cell_line) <- c('ensembl', 'gene', 'cell_line',
                                     'value', 'unit')
        hpa_downloaded_data$rna_cell_line <- rna_cell_line
    }    
    
    if('RNA transcript tissue' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/transcript_rna_tissue.tsv.zip',
                      destfile = temp)
        transcript_rna_tissue <- read_tsv(unz(temp, 'transcript_rna_tissue.tsv'))
        unlink(temp)
        transcript_rna_tissue <- gather(data = transcript_rna_tissue,
                                        key = 'tissue',
                                        value = 'value',
                                        -'ensgid', -'enstid')
        colnames(transcript_rna_tissue) <- c('ensembl', 'transcript', 
                                             'tissue', 'value')
        hpa_downloaded_data$transcript_rna_tissue <- transcript_rna_tissue
    }
    
    if('RNA transcript cell line' %in% downloadList) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/transcript_rna_celline.tsv.zip',
                      destfile = temp)
        transcript_rna_cell_line <- read_tsv(unz(temp, 'transcript_rna_celline.tsv'))
        unlink(temp)
        transcript_rna_cell_line <- gather(data = transcript_rna_cell_line,
                                           key = 'cell_line',
                                           value = 'value',
                                           -'ensgid', -'enstid')
        colnames(transcript_rna_cell_line) <- c('ensembl', 'transcript', 
                                                'cell_line', 'value')
        hpa_downloaded_data$transcript_rna_cell_line <- transcript_rna_cell_line
    }
    
    rm(temp)
    return(hpa_downloaded_data)
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
#' @return The output of this function is a list of vectors.
#' 
#' @examples
#'   data("hpa_downloaded_histology_v18")
#'   params <- hpaListParam(data = hpa_downloaded_histology_v18)
#'   params$normal_tissue
#'   \dontrun{
#'   downloaded_data <- hpaDownload(downloadList = 'histology')
#'   params <- hpaListParam(data = downloaded_data)
#'   params$normal_tissue
#'   }
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export

hpaListParam <- function(data) {
    available_data <- list()
    
    if('normal_tissue' %in% names(data)) {
        available_data$normal_tissue <- unique(data$normal_tissue[['tissue']])
        available_data$normal_cell <- unique(data$normal_tissue[['cell_type']])
    }
    
    if('pathology' %in% names(data)) {
        available_data$cancer <- unique(data$pathology[['cancer']])
    }
    
    if('subcellular_location' %in% names(data)) {
        available_data$subcellular_location <- unique(data$subcellular_location[['approved']])%>%
            strsplit(';') %>% unlist() %>% unique() %>% na.omit() %>% as.vector()
    }
    
    if('rna_tissue' %in% names(data)) {
        available_data$normal_tissue_rna <- unique(data$rna_tissue[['tissue']])
    }
    
    if('rna_cell_line' %in% names(data)) {
        available_data$cell_line_rna <- unique(data$rna_cell_line[['cell_line']])
    }
    
    return(available_data)
}


#################
## Subset data ##
#################

#' Subset downloaded data
#'
#' Subset data by gene name, tissue, cell type, cancer and/or cell line. The
#' input is the list object generated by \code{hpaDownload()} or as the output
#' of another \code{hpaSubset()}. Use \code{hpaListParam()} to see the list
#' of available parameters for a specific list object. Will not work on isoform
#' data.
#'
#' @param data Input the list object generated by \code{hpaDownload()} or
#'   \code{hpaSubset()}
#' @param targetGene Vector of strings of HGNC gene symbols. Will be used to
#'   subset every dataset in the list object.
#' @param targetTissue Vector of strings of normal tissues. Will be used to
#'   subset the \code{normal_tissue} and \code{rna_tissue} dataset.
#' @param targetCellType Vector of strings of normal cell types. Will be used
#'   to subset the \code{normal_tissue} dataset.
#' @param targetCancer Vector of strings of cancer types. Will be used to
#'   subset the \code{pathology} dataset.
#' @param targetCellLine Vector of strings of cell lines. Will be used to
#'   subset the \code{rna_cell_line} dataset.
#'
#' @return This function will return a list of tibbles.
#'
#' @examples
#'   data("hpa_downloaded_histology_v18")
#'   gene_list <- c('TP53', 'EGFR')
#'   tissue_list <- c('breast', 'cerebellum', 'skin 1')
#'   cancer_list <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subset_data <- hpaSubset(data = hpa_downloaded_histology_v18,
#'                             targetGene = gene_list,
#'                             targetTissue = tissue_list,
#'                             targetCancer = cancer_list)
#'   \dontrun{
#'   further_subset_data <- hpaSubset(data = subset_data,
#'                                     targetGene = 'TP53')
#'   }
#'
#' @import dplyr
#' @export

hpaSubset <- function(data,
                      targetGene = NULL,
                      targetTissue = NULL,
                      targetCellType = NULL,
                      targetCancer = NULL,
                      targetCellLine = NULL) {
    
    ## Just to pass the environment test in R CMD check
    gene <- tissue <- cell_type <- cancer <- cell_line <- NULL
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$normal_tissue <- filter(data$normal_tissue, gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$normal_tissue <- filter(data$normal_tissue, tissue %in% targetTissue)
        }
        
        if(!is.null(targetCellType)) {
            data$normal_tissue <- filter(data$normal_tissue, cell_type %in% targetCellType)
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$pathology <- filter(data$pathology, gene %in% targetGene)
        }
        
        if(!is.null(targetCancer)) {
            data$pathology <- filter(data$pathology, cancer %in% targetCancer)
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$subcellular_location <- filter(data$subcellular_location, gene %in% targetGene)
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$rna_tissue <- filter(data$rna_tissue, gene %in% targetGene)
        }
        
        if(!is.null(targetTissue)) {
            data$rna_tissue <- filter(data$rna_tissue, tissue %in% targetTissue)
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(targetGene)) {
            data$rna_cell_line <- filter(data$rna_cell_line, gene %in% targetGene)
        }
        
        if(!is.null(targetCellLine)) {
            data$rna_cell_line <- filter(data$rna_cell_line, cell_line %in% targetCellLine)
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
#'   data("hpa_downloaded_histology_v18")
#'   gene_list <- c('TP53', 'EGFR')
#'   tissue_list <- c('breast', 'cerebellum', 'skin 1')
#'   cancer_list <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subset_data <- hpaSubset(data = hpa_downloaded_histology_v18,
#'                            targetGene = gene_list,
#'                            targetTissue = tissue_list,
#'                            targetCancer = cancer_list)
#'   hpaExport(data = subset_data,
#'             fileName = 'TP53_EGFR_in_tissue_cancer.xlsx',
#'             fileType = 'xlsx')
#'
#' @import XLConnect
#' @export

hpaExport <- function(data, fileName, fileType = 'xlsx') {
    if(fileType == 'xlsx') {
        wb <- loadWorkbook(filename = fileName, create = TRUE)
        createSheet(wb, name = names(data))
        sheet_index = 0
        for (x in data) {
            sheet_index = sheet_index + 1
            writeWorksheet(wb, data = x, sheet = names(data)[sheet_index])
        }
        saveWorkbook(wb)
    }
}
