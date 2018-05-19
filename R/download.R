#######################
## Download datasets ##
#######################

#' Download datasets
#' 
#' Download the latest version of HPA datasets and import them in R. It is
#' recommended to only download the datasets you need, as some of them may be
#' very big.
#' 
#' @param download_list A vector or string indicate which datasets to download. Possible value:
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
#'   \dontrun{
#'   ## download rna expression data
#'   rna_data <- hpa_download('rna')
#'   
#'   ## download normal tissue and subcellular location data
#'   data <- hpa_download(c('Normal tissue', 'Subcellular location'))
#'   }
#'      
#' @import dplyr
#' @import readr
#' @import tidyr
#' @importFrom utils download.file
#' @export
#' 

hpa_download <- function(download_list = 'histology') {
    
    #generate a list of item to download
    if(download_list == 'all') {
        download_list <- c('Normal tissue', 
                           'Pathology',
                           'Subcellular location',
                           'RNA tissue',
                           'RNA cell line',
                           'RNA transcript tissue',
                           'RNA transcript cell line')        
    } else if(download_list == 'histology') {
        download_list <- c('Normal tissue', 
                           'Pathology',
                           'Subcellular location')  
    } else if(download_list == 'rna') {
        download_list <- c('RNA tissue',
                           'RNA cell line')  
    } else if(download_list == 'isoform') {
        download_list <- c('RNA transcript tissue',
                           'RNA transcript cell line')
    }
    
    hpa_downloaded_data <- list() #initiate the list to be returned
    
    # Check if the term is requested or not (all by default)
    # then download the file
    # then unzip and make a tibble out of data
    # then add the tibble to the list
    if('Normal tissue' %in% download_list) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/normal_tissue.tsv.zip',
                      destfile = temp)
        normal_tissue <- read_tsv(unz(temp, 'normal_tissue.tsv'))
        unlink(temp)
        colnames(normal_tissue) <- c('ensembl', 'gene', 'tissue', 
                                     'cell_type', 'level', 'reliability')
        hpa_downloaded_data$normal_tissue <- normal_tissue
    }
    
    if('Pathology' %in% download_list) {
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
    
    if('Subcellular location' %in% download_list) {
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
    
    if('Subcellular location' %in% download_list) {
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
    
    if('RNA tissue' %in% download_list) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/rna_tissue.tsv.zip',
                      destfile = temp)
        rna_tissue <- read_tsv(unz(temp, 'rna_tissue.tsv'))
        unlink(temp)
        colnames(rna_tissue) <- c('ensembl', 'gene', 'tissue',
                                  'value', 'unit')
        hpa_downloaded_data$rna_tissue <- rna_tissue
    }
    
    if('RNA cell line' %in% download_list) {
        temp <- tempfile()
        download.file(url = 'https://www.proteinatlas.org/download/rna_celline.tsv.zip',
                      destfile = temp)
        rna_cell_line <- read_tsv(unz(temp, 'rna_celline.tsv'))
        unlink(temp)
        colnames(rna_cell_line) <- c('ensembl', 'gene', 'cell_line',
                                     'value', 'unit')
        hpa_downloaded_data$rna_cell_line <- rna_cell_line
    }    
    
    if('RNA transcript tissue' %in% download_list) {
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
    
    if('RNA transcript cell line' %in% download_list) {
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
#' subset the data via \code{hpa_subset()}. This function work with the data
#' object generated by \code{hpa_download()}.
#'
#' @param data Input the list object generated by \code{hpa_download()}
#' 
#' @return The output of this function is a list of vectors.
#' 
#' @examples 
#'   \dontrun{
#'   downloaded_data <- hpa_download(download_list = 'histology')
#'   params <- hpa_list_param(data = downloaded_data)
#'   params$normal_tissue
#'   }
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export

hpa_list_param <- function(data) {
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
#' input is the list object generated by \code{hpa_download()} or as the output
#' of another \code{hpa_subset()}. Use \code{hpa_list_param()} to see the list
#' of available parameters for a specific list object. Will not work on isoform
#' data.
#'
#' @param data Input the list object generated by \code{hpa_download()} or
#'   \code{hpa_subset()}
#' @param target_gene Vector of strings of HGNC gene symbols. Will be used to
#'   subset every dataset in the list object.
#' @param target_tissue Vector of strings of normal tissues. Will be used to
#'   subset the \code{normal_tissue} and \code{rna_tissue} dataset.
#' @param target_cell_type Vector of strings of normal cell types. Will be used
#'   to subset the \code{normal_tissue} dataset.
#' @param target_cancer Vector of strings of cancer types. Will be used to
#'   subset the \code{pathology} dataset.
#' @param target_cell_line Vector of strings of cell lines. Will be used to
#'   subset the \code{rna_cell_line} dataset.
#'
#' @return This function will return a list of tibbles.
#'
#' @examples
#'   \dontrun{
#'   downloaded_data <- hpa_download(download_list = 'histology')
#'   gene_list <- c('TP53', 'EGFR')
#'   tissue_list <- c('breast', 'cerebellum', 'skin 1')
#'   cancer_list <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subset_data <- hpa_subset(data = downloaded_data,
#'                             target_gene = gene_list,
#'                             target_tissue = tissue_list,
#'                             target_cancer = cancer_list)
#'
#'   further_subset_data <- hpa_subset(data = subset_data,
#'                                     target_gene = 'TP53')
#'   }
#'
#' @import dplyr
#' @export

hpa_subset <- function(data,
                       target_gene = NULL,
                       target_tissue = NULL,
                       target_cell_type = NULL,
                       target_cancer = NULL,
                       target_cell_line = NULL) {
    
    ## Just to pass the environment test in R CMD check
    gene <- tissue <- cell_type <- cancer <- cell_line <- NULL
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$normal_tissue <- filter(data$normal_tissue, gene %in% target_gene)
        }
        
        if(!is.null(target_tissue)) {
            data$normal_tissue <- filter(data$normal_tissue, tissue %in% target_tissue)
        }
        
        if(!is.null(target_cell_type)) {
            data$normal_tissue <- filter(data$normal_tissue, cell_type %in% target_cell_type)
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$pathology <- filter(data$pathology, gene %in% target_gene)
        }
        
        if(!is.null(target_cancer)) {
            data$pathology <- filter(data$pathology, cancer %in% target_cancer)
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$subcellular_location <- filter(data$subcellular_location, gene %in% target_gene)
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$rna_tissue <- filter(data$rna_tissue, gene %in% target_gene)
        }
        
        if(!is.null(target_tissue)) {
            data$rna_tissue <- filter(data$rna_tissue, tissue %in% target_tissue)
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$rna_cell_line <- filter(data$rna_cell_line, gene %in% target_gene)
        }
        
        if(!is.null(target_cell_line)) {
            data$rna_cell_line <- filter(data$rna_cell_line, cell_line %in% target_cell_line)
        }       
    }
    
    return(data)
}


#################
## Export data ##
#################

#' Export the subset data
#'
#' Export the list object generated by \code{hpa_subset()} into xlsx format. Due
#' to the size of some HPA datasets, as well as the limitation of the output
#' format, exporting the full datasets generated by \code{hpa_download()} is not
#' recommended.
#'
#' @param data Input the list object generated by \code{hpa_subset()}
#' @param file_name A string indicate the desired output file name
#' @param file_type For compability of future development. Currently the only
#'   option is \code{'xlsx'}
#'
#' @return This function will create an xlsx file which one individual
#'   spreadsheet for each datasets in the input list object.
#'   
#' @examples 
#'   \dontrun{
#'   downloaded_data <- hpa_download(download_list = 'histology')
#'   gene_list <- c('TP53', 'EGFR')
#'   tissue_list <- c('breast', 'cerebellum', 'skin 1')
#'   cancer_list <- c('breast cancer', 'glioma', 'melanoma')
#'
#'   subset_data <- hpa_subset(data = downloaded_data,
#'                             target_gene = gene_list,
#'                             target_tissue = tissue_list,
#'                             target_cancer = cancer_list)
#'   hpa_export(data = subset_data,
#'              file_name = 'TP53_EGFR_in_tissue_cancer.xlsx',
#'              file_type = 'xlsx')
#'   
#'   }
#'
#' @import XLConnect
#' @export

hpa_export <- function(data, file_name, file_type = 'xlsx') {
    if(file_type == 'xlsx') {
        wb <- loadWorkbook(filename = file_name, create = TRUE)
        createSheet(wb, name = names(data))
        sheet_index = 0
        for (x in data) {
            sheet_index = sheet_index + 1
            writeWorksheet(wb, data = x, sheet = names(data)[sheet_index])
        }
        saveWorkbook(wb)
    }
}
