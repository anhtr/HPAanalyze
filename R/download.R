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
#'     \item \code{'RNA consensus tissue'}
#'     \item \code{'RNA HPA tissue'}
#'     \item \code{'RNA GTEx tissue'}
#'     \item \code{'RNA FANTOM tissue'}
#'     \item \code{'RNA single cell type'}
#'     \item \code{'RNA single cell type tissue cluster'}
#'     \item \code{'RNA GTEx brain region'}
#'     \item \code{'RNA FANTOM brain region'}
#'     \item \code{'RNA pig brain region'}
#'     \item \code{'RNA pig brain subregion sample'}
#'     \item \code{'RNA mouse brain region'}
#'     \item \code{'RNA mouse brain subregion sample'}
#'     \item \code{'RNA Allen mouse brain region'}
#'     \item \code{'RNA HPA immune cell'}
#'     \item \code{'RNA HPA immune cell sample'}
#'     \item \code{'RNA Monaco immune cell'}
#'     \item \code{'RNA Schmiedel immune cell'}
#'     \item \code{'RNA HPA blood cell'} (version 21.1)
#'     \item \code{'RNA HPA blood cell sample'} (version 21.1)
#'     \item \code{'RNA Monaco blood cell'} (version 21.1)
#'     \item \code{'RNA Schmiedel blood cell'} (version 21.1)
#'     \item \code{'RNA HPA cell line cancer'}
#'     \item \code{'RNA HPA cell line'}
#'     \item \code{'RNA TCGA cancer sample'}
#'     \item \code{'RNA transcript tissue'}
#'     \item \code{'RNA transcript GTEx retina'}
#'     \item \code{'RNA transcript immune cells'}
#'     \item \code{'RNA transcript cell line'} (version 21.1)
#'     \item \code{'RNA transcript pig brain'}
#'     \item \code{'RNA transcript mouse brain'}
#'     }
#'
#'   You can also use the following shortcuts:
#'   \itemize{
#'     \item \code{'all'}: download everything
#'     \item \code{'histology'}: same as \code{c('Normal tissue', 'Pathology',
#'     'Subcellular location')}
#'     \item \code{'rna tissue'}: same as \code{c('RNA consensus tissue', 'RNA
#'     HPA tissue', 'RNA GTEx tissue', 'RNA FANTOM tissue')}
#'     \item \code{'rna cell type'}: same as \code{c('RNA single cell
#'     type', 'RNA single cell type tissue cluster')}
#'     \item \code{'rna brain region'}: same as \code{c('RNA GTEx brain region',
#'     'RNA FANTOM brain region', 'RNA pig brain region', 'RNA pig brain
#'     subregion sample', 'RNA mouse brain region', 'RNA mouse brain subregion
#'     sample', 'RNA Allen mouse brain region')}
#'     \item \code{'rna immune cell'}: same as \code{c('RNA HPA immune
#'     cell', 'RNA HPA immune cell sample', 'RNA Monaco immune cell', 'RNA
#'     Schmiedel immune cell')}
#'     \item \code{'rna blood cell'}: same as \code{c('RNA HPA blood
#'     cell', 'RNA HPA blood cell sample', 'RNA Monaco blood cell', 'RNA
#'     Schmiedel blood cell')}
#'     \item \code{'isoform'}: same as \code{c('RNA isoform tissue', 'RNA
#'     isoform GTEx retina', 'RNA isoform immune cells', 'RNA
#'     isoform cell line', 'RNA isoform pig brain', 'RNA isoform mouse
#'     brain')}
#'   }
#'   See \url{https://www.proteinatlas.org/about/download} for more information.
#'
#' @param version A string indicate which version to be downloaded. Possible
#'   value:
#'   \itemize{
#'     \item \code{'latest'}: Download latest version. Certain legacy datasets
#'     will be downloaded with the highest version available. Require Internet
#'     connection. This is the default option.
#'     \item \code{'example'} or \code{'built-in'}: Load the built-in histology
#'     dataset from 'HPAanalyze' ('hpa_histology_data'). Do not require internet
#'     connection.
#'   }
#'
#' @family downloadable datasets functions
#'
#' @return This function will return a list of tibbles corresponding to
#'   requested datasets.
#'
#' @seealso \code{\link{hpaDownload}} \code{\link{hpa_histology_data}}
#'
#' @examples
#'   histologyData <- hpaDownload(downloadList='histology', version='example')
#'   # tissueTranscriptData <- hpaDownload('RNA transcript tissue')
#'
#' @import dplyr
#' @importFrom utils download.file data read.delim2 unzip
#' @importFrom stats reshape
#' @importFrom tibble as_tibble
#' @export
#' 

hpaDownload <- function(downloadList = 'histology',
                        version = 'latest') {
    ## set longer time out
    op <- options(timeout = 10000)
    on.exit(options(op))

    ## generate a list of item to download
    replace_shortcut <- function(x, shortcut, with) {
        x <- rep(x, 1 + (length(with) - 1)*(x == shortcut))
        x[x == shortcut] <- with
        return(x)
    }
    
    downloadList <- downloadList %>%
        replace_shortcut('all', hpa_download_list$table) %>%
        replace_shortcut('histology', 
                         c('Normal tissue',
                           'Pathology', 
                           'Subcellular location')) %>%
        replace_shortcut('rna tissue', 
                         c('RNA consensus tissue', 
                           'RNA HPA tissue', 
                           'RNA GTEx tissue', 
                           'RNA FANTOM tissue')) %>%
        replace_shortcut('rna cell type', 
                         c('RNA single cell type', 
                           'RNA single cell type tissue cluster')) %>%
        replace_shortcut('rna brain region', 
                         c('RNA GTEx brain region',
                           'RNA FANTOM brain region', 
                           'RNA pig brain region', 
                           'RNA pig brain subregion sample', 
                           'RNA mouse brain region', 
                           'RNA mouse brain subregion sample', 
                           'RNA Allen mouse brain region')) %>%
        replace_shortcut('rna immune cell', 
                         c('RNA HPA immunecell', 
                           'RNA HPA immune cell sample', 
                           'RNA Monaco immune cell', 
                           'RNA Schmiedel immune cell')) %>%
        replace_shortcut('rna blood cell', 
                         c('RNA HPA blood cell', 
                           'RNA HPA blood cell sample', 
                           'RNA Monaco blood cell', 
                           'RNA Schmiedel blood cell')) %>%
        replace_shortcut('isoform', 
                         c('RNA isoform tissue', 
                           'RNA isoform GTEx retina', 
                           'RNA isoform immune cells', 
                           'RNA isoform cell line', 
                           'RNA isoform pig brain', 
                           'RNA isoform mouse brain'))
    

    # filter the datasets to download
    downloadDatasets <-
        hpa_download_list %>%
        filter(version == {{version}}) %>%
        filter(table %in% downloadList)
    
    #initiate the list of processed data to be returned
    loadedData <- list()
    
    ## Download if version is 'built-in' or 'example'
    
    if (version %in% c('example', 'built-in')) {
        message(
            'Only the followings are example/built-in datasets: \n - Normal tissue \n - Pathology \n - Subcellular location \nOther datasets will not be loaded'
        )
        
        loadedData <- hpa_histology_data
        
    } else {
        
        ## download for any version that's not example/built-in
        for (i in seq_along(downloadDatasets$link)) {
            temp <- tempfile()
            download.file(url = downloadDatasets$link[[i]],
                          destfile = temp)
            loadedData[[i]] <- read.delim2(
                unz(temp, unzip(temp, list = TRUE)$Name[1]),
                stringsAsFactors = FALSE,
                check.names = FALSE,
                strip.white = TRUE,
                sep = "\t",
                na.strings = c("", " ")
            )
            unlink(temp)
            
            # if (downloadDatasets$table[[i]] %in% c(
            #     'RNA transcript tissue',
            #     'RNA transcript GTEx retina',
            #     'RNA transcript immune cells',
            #     'RNA transcript cell line',
            #     'RNA transcript pig brain',
            #     'RNA transcript mouse brain'
            # )) {
            #     loadedData[[i]] <-
            #         stats::reshape(
            #             loadedData[[i]],
            #             direction = "long",
            #             varying = list(3:ncol(loadedData[[i]])),
            #             v.names = "tpm",
            #             timevar = "sample",
            #             times = c(colnames(loadedData[[i]][, 3:ncol(loadedData[[i]])]))
            #         ) %>%
            #         subset(select = -id)
            #     
            # }
            
            ## assign tidy colnames
            # colnames(loadedData[[i]]) <- downloadDatasets$tidycols[[i]]
            
        }
        
        ## convert to tibbles
        loadedData <- lapply(loadedData, as_tibble)
        
        names(loadedData) <-
            downloadDatasets$link %>%
            gsub('.tsv.zip|https://.*.proteinatlas.org/download/',
                 '',
                 .)
    }
    
    return(loadedData)
}

#################
## Subset data ##
#################

#' Subset downloaded data
#'
#' \code{hpaSubset()} subsets data by gene name, tissue, cell type, cancer
#' and/or cell line. The input is the list object generated by
#' \code{hpaDownload()} or as the output of another \code{hpaSubset()}. Use
#' \code{hpaListParam()} to see the list of available parameters for a specific
#' list object. This is a convenient wrapper for `lapply/filter` and works on
#' any table which contain 'gene', 'tissue', 'cell_type', 'cancer', and
#' 'cell_line' columns.
#'
#' @param data Input the list object generated by \code{hpaDownload()} or
#'   \code{hpaSubset()}
#' @param targetGene Vector of strings of HGNC gene symbols. It will be used to
#'   subset every dataset in the list object. You can also mix HGNC gene symbols
#'   and ensemnbl ids (start with ENSG) and they will be converted to HGNC gene
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
#'   downloadedData <- hpaDownload(downloadList='histology', version='example')
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

hpaSubset <- function(data = NULL,
                      targetGene = NULL,
                      targetTissue = NULL,
                      targetCellType = NULL,
                      targetCancer = NULL,
                      targetCellLine = NULL) {
    # Check if data is provided or not
    data <- is_null_data(data = data)
    if (!is.null(targetGene))
        targetGene <- gene_ensembl_convert(targetGene, "gene")
    
    subsetting <- function(df) {
        if (!is.null(targetGene) & any(names(df) == "gene")) {
            df <- filter(df, gene %in% targetGene)
        }
        
        if (!is.null(targetTissue) & any(names(df) == "tissue")) {
            df <- filter(df, tissue %in% targetTissue)
        }
        
        if (!is.null(targetCellType) &
            any(names(df) == "cell_type")) {
            df <- filter(df, cell_type %in% targetCellType)
        }
        
        if (!is.null(targetCancer) & any(names(df) == "cancer")) {
            df <- filter(df, cancer %in% targetCancer)
        }
        
        if (!is.null(targetCellLine) &
            any(names(df) == "cell_line")) {
            df <- filter(df, cell_line %in% targetCellLine)
        }
        
        return(df)
    }
    
    data <- lapply(data, subsetting)
    
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
#' call of \code{hpaSubset()}. This is a convenient wrapper for `lapply/unique`
#' and works on any table which contain 'tissue', 'cell_type', 'cancer', and
#' 'cell_line' columns.
#'
#' @return The output of \code{hpaListParam()} is a list of vectors containing
#'   all subset parameter for the downloaded data.
#'
#' @examples
#'   downloadedData <- hpaDownload(downloadList='histology', version='example')
#'   params <- hpaListParam(data=downloadedData)
#'   params$normal_tissue
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export

hpaListParam <- function(data = NULL) {
    # Check if data is provided or not
    data <- is_null_data(data = data)
    
    # Write function for each df, list the param if exist
    listing <- function(df) {
        params <- lapply(c(
            "tissue" = "tissue",
            "cell_type" = "cell_type",
            "cancer" = "cancer",
            "cell_line" = "cell_line"
        ),
        function(x)
            unique(df[[x]]))
        
        # Remove empty param list
        params[lengths(params) != 0]
    }
    
    availData <- lapply(data, listing)
    
    return(availData[lengths(availData) != 0])
    
}

#################
## Export data ##
#################

#' Export the subset data
#'
#' Export the list object generated by \code{hpaSubset()} into xlsx format. Due
#' to the size of some HPA datasets, as well as the limitation of the output
#' format, exporting the full datasets generated by \code{hpaDownload()} is not
#' recommended. This is a convenient wrapper for `write.` functions.
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
#'   downloadedData <- hpaDownload(downloadList='histology', version='example')
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

hpaExport <- function(data, fileName, fileType = 'xlsx') {
    if (fileType == 'xlsx') {
        write.xlsx(data, file = paste0(fileName, ".xlsx"))
    }
    
    if (fileType == 'csv') {
        for (i in 1:length(data)) {
            write.csv(data[[i]],
                      file = paste0(fileName, "_", names(data[i]), ".csv"))
        }
    }
    
    if (fileType == 'tsv') {
        for (i in 1:length(data)) {
            write.table(data[[i]],
                        file = paste0(fileName, "_", names(data[i]), ".tsv"),
                        sep = "\t")
        }
    }
}
