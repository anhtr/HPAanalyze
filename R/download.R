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
#'     \item \code{'isoform'}: same as \code{c('RNA transcript tissue', 'RNA 
#'     transcript GTEx retina', 'RNA transcript immune cells', 'RNA
#'     transcript cell line', 'RNA transcript pig brain', 'RNA transcript mouse
#'     brain')}
#'   }
#'   See \url{https://www.proteinatlas.org/about/download} for more information.
#'
#' @param version A string indicate which version to be downloaded. Possible
#'   value:
#'   \itemize{
#'     \item \code{'latest'}: Download latest version. Require Internet
#'     connection.
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
#'   downloadedData <- hpaDownload(downloadList='histology', version='example')
#'   summary(downloadedData)
#'
#'
#' @import dplyr
#' @importFrom utils download.file data read.delim2 unzip
#' @importFrom stats reshape
#' @importFrom tibble as_tibble
#' @export
#' 

hpaDownload <- function(downloadList = 'histology',
                        version = 'latest') {
    ## Create a data frame with information to filter and download datasets
    allDatasets <- tibble(
        datasetnames = c(
            'Normal tissue',
            'Pathology',
            'Subcellular location',
            'RNA consensus tissue',
            'RNA HPA tissue',
            'RNA GTEx tissue',
            'RNA FANTOM tissue',
            'RNA single cell type',
            'RNA single cell type tissue cluster',
            'RNA GTEx brain region',
            'RNA FANTOM brain region',
            'RNA pig brain region',
            'RNA pig brain subregion sample',
            'RNA mouse brain region',
            'RNA mouse brain subregion sample',
            'RNA Allen mouse brain region',
            'RNA HPA immune cell',
            'RNA HPA immune cell sample',
            'RNA Monaco immune cell',
            'RNA Schmiedel immune cell',
            'RNA HPA blood cell',
            'RNA HPA blood cell sample',
            'RNA Monaco blood cell',
            'RNA Schmiedel blood cell',
            'RNA HPA cell line cancer',
            'RNA HPA cell line',
            'RNA TCGA cancer sample',
            'RNA transcript tissue',
            'RNA transcript GTEx retina',
            'RNA transcript immune cells',
            'RNA transcript cell line', 
            'RNA transcript pig brain',
            'RNA transcript mouse brain'
        ),
        
        tidycols = list(
            normal_tissue =
                c(
                    'ensembl',
                    'gene',
                    'tissue',
                    'cell_type',
                    'level',
                    'reliability'
                ),
            
            pathology =
                c(
                    'ensembl',
                    'gene',
                    'cancer',
                    'high',
                    'medium',
                    'low',
                    'not_detected',
                    'prognostic_favorable',
                    'unprognostic_favorable',
                    'prognostic_unfavorable',
                    'unprognostic_unfavorable'
                ),
            
            subcellular_location =
                c(
                    'ensembl',
                    'gene',
                    'reliability',
                    'main_location',
                    'additional_location',
                    'extracellular_location',
                    'enhanced',
                    'supported',
                    'approved',
                    'uncertain',
                    'single_cell_var_intensity',
                    'single_cell_var_spatial',
                    'cell_cycle_dependency',
                    'go_id'
                ),
            
            rna_tissue_consensus =
                c('ensembl', 'gene', 'tissue', 'nx'),
            
            rna_tissue_hpa =
                c('ensembl', 'gene', 'tissue', 'tpm', 'ptpm', "nx"),
            
            rna_tissue_gtex =
                c('ensembl', 'gene', 'tissue', 'tpm', 'ptpm', "nx"),
            
            rna_tissue_fantom =
                c(
                    'ensembl',
                    'gene',
                    'tissue',
                    'tags_per_million',
                    'scaled_tags_per_million',
                    "nx"
                ),
            
            rna_single_cell_type =
                c('ensembl', 'gene', 'cell_type', 'nx'),
            
            rna_single_cell_type_tissue =
                c(
                    'ensembl',
                    'gene',
                    'tissue',
                    'cluster',
                    'cell_type',
                    'read_count',
                    'ptpm'
                ),
            
            rna_brain_gtex =
                c('ensembl', 'gene', 'brain_region', 'tpm', 'ptpm', 'nx'),
            
            rna_brain_fantom =
                c(
                    'ensembl',
                    'gene',
                    'brain_region',
                    'tags_per_million',
                    'scaled_tags_per_million',
                    "nx"
                ),
            
            rna_pig_brain_hpa =
                c('ensembl', 'gene', 'brain_region', 'tpm', 'ptpm', 'nx'),
            
            rna_pig_brain_sample_hpa =
                c(
                    'ensembl',
                    'main_region',
                    'subregion',
                    'animal',
                    'tpm',
                    'ptpm'
                ),
            
            rna_mouse_brain_hpa =
                c('ensembl', 'gene', 'brain_region', 'tpm', 'ptpm', 'nx'),
            
            rna_mouse_brain_sample_hpa =
                c(
                    'ensembl',
                    'main_region',
                    'subregion',
                    'animal',
                    'tpm',
                    'ptpm'
                ),
            
            rna_mouse_brain_allen =
                c('ensembl', 'gene', 'brain_region', 'expression_energy'),
            
            rna_immune_cell =
                c('ensembl', 'gene', 'immune_cell', 'tpm', 'ptpm', 'ntpm'),
            
            rna_immune_cell_sample =
                c('sample_id', 'donor', 'immune_cell', 'ensembl', 'gene', 'tpm', 'ptpm', 'ntpm'),
            
            rna_immune_cell_monaco =
                c('ensembl', 'gene', 'immune_cell', 'tpm', 'ptpm'),
            
            rna_immune_cell_schmiedel =
                c('ensembl', 'gene', 'immune_cell', 'tpm'),
            
            rna_blood_cell =
                c('ensembl', 'gene', 'blood_cell', 'tpm', 'ptpm', 'nx'),
            
            rna_blood_cell_sample =
                c('ensembl', 'blood_cell_type', 'donor', 'ptpm', 'nx'),
            
            rna_blood_cell_monaco =
                c('ensembl', 'gene', 'blood_cell', 'tpm', 'ptpm'),
            
            rna_blood_cell_schmiedel =
                c('ensembl', 'gene', 'blood_cell', 'tpm'),
            
            rna_celline_cancer =
                c('ensembl', 'gene', 'cancer', 'tpm', 'ptpm', "ntpm"),
            
            rna_celline =
                c('ensembl', 'gene', 'cell_line', 'tpm', 'ptpm', "ntpm"),
            
            rna_cancer_sample =
                c('ensembl', 'sample', 'cancer', 'fpkm'),
            
            transcript_rna_tissue =
                c('ensgid', 'enstid', 'sample', 'tpm'),
            
            transcript_rna_gtexretina =
                c('ensgid', 'enstid', 'sample', 'tpm'),
            
            transcript_rna_immunecells =
                c('ensgid', 'enstid', 'sample', 'tpm'),
            
            transcript_rna_celline =
                c('ensgid', 'enstid', 'sample', 'tpm'),
            
            transcript_rna_pigbrain =
                c('ensgid', 'enstid', 'sample', 'tpm'),
            
            transcript_rna_mousebrain =
                c('ensgid', 'enstid', 'sample', 'tpm')
            
        ),
        
        urls = c(
            normal_tissue =
                'https://www.proteinatlas.org/download/normal_tissue.tsv.zip',
            pathology =
                'https://www.proteinatlas.org/download/pathology.tsv.zip',
            subcellular_location =
                'https://www.proteinatlas.org/download/subcellular_location.tsv.zip',
            rna_tissue_consensus =
                'https://www.proteinatlas.org/download/subcellular_location.tsv.zip',
            rna_tissue_hpa =
                'https://www.proteinatlas.org/download/rna_tissue_hpa.tsv.zip',
            rna_tissue_gtex =
                'https://www.proteinatlas.org/download/rna_tissue_gtex.tsv.zip',
            rna_tissue_fantom =
                'https://www.proteinatlas.org/download/rna_tissue_fantom.tsv.zip',
            rna_single_cell_type =
                'https://www.proteinatlas.org/download/rna_single_cell_type.tsv.zip',
            rna_single_cell_type_tissue =
                'https://www.proteinatlas.org/download/rna_single_cell_type_tissue.tsv.zip',
            rna_brain_gtex =
                'https://www.proteinatlas.org/download/rna_brain_gtex.tsv.zip',
            rna_brain_fantom =
                'https://www.proteinatlas.org/download/rna_brain_fantom.tsv.zip',
            rna_pig_brain_hpa =
                'https://www.proteinatlas.org/download/rna_pig_brain_hpa.tsv.zip',
            rna_pig_brain_sample_hpa =
                'https://www.proteinatlas.org/download/rna_pig_brain_sample_hpa.tsv.zip',
            rna_mouse_brain_hpa =
                'https://www.proteinatlas.org/download/rna_mouse_brain_hpa.tsv.zip',
            rna_mouse_brain_sample_hpa =
                'https://www.proteinatlas.org/download/rna_mouse_brain_sample_hpa.tsv.zip',
            rna_mouse_brain_allen =
                'https://www.proteinatlas.org/download/rna_mouse_brain_allen.tsv.zip',
            rna_immune_cell =
                'https://www.proteinatlas.org/download/rna_immune_cell.tsv.zip',
            rna_immune_cell_sample =
                'https://www.proteinatlas.org/download/rna_immune_cell_sample.tsv.zip',
            rna_immune_cell_monaco =
                'https://www.proteinatlas.org/download/rna_immune_cell_monaco.tsv.zip',
            rna_immune_cell_schmiedel =
                'https://www.proteinatlas.org/download/rna_immune_cell_schmiedel.tsv.zip',
            rna_blood_cell =
                'https://v21.proteinatlas.org/download/rna_blood_cell.tsv.zip',
            rna_blood_cell_sample =
                'https://v21.proteinatlas.org/download/rna_blood_cell_sample.tsv.zip',
            rna_blood_cell_monaco =
                'https://v21.proteinatlas.org/download/rna_blood_cell_monaco.tsv.zip',
            rna_blood_cell_schmiedel =
                'https://v21.proteinatlas.org/download/rna_blood_cell_schmiedel.tsv.zip',
            rna_celline_cancer =
                'https://www.proteinatlas.org/download/rna_celline_cancer.tsv.zip',
            rna_celline =
                'https://www.proteinatlas.org/download/rna_celline.tsv.zip',
            rna_cancer_sample =
                'https://www.proteinatlas.org/download/rna_cancer_sample.tsv.zip',
            transcript_rna_tissue =
                'https://www.proteinatlas.org/download/transcript_rna_tissue.tsv.zip',
            transcript_rna_gtexretina =
                'https://www.proteinatlas.org/download/transcript_rna_gtexretina.tsv.zip',
            transcript_rna_immunecells =
                'https://www.proteinatlas.org/download/transcript_rna_immunecells.tsv.zip',
            transcript_rna_celline =
                'https://v21.proteinatlas.org/download/transcript_rna_celline.tsv.zip',
            transcript_rna_pigbrain =
                'https://www.proteinatlas.org/download/transcript_rna_pigbrain.tsv.zip',
            transcript_rna_mousebrain =
                'https://www.proteinatlas.org/download/transcript_rna_mousebrain.tsv.zip'
        )
    )
    
    ## generate a list of item to download
    replace_shortcut <- function(x, shortcut, with) {
        x <- rep(x, 1 + (length(with) - 1)*(x == shortcut))
        x[x == shortcut] <- with
        return(x)
    }
    
    downloadList <- downloadList %>%
        replace_shortcut('all', allDatasets$datasetnames) %>%
        replace_shortcut('histology', allDatasets$datasetnames[1:3]) %>%
        replace_shortcut('rna tissue', allDatasets$datasetnames[4:7]) %>%
        replace_shortcut('rna cell type', allDatasets$datasetnames[8:9]) %>%
        replace_shortcut('rna brain region', allDatasets$datasetnames[10:16]) %>%
        replace_shortcut('rna immune cell', allDatasets$datasetnames[17:20]) %>%
        replace_shortcut('rna blood cell', allDatasets$datasetnames[21:24]) %>%
        replace_shortcut('isoform', allDatasets$datasetnames[28:33])

    ## make sure that everything are downloadable, and get downloaded once
    # downloadList <- unique(downloadList)
    # downloadList <- downloadList[downloadList %in% allDatasets$datasetnames]
    
    # filter the datasets to download
    downloadDatasets <-
        filter(allDatasets, datasetnames %in% downloadList)
    
    #initiate the list of processed data to be returned
    loadedData <- list()
    
    ## Download if version is 'built-in' or 'example'
    
    if (version %in% c('example', 'built-in')) {
        message(
            'Only the followings are example/built-in datasets: \n - Normal tissue \n - Pathology \n - Subcellular location \nOther datasets will not be loaded'
        )
        
        downloadDatasets <- filter(
            downloadDatasets,
            datasetnames %in% c('Normal tissue',
                                'Pathology',
                                'Subcellular location')
        )
        
        for (i in names(downloadDatasets$urls)) {
            loadedData[[i]] <- hpa_histology_data[[i]]
        }
        
    } else if (version == "latest") {
        ## Download the requested datasets if version is "latest"
        
        for (i in seq_along(downloadDatasets$urls)) {
            temp <- tempfile()
            download.file(url = downloadDatasets$urls[[i]],
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
            
            if (downloadDatasets$datasetnames[[i]] %in% c(
                'RNA transcript tissue',
                'RNA transcript GTEx retina',
                'RNA transcript immune cells',
                'RNA transcript cell line',
                'RNA transcript pig brain',
                'RNA transcript mouse brain'
            )) {
                loadedData[[i]] <-
                    stats::reshape(
                        loadedData[[i]],
                        direction = "long",
                        varying = list(3:ncol(loadedData[[i]])),
                        v.names = "tpm",
                        timevar = "sample",
                        times = c(colnames(loadedData[[i]][, 3:ncol(loadedData[[i]])]))
                    ) %>%
                    subset(select = -id)
                
            }
            
            ## assign tidy colnames
            colnames(loadedData[[i]]) <- downloadDatasets$tidycols[[i]]
            
        }
        
        ## convert to tibbles
        loadedData <- lapply(loadedData, as_tibble)
        
        names(loadedData) <-
            downloadDatasets$urls %>%
            gsub('.tsv.zip|https://www.proteinatlas.org/download/|https://v21.proteinatlas.org/download/',
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
    
    #
    # if('normal_tissue' %in% names(data)) {
    #     if(!is.null(targetGene)) {
    #         targetGene <- gene_ensembl_convert(targetGene, "gene")
    #         data$normal_tissue <-
    #             filter(data$normal_tissue, gene %in% targetGene)
    #     }
    #
    #     if(!is.null(targetTissue)) {
    #         data$normal_tissue <-
    #             filter(data$normal_tissue, tissue %in% targetTissue)
    #     }
    #
    #     if(!is.null(targetCellType)) {
    #         data$normal_tissue <-
    #             filter(data$normal_tissue, cell_type %in% targetCellType)
    #     }
    # }
    #
    # if('pathology' %in% names(data)) {
    #     if(!is.null(targetGene)) {
    #         targetGene <- gene_ensembl_convert(targetGene, "gene")
    #         data$pathology <-
    #             filter(data$pathology, gene %in% targetGene)
    #     }
    #
    #     if(!is.null(targetCancer)) {
    #         data$pathology <-
    #             filter(data$pathology, cancer %in% targetCancer)
    #     }
    # }
    #
    # if('subcellular_location' %in% names(data)) {
    #     if(!is.null(targetGene)) {
    #         targetGene <- gene_ensembl_convert(targetGene, "gene")
    #         data$subcellular_location <-
    #             filter(data$subcellular_location, gene %in% targetGene)
    #     }
    # }
    #
    # if('rna_tissue' %in% names(data)) {
    #     if(!is.null(targetGene)) {
    #         targetGene <- gene_ensembl_convert(targetGene, "gene")
    #         data$rna_tissue <-
    #             filter(data$rna_tissue, gene %in% targetGene)
    #     }
    #
    #     if(!is.null(targetTissue)) {
    #         data$rna_tissue <-
    #             filter(data$rna_tissue, tissue %in% targetTissue)
    #     }
    # }
    #
    # if('rna_cell_line' %in% names(data)) {
    #     if(!is.null(targetGene)) {
    #         targetGene <- gene_ensembl_convert(targetGene, "gene")
    #         data$rna_cell_line <-
    #             filter(data$rna_cell_line, gene %in% targetGene)
    #     }
    #
    #     if(!is.null(targetCellLine)) {
    #         data$rna_cell_line <-
    #             filter(data$rna_cell_line, cell_line %in% targetCellLine)
    #     }
    # }
    
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
    
    # if('normal_tissue' %in% names(data)) {
    #     availData$normal_tissue <- unique(data$normal_tissue[['tissue']])
    #     availData$normal_cell <- unique(data$normal_tissue[['cell_type']])
    # }
    #
    # if('pathology' %in% names(data)) {
    #     availData$cancer <- unique(data$pathology[['cancer']])
    # }
    #
    # if('subcellular_location' %in% names(data)) {
    #     availData$subcellular_location <- unique(data$subcellular_location[['approved']])%>%
    #         strsplit(';') %>% unlist() %>%
    #         unique() %>% na.omit() %>% as.vector()
    # }
    #
    # if('rna_tissue' %in% names(data)) {
    #     availData$normal_tissue_rna <- unique(data$rna_tissue[['tissue']])
    # }
    #
    # if('rna_cell_line' %in% names(data)) {
    #     availData$cell_line_rna <- unique(data$rna_cell_line[['cell_line']])
    # }
    
    # return(availData)
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
