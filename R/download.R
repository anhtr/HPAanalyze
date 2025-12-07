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
#'   Common values:
#'   \itemize{
#'     \item \code{'normal_tissue'}
#'     \item \code{'pathology'}
#'     \item \code{'subcellular_location'}
#'     }
#'   For the full list of possible values for a specific version, set
#'   downloadList as NULL: \code{hpaDownload(downloadList = NULL, version =
#'   <version>)}
#'
#'   You can also use the following shortcuts:
#'   \itemize{
#'     \item \code{'all'}: download everything (not recommended!!!)
#'     \item \code{'histology'}: same as \code{c('normal_tissue', 'pathology',
#'     'subcellular_location')}
#'   }
#'   See \url{https://www.proteinatlas.org/about/download} for more information.
#'
#' @param version A string indicate which version to be downloaded. Possible
#'   value:
#'   \itemize{
#'     \item \code{'latest'}: Download latest version. Due to the constantly
#'     changing nature of the API, this will download the latest version known
#'     to work with this package. Require Internet connection. This is the
#'     default option.
#'     \item \code{'example'} or \code{'built-in'}: Load the built-in histology
#'     dataset from 'HPAanalyze' ('hpa_histology_data'). Do not require internet
#'     connection.
#'     \item \code{'v23'}: version 23
#'     \item \code{'v24'}: version 24
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
    
    # Set a longer timeout for downloads
    .set_download_timeout(10000)
    
    # If downloadList is NULL, return available table names for the version
    if (is.null(downloadList)) {
        table_names <- hpa_download_list %>%
            filter(version == !!version) %>%
            pull(table)
        return(table_names)
    }
    
    # Expand download shortcuts
    downloadListClean <- .expand_download_list(downloadList, hpa_download_list)
    
    # Filter metadata for datasets to download
    downloadDatasets <- .filter_metadata(hpa_download_list, downloadListClean, version)
    
    loadedData <- list()
    
    # Example/built-in version returns example dataset
    if (version %in% c('example', 'built-in')) {
        message(
            'Only the followings are example/built-in datasets: \n - Normal tissue \n - Pathology \n - Subcellular location \nOther datasets will not be loaded'
        )
        loadedData <- hpa_histology_data
    } else {
        # Download for each dataset link
        for (i in seq_along(downloadDatasets$link)) {
            loadedData[[i]] <- .download_and_import_dataset(downloadDatasets$link[[i]])
        }
        loadedData <- .list_to_tibble(loadedData)
        names(loadedData) <- downloadDatasets$table
        loadedData <- .clean_datasets(loadedData)
    }
    
    return(loadedData)
}

# Helper function to set timeout
.set_download_timeout <- function(timeout = 10000) {
    op <- options(timeout = timeout)
    on.exit(options(op), add = TRUE)
    invisible(op)
}

# Helper function to replace shortcut names with full item list
.replace_shortcut <- function(x, shortcut, with) {
    x <- rep(x, 1 + (length(with) - 1)*(x == shortcut))
    x[x == shortcut] <- with
    return(x)
}

# Helper to expand download list
.expand_download_list <- function(downloadList, hpa_download_list) {
    downloadList %>%
        .replace_shortcut('all', hpa_download_list$table) %>%
        .replace_shortcut('histology',
                          c('normal_tissue',
                            'pathology',
                            'subcellular_location'))
}

# Helper function to filter dataset metadata for requested version and tables
.filter_metadata <- function(hpa_download_list, downloadListClean, version) {
    hpa_download_list %>%
        filter(version == !!version) %>%
        filter(table %in% downloadListClean)
}

# Helper to download and import a single dataset from URL (zip)
.download_and_import_dataset <- function(url) {
    temp <- tempfile()
    download.file(url = url, destfile = temp)
    firstfile <- unzip(temp, list = TRUE)$Name[1]
    dat <- read.delim2(
        unz(temp, firstfile),
        stringsAsFactors = FALSE,
        check.names = FALSE,
        strip.white = TRUE,
        sep = "\t",
        na.strings = c("", " ")
    )
    unlink(temp)
    return(dat)
}

# Helper to convert all data frames in a list to tibbles
.list_to_tibble <- function(data_list) {
    lapply(data_list, as_tibble)
}

# Helper to assign or rename columns for datasets
.clean_datasets <- function(loadedData) {
    # For normal tissue
    if (!is.null(loadedData$normal_tissue)) {
        loadedData$normal_tissue <- loadedData$normal_tissue %>%
            select(
                ensembl = Gene,
                gene = `Gene name`,
                tissue = Tissue,
                cell_type = `Cell type`,
                level = Level,
                reliability = Reliability
            )
    }
    # For pathology
    if (!is.null(loadedData$pathology)) {
        loadedData$pathology <- loadedData$pathology %>%
            select(
                ensembl = Gene,
                gene = `Gene name`,
                cancer = Cancer,
                high = High,
                medium = Medium,
                low = Low,
                not_detected = `Not detected`
            )
    }
    # For subcellular_location
    if (!is.null(loadedData$subcellular_location)) {
        loadedData$subcellular_location <- loadedData$subcellular_location %>%
            select(
                ensembl = Gene,
                gene = `Gene name`,
                reliability = Reliability,
                main_location = `Main location`,
                additional_location = `Additional location`,
                extracellular_location = `Extracellular location`,
                enhanced = Enhanced,
                supported = Supported,
                approved = Approved,
                uncertain = Uncertain,
                single_cell_var_intensity = `Single-cell variation intensity`,
                single_cell_var_spatial = `Single-cell variation spatial`,
                cell_cycle_dependency = `Cell cycle dependency`,
                go_id = `GO id`
            )
    }
    loadedData
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
    
    # Ensure the input data is valid (e.g., not NULL or improperly formatted)
    data <- is_null_data(data = data)
    
    # If gene names are provided, convert them to the standard format using a helper function
    if (!is.null(targetGene))
        targetGene <- gene_ensembl_convert(targetGene, "gene")
    
    # Define a sub-function to apply filtering based on the target criteria
    subsetting <- function(df) {
        
        # Filter by gene names if the 'gene' column exists and targetGene is specified
        if (!is.null(targetGene) & any(names(df) == "gene")) {
            df <- filter(df, gene %in% targetGene)
        }
        
        # Filter by tissue type if the 'tissue' column exists and targetTissue is specified
        if (!is.null(targetTissue) & any(names(df) == "tissue")) {
            df <- filter(df, tissue %in% targetTissue)
        }
        
        # Filter by cell type if the 'cell_type' column exists and targetCellType is specified
        if (!is.null(targetCellType) & any(names(df) == "cell_type")) {
            df <- filter(df, cell_type %in% targetCellType)
        }
        
        # Filter by cancer type if the 'cancer' column exists and targetCancer is specified
        if (!is.null(targetCancer) & any(names(df) == "cancer")) {
            df <- filter(df, cancer %in% targetCancer)
        }
        
        # Filter by cell line if the 'cell_line' column exists and targetCellLine is specified
        if (!is.null(targetCellLine) & any(names(df) == "cell_line")) {
            df <- filter(df, cell_line %in% targetCellLine)
        }
        
        # Return the filtered dataframe
        return(df)
    }
    
    # Apply the subsetting function to each dataset in the list (if multiple datasets provided)
    data <- lapply(data, subsetting)
    
    # Return the filtered data (a list of dataframes or tibbles)
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
    # Ensure input data is valid and standardized
    data <- is_null_data(data = data)
    
    # Define a helper function to extract available parameters from each dataset
    listing <- function(df) {
        # Attempt to extract unique values for each of the possible filter parameters
        params <- lapply(c(
            "tissue" = "tissue",
            "cell_type" = "cell_type",
            "cancer" = "cancer",
            "cell_line" = "cell_line"
        ), function(x)
            unique(df[[x]]))  # Get unique values for the column, if it exists
        
        # Remove any parameters that returned NULL (i.e., don't exist in this dataset)
        params[lengths(params) != 0]
    }
    
    # Apply the listing function to each dataset in the list
    availData <- lapply(data, listing)
    
    # Remove entries from the result where no parameters were found (empty lists)
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
    # If the user specifies Excel format
    if (fileType == 'xlsx') {
        # Save all datasets in a single Excel file with multiple sheets
        write.xlsx(data, file = paste0(fileName, ".xlsx"))
    }
    
    # If the user specifies CSV format
    if (fileType == 'csv') {
        # Loop through each dataset in the list
        for (i in 1:length(data)) {
            # Save each dataset as a separate .csv file
            write.csv(data[[i]],
                      file = paste0(fileName, "_", names(data[i]), ".csv"))
        }
    }
    
    # If the user specifies TSV format
    if (fileType == 'tsv') {
        # Loop through each dataset in the list
        for (i in 1:length(data)) {
            # Save each dataset as a separate .tsv file
            write.table(data[[i]],
                        file = paste0(fileName, "_", names(data[i]), ".tsv"),
                        sep = "\t")  # Use tab separator for TSV
        }
    }
}
