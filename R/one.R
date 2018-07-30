####################
## Visualize data ##
####################

#' Visualize data in one function
#'
#' This function is an universal visualization function that allow calling other
#' hpaVis functions via a single function call. By default, this function will
#' use the dataset bundled with HPAanalyze, and provide a grid of all available
#' plots. The types of plots in the output can be specified via the
#' \code{visType} argument. If only one plot type is specified, this function
#' will return the exact same output as the specific hpaVis function used to
#' create the plot.
#'
#' @param data Input the list object generated by \code{hpa_download()} or
#'   \code{hpa_subset()}. By default this function use the example dataset
#'   bundled with HPAanalyze, which is from version 18 of the HPA program.
#' @param targetGene Vector of strings of HGNC gene symbols. By default it is
#'   set to \code{c('TP53', 'RB1', 'MYC', 'KRAS', 'EGFR')}.
#' @param visType Vector of strings indicating which plots will be generated.
#'   Currently available values are \code{'all'}, \code{'Tissue'},
#'   \code{'Patho'}, \code{Cancer}, \code{Subcell}.
#' @param color Vector of 4 colors used to depict different expression levels.
#' @param customTheme Logical argument. If \code{TRUE}, the function will return
#'   a barebone ggplot2 plot to be customized further.
#' @param ... Additional arguments to be passed downstream to other hpaVis
#'   functions being called behind the scene. These arguments includes
#'   \code{targetTissue}, \code{targetCellType}, \code{targetCancer}. See
#'   documentation for individual hpaVis functions for more information.
#'
#' @return If multiple visType is chosen, this function will return multiple
#'   graphs in one panel. If only one visType is chosen, this function will
#'   return a ggplot2 plot object, which can be further modified if desirable.
#'   See help file for each of the hpaVis function for more information about
#'   individual graphs.
#'
#' @examples
#'   hpaVis()
#'
#' @export

hpaVis <- function(data=NULL,
                   targetGene=NULL,
                   visType='all',
                   color=c('#ffffb2', '#fecc5c', '#fd8d3c', '#e31a1c'),
                   customTheme=FALSE,
                   ...) {
    
    # Initiate empty variables
    infoDisp <- FALSE
    plot <- list()
    
    # Check if data is provided or not
    if (is.null(data)) {
        message('No data provided. Use version 18.')
        data = HPAanalyze::hpa_downloaded_histology_v18
    }
    
    # Check if targetGene is provided
    if (is.null(targetGene)) {
        message('targetGene variable not specified, default to TP53, RB1, MYC, KRAS and EGFR.')
        targetGene <- c('TP53', 'RB1', 'MYC', 'KRAS', 'EGFR')
        infoDisp <- TRUE
    }
    
    # Generate a list of visType if choose 'all'
    if (visType == 'all') {
        visType <- c('Tissue', 'Patho', 'Subcell')
    }
    
    # Make tissue plot
    if ('Tissue' %in% visType) {
        if (!exists('targetTissue')) {
            message('targetTissue variable not specified, default to breast.')
            targetTissue <- 'breast'
            infoDisp <- TRUE
        }
        
        if (!exists('targetCellType')) {
            message('targetCellType variable not specified, visualize all.')
            targetCellType <- NULL
            infoDisp <- TRUE
        }
        
        plot$Tissue <- hpaVisTissue(data=data, 
                                    targetGene=targetGene,
                                    targetTissue=targetTissue,
                                    targetCellType=targetCellType,
                                    color=color,
                                    customTheme=customTheme)
    }
    
    # Make cancer plot
    if ('Patho' %in% visType | 'Cancer' %in% visType) {
        if (!exists('targetCancer')) {
            message('targetCancer variable not specified, default to breast cancer')
            targetCancer <- 'breast cancer'
            infoDisp <- TRUE
        }
        
        plot$Patho <- hpaVisPatho(data=data,
                                  targetGene=targetGene,
                                  targetCancer=targetCancer,
                                  color=color,
                                  customTheme=customTheme)
    }
    
    # Make Subcell plot
    if ('Subcell' %in% visType) {
        plot$Subcell <- hpaVisSubcell(data=data,
                                      targetGene=targetGene,
                                      color=color,
                                      customTheme=customTheme)
    }
    
    # Show a message if any parameter is not defined
    if (infoDisp) {
        message('Use hpaListParam() to list possible values for target variables.')
    }
    
    # return a grid if more than one plot, otherwise return the ggplot
    if (length(plot) > 1) { 
        return(cowplot::plot_grid(plotlist = plot))
    } else {
        return(plot[[1]])
    }
}


#######################################
## Universal XML extraction function ##
#######################################

#' Extract details about an individual protein from XML file in one function
#'
#' This function is the umbrella function for the hpaXml function family. It
#' take the input of either one Ensembl gene id or a imported XML object
#' resulting from a \code{hpaXmlGet()} function call. By default, it will
#' extract all information available for HPAanalyze user from the XML file by
#' calling every hpaXml function and put all results into a list.
#'
#' @param inputXml Input can be either one Ensembl gene id or a imported XML
#'   object resulting from a \code{hpaXmlGet()} function call.
#' @param extractType A vector of strings indicate which information is desired
#'   for extraction. By default this function will call all \code{hpaXml}
#'   functions available. Other options are \code{'ProtClass'},
#'   \code{'TissueExprSum'}, \code{'Antibody'}, \code{'TissueExpr'}.
#' @param ... Additional arguments to be passed downstream to other hpaXml
#'   functions being called behind the scene. See help files of other hpaXml
#'   functions for more information.
#'
#' @return This function returns a list. Each element of the list is information
#'   extracted from the XML file specified using other hpaXml functions. See
#'   help file for each XML function for more information.
#'
#' @example hpaXml(inputXml='ENSG00000131979', extractType=c('ProtClass',
#' 'TissueExprSum', 'Antibody'))
#'
#' @export

hpaXml <- function(inputXml, 
                   extractType = 'all',
                   ...) {
    
    # initate variables
    xml_output <- list()
    
    # Test if input is a string or an xml object. If string, download the xml.
    if (class(inputXml) == 'character') {
        inputXml <- hpaXmlGet(targetEnsemblId=inputXml,
                              version = 'latest')
    }
    
    # Generate a list of extractType
    if (extractType == 'all') {
        extractType <- c('ProtClass', 'TissueExprSum', 'Antibody', 'TissueExpr')
    }
    
    # Extract protein classes
    if ('ProtClass' %in% extractType) {
        xml_output$ProtClass <- hpaXmlProtClass(importedXml=inputXml)
    }
    
    # Extract tissue expression summary
    if ('TissueExprSum' %in% extractType) {
        xml_output$TissueExprSum <- hpaXmlTissueExprSum(importedXml=inputXml,
                                                        downloadImg=FALSE)
    }
    
    # Extract antibody
    if ('Antibody' %in% extractType) {
        xml_output$Antibody <- hpaXmlProtClass(importedXml=inputXml)
    }
    
    # Extract tissue expression
    if ('TissueExpr' %in% extractType) {
        xml_output$TissueExpr <- hpaXmlTissueExpr(importedXml=inputXml)
    }
    
    return(xml_output)
}