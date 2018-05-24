##################
## Get xml file ##
##################

#' Download and import xml file
#'
#' Download and import individual xml file for a specified protein. This
#' function calls \code{xml2::read_xml()} under the hood.
#'
#' @param targetEnsemblId A string of one ensembl ID, start with ESNG. For
#'   example \code{'ENSG00000134057'}
#'
#' @param version A string indicate which version to be downloaded. Possible
#'   value:
#'   \itemize{
#'     \item \code{'latest'}: Download latest version. 
#'     \item \code{'vn'} with 'n' is a integer: Download a specific version of
#'     the dataset. For example: 'v18' download version 18. Currently support
#'     version 13 and above.
#'   }
#'   
#' @return This function return an object of class \code{"xml_document"
#'   "xml_node"}. See documentations for package \code{xml2} for more
#'   information.
#'
#' @examples
#'   print('Please run the example below in your console.')
#'   \dontrun{
#'   CCNB1_xml <- hpaXmlGet('ENSG00000134057')
#'   }
#'
#' @import xml2
#' @export

hpaXmlGet <- function(targetEnsemblId, version = 'latest') {
    temp <- tempfile()
    
    raw_xml <- read_xml(download_xml(url = version_to_xml_url(targetEnsemblId, 
                                                              version), 
                                     file = temp))
    
    unlink(temp)
    
    return(raw_xml)
}

#############################
## Extract protein classes ##
#############################

#' Extract protein classes
#'
#' Extract protein class information from imported xml document resulted from
#' \code{hpaXmlGet()}.
#'
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#'
#' @return This function return a tibble of 4 columns.
#'
#' @examples
#'   print('Please run the example below in your console.')
#'   \dontrun{
#'   CCNB1_xml <- hpaXmlGet('ENSG00000134057')
#'   hpaXmlProtClass(CCNB1_xml)
#'   }
#' 
#' @import xml2
#' @export

hpaXmlProtClass <- function(importedXml) {
    protein_classes <- importedXml %>%
        # xpath to get into proteinClasses
        xml_find_all('//proteinClasses') %>%
        xml_find_all('//proteinClass') %>%
        # get attributes, which contains the wanted data, as a list
        xml_attrs() %>%
        # turn attrs into a tibble
        named_vector_list_to_tibble() %>%
        # replace blank cells with NA and convert the result back to tibble
        apply(2, function(x) gsub("^$|^ $", NA, x)) %>% as.tibble()
    
    return(protein_classes)
}

#######################################
## Extract tissue expression summary ##
#######################################

#' Extract tissue expression and download images
#' 
#' Extract tissue expression information and url to download images from
#' imported xml document resulted from \code{hpaXmlGet()}.
#'
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#' @param downloadImg Logical argument. The function will download all image
#'   from the extracted urls into the working folder.
#'
#' @return This function return a list consists of a summary string and a tibble
#'   of 2 columns.
#'
#' @examples
#'   print('Please run the example below in your console.')
#'   \dontrun{
#'   CCNB1_xml <- hpaXmlGet('ENSG00000134057')
#'   hpaXmlTissueExprSum(CCNB1_xml)
#'   }
#'   
#' @import xml2
#' @import dplyr
#' @export

hpaXmlTissueExprSum <- function(importedXml, downloadImg = FALSE) {
    
    ## Just to pass R CMD check
    tissue <- imageUrl <- tissue_expression_img <- NULL
    
    output <- list()
    
    tissue_expression <- importedXml %>%
        # xpath to get to tissueExpression that is not under any antibodies
        xml_find_all('entry/tissueExpression')
    
    output$summary <- tissue_expression %>%
        xml_find_first('summary') %>%
        xml_text()
    
    output$img <- tissue_expression %>%
        xml_find_all('image') %>%
        as_list() %>%
        reshape2::melt() %>%
        spread(key = 'L2', value = 'value') %>%
        select(tissue, imageUrl) %>%
        mutate(tissue = as.character(tissue), imageUrl = as.character(imageUrl))
    
    if(downloadImg == TRUE) {
        image_url_list <- output$img$imageUrl
        # create the list of file name to save
        image_file_list <- paste0(tissue_expression_img$tissue, '.jpg')
        # loop through the 
        Map(function(u,d) download.file(u,d, mode = 'wb'), image_url_list, image_file_list)
    }
    
    return(output)
}


##################################
## Extract antibody information ##
##################################

#' Extract antibody information
#' 
#' Extract information about the antibodies used for a specific protein.
#' 
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#'   
#' @return This function returns a tibble of 4 columns.
#' 
#' @examples
#'   print('Please run the example below in your console.')
#'   \dontrun{
#'   CCNB1_xml <- hpaXmlGet('ENSG00000134057')
#'   hpaXmlAntibody(CCNB1_xml)
#'   }
#'   
#' @import xml2
#' @import dplyr
#' @export

hpaXmlAntibody <- function(importedXml) {
    output <- importedXml %>%
        xml_find_all('entry/antibody') %>%
        xml_attrs() %>%
        named_vector_list_to_tibble()
    
    return(output)
}



#######################################
## Extract tissue expression details ##
#######################################



