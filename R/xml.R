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

#' Extract tissue expression details
#' 
#' Extract tissue expression information for each sample and url to download
#' images from imported xml document resulted from \code{hpaXmlGet()}.
#' 
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#'   
#' @return This function returns a list of tibbles, each for an antibody.
#' 
#' @examples
#'   print('Please run the example below in your console.')
#'   \dontrun{
#'   CCNB1_xml <- hpaXmlGet('ENSG00000134057')
#'   hpaXmlTissueExpr(CCNB1_xml)
#'   }
#' 
#' @import xml2
#' @import dplyr
#' @export

hpaXmlTissueExpr <- function(importedXml) {
    antibody_nodes <- importedXml %>% xml_find_all('entry/antibody')
    
    lapply(antibody_nodes, function(antibody_node){
        tissueExpression_nodes <- xml_find_all(antibody_node, 'tissueExpression')
        lapply(tissueExpression_nodes, function(tissueExpression_node){
            data_nodes <- xml_find_all(tissueExpression_node, 'data')
            lapply(data_nodes, function(data_node) {
                patient_nodes_to_tibble(xml_find_all(data_node, 'patient'))
            })
        }) %>% unlist(recursive = FALSE) %>% bind_rows() -> x
        
        if (!(0 %in% dim(x))) {
            select(x, patientId, age, sex, staining, intensity, quantity,
                   location, imageUrl, starts_with('snomedCode'),
                   starts_with('tissueDescription'))
        } else {x}
        
    }) -> result

    return(result)
}

## Define patient_nodes_to_tibble() for simpler parsing =======================

patient_nodes_to_tibble <- function(patient_nodes) {
    lapply(patient_nodes,
           function(patient_node) {
               pair <- c('sex' = 'sex',
                         'age' = 'age',
                         'patientId' = 'patientId',
                         'staining' = 'level[@type=\'staining\']',
                         'intensity' = 'level[@type=\'intensity\']',
                         'quantity' = 'quantity',
                         'location' = 'location')
               
               vapply(pair,
                      FUN.VALUE = character(1),
                      function(x) {
                          temp <- c()
                          xml_find_first(patient_node, x) %>%
                              xml_text() -> temp[names(x)]
                      }) -> info
               
               sample_node <- xml_find_first(patient_node, 'sample')
               samp <- c()
               xml_find_all(sample_node, 'snomedParameters/snomed') %>%
                   xml_attrs() %>% named_vector_list_to_tibble() %>%
                   unlist() -> samp
               xml_find_all(sample_node, 'assayImage/image/imageUrl') %>%
                   xml_text() -> samp['imageUrl']
               result <- c(info, samp)
               
               return(result)
               
           }) %>% named_vector_list_to_tibble() -> result
    
    return(result)
}