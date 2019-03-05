##################
## Get xml file ##
##################

#' Download and import xml file
#'
#' Download and import individual xml file for a specified protein. This
#' function calls \code{xml2::read_xml()} under the hood.
#'
#' @param targetEnsemblId A string of one ensembl ID, start with ESNG. For
#'   example \code{'ENSG00000131979'}
#'
#' @param version A string indicate which version to be downloaded. Possible
#'   value: \itemize{ \item \code{'latest'}: Download latest version. \item
#'   \code{'v?'} with '?' is a integer: Download a specific version of the
#'   dataset. For example: 'v18' download version 18. Currently support version
#'   13 and above. }
#'
#' @return This function return an object of class \code{"xml_document"
#'   "xml_node"} containing the content of the imported XML file. (See
#'   documentations for package \code{xml2} for more information.)
#'
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#' }
#'
#' @importFrom xml2 read_xml download_xml
#' @export

hpaXmlGet <- function(targetEnsemblId, version='latest') {
    temp <- tempfile()
    
    rawXml <- read_xml(download_xml(url=version_to_xml_url(targetEnsemblId, 
                                                           version), 
                                    file=temp))
    
    unlink(temp)
    
    return(rawXml)
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
#' @family xml functions
#' 
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlProtClass(GCH1xml)
#' }
#' 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom tibble as_tibble
#' @export

hpaXmlProtClass <- function(importedXml) {
    proteinClasses <- importedXml %>%
        # xpath to get into proteinClasses
        xml_find_all('//proteinClasses') %>%
        xml_find_all('//proteinClass') %>%
        # get attributes, which contains the wanted data, as a list
        xml_attrs() %>%
        # turn attrs into a tibble
        named_vector_list_to_tibble() %>%
        # replace blank cells with NA and convert the result back to tibble
        apply(2, function(x) gsub("^$|^ $", NA, x)) %>% as_tibble()
    
    return(proteinClasses)
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
#' @return This function return a list consists of a summary string, which is a
#'   very brief description of the protein, and a tibble of 2 columns: tissue
#'   (name of tissue available) and imageUrl (link to download the perspective
#'   image)
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlTissueExprSum(GCH1xml)
#' }
#'
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @import dplyr
#' @importFrom tidyr spread
#' @export

hpaXmlTissueExprSum <- function(importedXml, downloadImg=FALSE) {
    
    output <- list()
    
    tissueExpression <- importedXml %>%
        # xpath to get to tissueExpression that is not under any antibodies
        xml_find_all('entry/tissueExpression')
    
    output$summary <- tissueExpression %>%
        xml_find_first('summary') %>%
        xml_text()
    
    output$img <- tissueExpression %>%
        xml_find_all('image') %>%
        as_list() %>%
        reshape2::melt() %>%
        spread(key='L2', value='value') %>%
        select(tissue, imageUrl) %>%
        mutate(tissue=as.character(tissue), 
               imageUrl=as.character(imageUrl))
    
    if(downloadImg == TRUE) {
        imageUrlList <- output$img$imageUrl
        # create the list of file name to save
        imageFileList <- paste0(output$img$tissue, '.jpg')
        # loop through the 
        Map(function(u,d) download.file(u,d, mode='wb'), 
            imageUrlList, imageFileList)
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
#' @return This function returns a tibble of 4 columns, containing information
#'   about the antibodies used in the project for the inquired protein: id,
#'   releaseDate, releaseVersion, and RRID.
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlAntibody(GCH1xml)
#' }
#'
#' @importFrom xml2 xml_find_all xml_attrs
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
#' @return This function returns a list of tibbles, each for an antibody. Each
#'   tibble contains information about all individual samples and their
#'   staining. Due to the variation in amount of information available for these
#'   samples, the number of columns differs, but the tibble essentially
#'   includes: patientId, age, sex, staining, intensity, quantity, location,
#'   imageUrl, snomedCode, and tissueDescription. The last two items may have
#'   more than one column each.
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlTissueExpr(GCH1xml)
#' }
#'
#' @import xml2
#' @import dplyr
#' @export

hpaXmlTissueExpr <- function(importedXml) {
    antibodyNodes <- importedXml %>% xml_find_all('entry/antibody')
    
    lapply(antibodyNodes, function(antibodyNode){
        tissueExpressionNodes <- xml_find_all(antibodyNode, 'tissueExpression')
        lapply(tissueExpressionNodes, function(tissueExpressionNode){
            dataNodes <- xml_find_all(tissueExpressionNode, 'data')
            lapply(dataNodes, function(dataNode) {
                patient_nodes_to_tibble(xml_find_all(dataNode, 'patient'))
            })
        }) %>% unlist(recursive=FALSE) %>% bind_rows() -> x
        
        if (!(0 %in% dim(x))) {
            select(x, patientId, age, sex, staining, intensity, quantity,
                   location, imageUrl, starts_with('snomedCode'),
                   starts_with('tissueDescription'))
        } else {x}
        
    }) -> result

    return(result)
}

## Define patient_nodes_to_tibble() for simpler parsing =======================

#' @importFrom xml2 xml_find_first xml_text xml_find_all xml_attrs 

patient_nodes_to_tibble <- function(patientNodes) {
    lapply(patientNodes,
           function(patientNode) {
               pair <- c('sex'='sex',
                         'age'='age',
                         'patientId'='patientId',
                         'staining'='level[@type=\'staining\']',
                         'intensity'='level[@type=\'intensity\']',
                         'quantity'='quantity',
                         'location'='location')
               
               vapply(pair,
                      FUN.VALUE=character(1),
                      function(x) {
                          temp <- c()
                          xml_find_first(patientNode, x) %>%
                              xml_text() -> temp[names(x)]
                      }) -> info
               
               sampleNode <- xml_find_first(patientNode, 'sample')
               samp <- c()
               xml_find_all(sampleNode, 'snomedParameters/snomed') %>%
                   xml_attrs() %>% named_vector_list_to_tibble() %>%
                   unlist() -> samp
               xml_find_all(sampleNode, 'assayImage/image/imageUrl') %>%
                   xml_text() -> samp['imageUrl']
               result <- c(info, samp)
               
               return(result)
               
           }) %>% named_vector_list_to_tibble() -> result
    
    return(result)
}