##################
## Get xml file ##
##################

#' Download and import xml file
#'
#' Download and import individual xml file for a specified protein. This
#' function calls \code{xml2::read_xml()} under the hood.
#'
#' @param targetEnsemblId A string of one ensembl ID, start with ENSG. For
#'   example \code{'ENSG00000131979'}. You can also use HGNC gene symbol and it
#'   will be converted to ensembl id.
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
#' @importFrom xml2 read_xml
#' @importFrom utils download.file
#' @export

hpaXmlGet <- function(targetEnsemblId, version = 'latest') {
    targetEnsemblId <- gene_ensembl_convert(targetEnsemblId, "ensembl")
    
    temp <- tempfile()
    
    download.file(url = version_to_xml_url(targetEnsemblId, version),
                  destfile = temp)
    
    rawXml <- read_xml(x = temp)
    
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
#' @return This function return a data frame of 4 columns.
#' 
#' @family xml functions
#' 
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   protein_df <- hpaXmlProtClass(GCH1xml)
#' }
#' 
#' @importFrom xml2 xml_find_all xml_attrs
#' @export

hpaXmlProtClass <- function(importedXml) {   
    proteinClasses <- xml_attrs(
                          xml_find_all(
                             xml_find_all(importedXml, '//proteinClasses'), 
                             '//proteinClass')
                          )

    proteinClasses <- data.frame(t(sapply(proteinClasses, identity)),
                                 stringsAsFactors = FALSE)
    proteinClasses <- proteinClasses[c("id", "name", "parent_id", "source")]
    proteinClasses[proteinClasses == ''] <- NA
    
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
#'   very brief description of the protein, and a data frame of 2 columns: tissue
#'   (name of tissue available) and imageUrl (link to download the perspective
#'   image)
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   tissue_list <- hpaXmlTissueExprSum(GCH1xml)
#' }
#'
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @export

hpaXmlTissueExprSum <- function(importedXml, downloadImg=FALSE) {
  
  output <- list(summary=NA, img=NA)
  
  # xpath to get to tissueExpression that is not under any antibodies
  tissueExpression <- xml_find_all(importedXml, 'entry/tissueExpression')
  
  output$summary <- xml_text(xml_find_first(tissueExpression, 'summary'))
  
  output$img <- data.frame(tissue = xml_text(xml_find_all(importedXml, 
                                  '//entry/tissueExpression/image/tissue')),
                           imageUrl = xml_text(xml_find_all(importedXml, 
                                    '//entry/tissueExpression/image/imageUrl')),
                           stringsAsFactors = FALSE)
  
  if(downloadImg == TRUE) {
    imageUrlList <- output$img$imageUrl
    # create the list of file name to save
    imageFileList <- paste0(output$img$tissue, '.jpg')
    # loop through each tissue and image URL
    mapply(function(u,d) download.file(u,d, mode='wb'), 
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
#' @return This function returns a data frame of 4 columns, containing information
#'   about the antibodies used in the project for the inquired protein: id,
#'   releaseDate, releaseVersion, and RRID.
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   antibody_df <- hpaXmlAntibody(GCH1xml)
#' }
#'
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom stats setNames
#' @export

hpaXmlAntibody <- function(importedXml) {
  
  hpa_attrs <- xml_attrs(xml_find_all(importedXml, 
                                     '//entry/antibody'))[[1]]
  hpa_attrs <- hpa_attrs[order(names(hpa_attrs))]
  
  output <- data.frame(setNames(rbind(hpa_attrs),
                                names(hpa_attrs)), 
                       row.names = NULL,
                       stringsAsFactors = FALSE)
  
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
#' @return This function returns a list of data frames, each for an antibody. 
#'   Each data frame contains information about all individual samples and their
#'   staining. Due to the variation in amount of information available for these
#'   samples, the number of columns differs, but the data frame essentially
#'   includes: patientId, age, sex, staining, intensity, quantity, location,
#'   imageUrl, snomedCode, and tissueDescription. The last two items may have
#'   more than one column each.
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   tissue_details_df_list <- hpaXmlTissueExpr(GCH1xml)
#' }
#'
#' @import xml2
#' @import xslt
#' @importFrom stats setNames
#' @export

hpaXmlTissueExpr <- function(importedXml) {

    # load xslt script
    xsl_file <- system.file("xsl", "hpaXmlTissueExpr.xsl", package="HPAanalyze")
    style <- read_xml(xsl_file, package = "xslt")

    # transform input
    new_xml <- xml_xslt(importedXml, style)

    # retrieve all patient nodes
    recs <- xml_find_all(new_xml, "//patient")

    # bind each node child text and node name to data frames
    df_list <- lapply(recs, function(r) 
          data.frame(rbind(setNames(xml_text(xml_children(r)), 
                                    xml_name(xml_children(r)))),
                     stringsAsFactors = FALSE)
  
    )

    # normalize and re-order columns
    nms <- unique(unlist(lapply(df_list, names)))

    df_list <- lapply(df_list, function(df) {
        df[nms[!(nms %in% names(df))]] <- NA

        df <- df[c("patientId", "age", "sex", "staining", "intensity", 
                   "quantity", "location", "imageUrl", 
                 names(df)[grep("snomedCode", names(df))],
                 names(df)[grep("tissueDescription", names(df))])]

        return(df)
    })

    return(df_list)
}



#######################################
## Extract RNA expression details ##
#######################################

#' Extract RNA expression details
#'
#' Extract RNA expression information for each sample and url to download
#' images from imported xml document resulted from \code{hpaXmlGet()}.
#'
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#'
#' @param section Select the section to be extracted from xml including:
#'   tissue, cellLine, or bloodCell.
#'
#' @param child Select the multiple child nodes to extract from section
#'   including: level or RNASample.
#'
#' @return This function returns a data frame of listed RNA expressions. 
#'   Each data frame contains information about all entries' RNA expression.
#'   Due to the variation in amount of information different parameters
#'   return different data frames by section (tissue, cellLine, or 
#'   bloodCell) with either level or RNASample information.
#'   
#' @family xml functions
#'
#' @examples
#' \dontrun{
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   rna_df <- hpaXmlRNAExpr(GCH1xml)
#'   rna_df <- hpaXmlRNAExpr(GCH1xml, section="cellLine", child="level")
#'   rna_df <- hpaXmlRNAExpr(GCH1xml, section="bloodCell", child="RNASample")
#' }
#'
#' @import xml2
#' @import xslt
#' @importFrom stats setNames
#' @export

hpaXmlRNAExpr <- function(importedXml, section="tissue", child="level") {
  
  # load xslt script
  xsl_file <- system.file("xsl", "hpaXmlRNAExpr.xsl", package="HPAanalyze")
  style <- read_xml(xsl_file, package = "xslt")
  
  # transform input
  new_xml <- xml_xslt(importedXml, style, 
                      params = list(section_type=section, 
                                    multi_child=child))
  
  # retrieve all patient nodes
  recs <- xml_find_all(new_xml, "//data")
  
  # bind each node child text and node name to data frames
  df_list <- lapply(recs, function(r) 
    data.frame(rbind(setNames(xml_text(xml_children(r)), 
                              xml_name(xml_children(r)))),
               stringsAsFactors = FALSE)
    
  )
  
  # normalize and re-order columns
  nms <- unique(unlist(lapply(df_list, names)))
  
  df_list <- lapply(df_list, function(df) {
    df[nms[!(nms %in% names(df))]] <- NA
    return(df)
  })
  
  final_df <- do.call(rbind, df_list)
  return(final_df)
}

