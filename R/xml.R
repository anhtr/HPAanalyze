##################
## Get xml file ##
##################

#' Download and import xml file
#'
#' Download and import individual xml file for a specified protein. This
#' function calls \code{xml2::read_xml()} under the hood.  It is important to
#' note that the data that HPA provides on their website and through xml files
#' are not one-to-one equivalents.
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
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
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
#' \code{hpaXmlGet()}.  It is important to note that the data that HPA provides
#' on their website and through xml files are not one-to-one equivalents.
#' 
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#' 
#' @return This function return a tibble of 4 columns.
#' 
#' @family xml functions
#' 
#' @examples
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlProtClass(GCH1xml)
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
#' imported xml document resulted from \code{hpaXmlGet()}.  It is important to
#' note that the data that HPA provides on their website and through xml files
#' are not one-to-one equivalents.
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
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlTissueExprSum(GCH1xml)
#'
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @import dplyr
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
        # lapply(list_to_df) %>%
        bind_rows() %>%
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
#' Extract information about the antibodies used for a specific protein.  It is
#' important to note that the data that HPA provides on their website and
#' through xml files are not one-to-one equivalents.
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
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlAntibody(GCH1xml)
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
#' images from imported xml document resulted from \code{hpaXmlGet()}. It is
#' important to note that the data that HPA provides on their website and
#' through xml files are not one-to-one equivalents. For example, xml files
#' usually only provide one of the two histology image for each patient.
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
#' \donttest{
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


#####################################
## Parse the entire xml in one go  ##
#####################################

# Two small lookup tables, mechanically derived from the Human Protein Atlas
# xml schema (proteinatlas.xsd, schema version 3.1 / HPA25,
# https://www.proteinatlas.org/download/proteinatlas.xsd), that drive every
# structural decision hpaXmlParse() makes. Keeping these schema-derived
# (rather than inferring structure from what a particular gene's xml happens
# to contain) is what makes the output shape reproducible across genes and
# upgradable with minimal effort when HPA revises the schema -- update these
# two vectors to match the new schema, no other code needs to change.

# Tag names whose *type definition* in the schema can contain child elements
# (a <xs:sequence>/<xs:choice>/<xs:all> with at least one <xs:element>
# inside), as opposed to types that are simpleContent or attribute-only.
# hpaXmlParse() always recurses into these and gives them their own tibble
# (row per occurrence, plain tag name), even when a particular occurrence
# happens to have zero children in a particular gene's file -- e.g.
# <identifier> can contain <xref> children, so it is always its own
# "identifier" table, never folded into a "entry_identifier" table just
# because one gene's xref list happens to be empty.

.hpaXmlEntityTags <- c(
    'antibody', 'antibodyData', 'antibodyTargetWeights', 'assayImage',
    'blotLanes', 'cancerExpression', 'cellExpression', 'cellSample',
    'cellTypeExpression', 'cellTypeSpecificity', 'chain', 'channels',
    'customAssay', 'data', 'entry', 'gfpData', 'identifier', 'image',
    'imageGroup', 'interactionstructure', 'lane', 'mouseBrainStaining',
    'patient', 'proteinArray', 'proteinAtlas', 'proteinClasses',
    'proteinEvidence', 'proteinstructure', 'rnaExpression', 'rnaSpecificity',
    'sample', 'snomedParameters', 'stainingData', 'structure', 'subAssay',
    'tissueCell', 'tissueExpression', 'westernBlot'
)

# Tag names whose type definition is simpleContent or attribute-only (never
# has element children), i.e. everything not in .hpaXmlEntityTags above.
# Kept as an explicit list (rather than "anything not an entity") so that a
# genuinely new tag introduced by a future schema version -- not yet added
# to either list -- is detected and falls back to inspecting that occurrence
# directly, instead of being silently misclassified.

.hpaXmlLeafTags <- c(
    'BACid', 'RNASample', 'age', 'antibodyDilution', 'antigenSequence',
    'assayDescription', 'cellLine', 'cellType', 'cellTypeDistribution',
    'cellTypeExpressionCluster', 'cellcycledependent', 'channel',
    'copyright', 'customAssayType', 'downRegulation', 'evidence',
    'imageUrl', 'imageUrlTif', 'immuneCell', 'level', 'lineage', 'location',
    'name', 'numberOfCells', 'patientId', 'percentageStainedCells',
    'positiveStaining', 'predictedLocation', 'proteinClass', 'quantity',
    'region', 'rnaCancerDistribution', 'rnaCancerSpecificity',
    'rnaDistribution', 'rnaExpressionCluster', 'sex',
    'singleCellTypeExpression', 'sirna', 'snomed', 'staining', 'summary',
    'survivalAnalysis', 'synonym', 'tag', 'tissue', 'validation',
    'verification', 'weight', 'xref'
)

# Leaf tag names that the schema allows to occur more than once in at least
# one context (maxOccurs="unbounded" or a number > 1). hpaXmlParse() always
# turns these into their own tibble, even when only one instance is present
# in a particular gene's file, so the output shape never depends on which
# gene was parsed (e.g. a <tissueCell> with one <level> gets the same
# tissueCell_level table as one with four).

.hpaXmlRepeatableTags <- c(
    'synonym', 'xref', 'proteinClass', 'evidence', 'imageUrl',
    'imageUrlTif', 'channel', 'cellType', 'location', 'snomed', 'weight',
    'level', 'staining', 'singleCellTypeExpression'
)

## Internal helpers for the generic recursive engine ===========================

# Attributes and (if the node has no element children) text content of a
# single node, as a named list of length-1 character values.

.xml_own_fields <- function(node) {
    fields <- as.list(xml_attrs(node))
    if (length(xml_children(node)) == 0) {
        text <- trimws(xml_text(node, trim = FALSE))
        if (nzchar(text)) fields$value <- text
    }
    return(fields)
}

# Simple mutable counter/table accumulators, implemented with environments so
# the recursive flattener below doesn't need to thread return values through
# every call.

.xml_next_id <- function(counters, tag) {
    current <- if (exists(tag, envir = counters, inherits = FALSE))
        get(tag, envir = counters) else 0L
    current <- current + 1L
    assign(tag, current, envir = counters)
    return(current)
}

.xml_add_row <- function(tables, tableName, row) {
    existing <- if (exists(tableName, envir = tables, inherits = FALSE))
        get(tableName, envir = tables) else list()
    existing[[length(existing) + 1]] <- row
    assign(tableName, existing, envir = tables)
}

# Recursively flatten one xml node into `tables`, a flat named list of
# tibbles keyed by tag name (or "<parentTag>_<tag>" for leaf/property-style
# repeated elements, since the same tag can mean different things under
# different parents, e.g. tissueCell_level vs patient_level). Every row gets
# a foreign key column "<parentTag>_id" pointing back at the row it came
# from, so the resulting tables can be joined like a small relational
# database instead of nested many levels deep. Entity tables additionally get
# a surrogate key column "<tag>_id", since their own rows are referenced by
# children further down; leaf/property tables are never referenced, so they
# carry the parent's foreign key only.

.xml_flatten_node <- function(node, tag, tables, counters,
                              fkCol = NULL, fkVal = NULL) {
    row <- .xml_own_fields(node)
    if (!is.null(fkCol)) row[[fkCol]] <- fkVal

    thisId <- .xml_next_id(counters, tag)
    idCol <- paste0(tag, '_id')
    row[[idCol]] <- thisId

    children <- xml_children(node)

    if (length(children) > 0) {
        childTags <- xml_name(children)

        for (ctag in unique(childTags)) {
            group <- children[childTags == ctag]

            # Classify by the tag's schema type, not by whether this
            # particular occurrence happens to have children -- an
            # <identifier> with no <xref> children in one gene's file must
            # still be classified the same way as one that has several, or
            # the output shape would depend on which gene was parsed.
            isEntity <- ctag %in% .hpaXmlEntityTags
            if (!isEntity && !(ctag %in% .hpaXmlLeafTags)) {
                # Tag not covered by either schema-derived list (e.g. a new
                # element introduced by a future HPA schema revision this
                # package doesn't know about yet): fall back to inspecting
                # this occurrence directly rather than failing.
                isEntity <- !all(vapply(group,
                                         function(x) length(xml_children(x)) == 0,
                                         logical(1)))
            }

            if (isEntity) {
                # nested entity: same kind of record wherever it appears, so
                # it keeps its own plain tag name and gets its own row(s),
                # disambiguated by whichever foreign key column is populated
                for (child in group) {
                    .xml_flatten_node(child, ctag, tables, counters,
                                      fkCol = idCol, fkVal = thisId)
                }
            } else {
                forceTable <- ctag %in% .hpaXmlRepeatableTags

                if (length(group) == 1 && !forceTable) {
                    # singleton, schema-guaranteed to never repeat: merge
                    # directly into the parent row as plain column(s)
                    childFields <- .xml_own_fields(group[[1]])
                    for (fname in names(childFields)) {
                        col <- if (identical(fname, 'value')) ctag
                               else paste0(ctag, '_', fname)
                        row[[col]] <- childFields[[fname]]
                    }
                } else {
                    # may repeat somewhere in the schema: always its own
                    # tibble, named after its immediate parent, regardless
                    # of how many times it happens to occur in this file
                    tableName <- paste0(tag, '_', ctag)
                    for (child in group) {
                        childFields <- .xml_own_fields(child)
                        childFields[[idCol]] <- thisId
                        .xml_add_row(tables, tableName, childFields)
                    }
                }
            }
        }
    }

    .xml_add_row(tables, tag, row)
    return(invisible(thisId))
}

#' Parse an entire imported xml document into a list of tibbles
#'
#' Parse everything available in an imported xml document resulted from
#' \code{hpaXmlGet()}, based on the Human Protein Atlas xml schema
#' (\url{https://www.proteinatlas.org/download/proteinatlas.xsd}). Unlike the
#' other \code{hpaXml} functions, which each extract one specific piece of
#' information, \code{hpaXmlParse()} generically walks the entire xml tree and
#' normalizes it into a flat, single-level named list of tibbles, similar to a
#' small relational database extracted from the xml file.
#'
#' Every row, except the single top-level \code{result$entry} row, carries a
#' foreign key column named \code{"<parent>_id"} pointing back at the row (in
#' another tibble) it belongs to. For example, rows in
#' \code{result$tissueCell} carry a \code{data_id} that matches the
#' \code{data_id} of their parent row in \code{result$data}, and
#' \code{result$tissueCell_level} rows carry a \code{tissueCell_id} pointing
#' back at \code{result$tissueCell}.
#'
#' Two kinds of tibble are produced. Tibbles for nested entities
#' (\code{entry}, \code{data}, \code{patient}, \code{image},
#' \code{antibody}, ...) keep their plain tag name, since they represent the
#' same kind of record wherever they occur, and additionally have a surrogate
#' key column named \code{"<name>_id"} that their own children point at.
#' Tibbles for leaf/property-style elements are instead named
#' \code{"<parent>_<tag>"} (e.g. \code{tissueCell_level},
#' \code{patient_location}), because some xml tags are reused with different
#' meaning under different parents (for example \code{level} means staining
#' intensity under \code{tissueCell} but RNA abundance under \code{data});
#' nothing refers back to their rows, so they carry the parent's foreign key
#' only and have no surrogate key of their own.
#'
#' In a leaf/property tibble, the element's own text content is held in a
#' column literally named \code{"value"} and its xml attributes keep their
#' own names as columns, so e.g. \code{result$tissueCell_cellType$value}
#' holds the cell type name. A leaf element that the schema guarantees can
#' never repeat gets no tibble at all and is merged into its parent's row
#' instead: its text content becomes a column named after the tag (e.g.
#' \code{result$entry$name}, \code{result$data$tissue}) and any attributes
#' become columns named \code{"<tag>_<attribute>"}.
#'
#' Because the function is driven entirely by the xml structure itself
#' (whether an xml tag is, per the schema, allowed to occur more than once)
#' rather than by hand-written per-element extraction code, its output shape
#' does not depend on which gene was parsed, gracefully tolerates xml files
#' that are missing elements the schema allows, and requires no code changes
#' to support new elements introduced by future HPA schema revisions.
#' \code{hpaXmlParse()} only includes tibbles for elements that are actually
#' present in the imported document; an element absent for a given gene (for
#' example an antibody with no western blot data) simply has no
#' corresponding entry in the returned list.
#'
#' @param importedXml Input an xml document object resulted from a
#'   \code{hpaXmlGet()} call.
#'
#' @return This function returns a flat, named list of tibbles. See Details
#'   for how the tibbles are named and joined to each other.
#'
#' @family xml functions
#'
#' @examples
#'   GCH1xml <- hpaXmlGet('ENSG00000131979')
#'   hpaXmlParse(GCH1xml)
#'
#' @import xml2
#' @import dplyr
#' @importFrom tibble as_tibble
#' @export

hpaXmlParse <- function(importedXml) {
    entryNodes <- importedXml %>% xml_find_all('entry')

    tables <- new.env(parent = emptyenv())
    counters <- new.env(parent = emptyenv())

    for (entryNode in entryNodes) {
        .xml_flatten_node(entryNode, 'entry', tables, counters)
    }

    tableNames <- ls(envir = tables)
    result <- lapply(tableNames, function(nm) {
        rows <- get(nm, envir = tables)
        bind_rows(lapply(rows, as_tibble))
    })
    names(result) <- tableNames

    return(result)
}