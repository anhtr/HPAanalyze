## Data manipulation as part of xml parsing ===================================

#' @import dplyr
#' @importFrom tibble tibble

named_vector_list_to_tibble <- function(x) {
    
    # define a blank tibble
    tibble_x <- tibble(index=NA, attr=NA, value=NA)
    # loop though the list, turn vectors into tibble and bind them together
    for (i in seq_along(x)) {
        tibble_i <- tibble(index=i, attr=names(x[[i]]), value=x[[i]])
        tibble_x <- bind_rows(tibble_x, tibble_i)
    }
    
    # process tibble_x into final product
    ## the old way used tidyr::spread
    # tibble_x <- tibble_x %>%
    #     # remove the NA row resulted from defining tibble_x
    #     filter(!is.na(index)) %>%
    #     # spead tibble_x into tidy format
    #     spread(attr, value) %>%
    #     # remove the index column
    #     select(-index)
    
    ## the new way used stats::reshape
    tibble_x <- tibble_x %>%
        # remove the NA row resulted from defining tibble_x
        filter(!is.na(index)) %>%
        as.data.frame() %>%
        # reshape tibble_x into tidy format
        reshape(
            direction = "wide",
            timevar = "attr",
            idvar = "index"
        ) %>%
        # remove the index column
        select(-index) %>%
        # convert back to tibble
        as_tibble()
    
    # remove the "value." prefix in column names
    names(tibble_x) <- substring(names(tibble_x), 7)
    
    return(tibble_x)
}


## Generate urls to download datasets =========================================

version_to_download_urls <- function(x) {
    if (x == 'latest') {
        x <- 'www'
    }
    
    urls <- c(paste0('https://', x, 
                     '.proteinatlas.org/download/normal_tissue.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/pathology.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/subcellular_location.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/rna_tissue.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/rna_celline.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/transcript_rna_tissue.tsv.zip'),
              paste0('https://', x, 
                     '.proteinatlas.org/download/transcript_rna_celline.tsv.zip'))
    
    names(urls) <- c('normal_tissue',
                     'pathology',
                     'subcellular_location',
                     'rna_tissue',
                     'rna_cell_line',
                     'transcript_rna_tissue',
                     'transcript_rna_cell_line')
    
    return(urls)
}


## Generate url to download xml ===============================================

version_to_xml_url <- function(id, vers) {
    if (vers == 'latest') {
        vers <- 'www'
    }
    
    return(paste0('https://', vers, '.proteinatlas.org/', id, '.xml'))
}

## Convert between ensembl id and gene name ===================================

gene_ensembl_convert <- function(id, convert_to) {
    id_c <- id
    warn <- FALSE
    
    for (i in seq_along(id)) {
        if (convert_to == "ensembl") {
            if (substr(id[i], 1, 4) != "ENSG") {
                id_c[i] <- lookup_df$ensembl[lookup_df$gene == id[i]][1]
                if (is.na(id_c[i])) {
                    id_c[i] <- id[i]
                    warn <- TRUE
                }
            } else {
                id_c[i] <- id[i]
            }
        } else if (convert_to == "gene") {
            if (substr(id[i], 1, 4) == "ENSG") {
                id_c[i] <- lookup_df$gene[lookup_df$ensembl == id[i]][1]
                if (is.na(id_c[i])) {
                    id_c[i] <- id[i]
                    warn <- TRUE
                }
            } else {
                id_c[i] <- id[i]
            }
        }
    }
    
    if (warn == TRUE)
        message(
            "Couldn't find all requested genes in lookup table. There may be typo(s) or your gene(s) may not currently be supported by HPA."
        )
    
    return(id_c)
}