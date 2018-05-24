#' @import dplyr
#' @import tibble

named_vector_list_to_tibble <- function(x) {
    
    ## Just to pass R CMD check
    index <- value <- NULL
    
    # define a blank tibble
    tibble_x = tibble(index = NA, attr = NA, value = NA)
    # loop though the list, turn vectors into tibble and bind them together
    for (i in 1:length(x)) {
        tibble_i <- tibble(index = i, attr = names(x[[i]]), value = x[[i]])
        tibble_x <- bind_rows(tibble_x, tibble_i)
    }
    # process tibble_x into final product
    tibble_x <- tibble_x %>%
        # remove the NA row resulted from defining tibble_x
        filter(!is.na(index)) %>%
        # spead tibble_x into tidy format
        spread(attr, value) %>%
        # remove the index column
        select(-index)
    return(tibble_x)
}

#' @import dplyr
#' @import tibble

version_to_download_urls <- function(x) {
    if (x == 'latest') {
        x = 'www'
    }
    
    urls <- c(paste0('https://', x, '.proteinatlas.org/download/normal_tissue.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/pathology.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/subcellular_location.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/rna_tissue.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/rna_celline.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/transcript_rna_tissue.tsv.zip'),
              paste0('https://', x, '.proteinatlas.org/download/transcript_rna_celline.tsv.zip'))
    
    names(urls) <- c('normal_tissue',
                     'pathology',
                     'subcellular_location',
                     'rna_tissue',
                     'rna_cell_line',
                     'transcript_rna_tissue',
                     'transcript_rna_cell_line')
    
    return(urls)
}

version_to_xml_url <- function(id, vers) {
    if (vers == 'latest') {
        vers = 'www'
    }
    
    return(paste0('https://', vers, '.proteinatlas.org/', id, '.xml'))
}
