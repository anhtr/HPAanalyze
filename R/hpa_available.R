hpa_available <- function(data) {
    available_data <- list()
    
    if('normal_tissue' %in% names(data)) {
        available_data$normal_tissue <- unique(data$normal_tissue[['tissue']])
        available_data$normal_cell <- unique(data$normal_tissue[['cell_type']])
    }
    
    if('pathology' %in% names(data)) {
        available_data$cancer <- unique(data$pathology[['cancer']])
    }
    
    if('subcellular_location' %in% names(data)) {
        available_data$subcellular_location <- unique(data$subcellular_location[['approved']])%>%
            strsplit(';') %>% unlist() %>% unique() %>% na.omit() %>% as.vector()
    }
    
    if('rna_tissue' %in% names(data)) {
        available_data$normal_tissue_rna <- unique(data$rna_tissue[['tissue']])
    }
    
    if('rna_cell_line' %in% names(data)) {
        available_data$cell_line_rna <- unique(data$rna_cell_line[['cell_line']])
    }
    
    return(available_data)
}
