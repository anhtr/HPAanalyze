hpa_subset <- function(data,
                       target_gene = NULL,
                       target_tissue = NULL,
                       target_cell_type = NULL,
                       target_cancer = NULL,
                       target_cell_line = NULL) {
    
    if('normal_tissue' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$normal_tissue <- filter(data$normal_tissue, gene %in% target_gene)
        }
        
        if(!is.null(target_tissue)) {
            data$normal_tissue <- filter(data$normal_tissue, tissue %in% target_tissue)
        }
        
        if(!is.null(target_cell_type)) {
            data$normal_tissue <- filter(data$normal_tissue, cell_type %in% target_cell_type)
        }
    }
    
    if('pathology' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$pathology <- filter(data$pathology, gene %in% target_gene)
        }
        
        if(!is.null(target_cancer)) {
            data$pathology <- filter(data$pathology, cancer %in% target_cancer)
        }
    }
    
    if('subcellular_location' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$subcellular_location <- filter(data$subcellular_location, gene %in% target_gene)
        }
    }
    
    if('rna_tissue' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$rna_tissue <- filter(data$rna_tissue, gene %in% target_gene)
        }
        
        if(!is.null(target_tissue)) {
            data$rna_tissue <- filter(data$rna_tissue, tissue %in% target_tissue)
        }        
    }
    
    if('rna_cell_line' %in% names(data)) {
        if(!is.null(target_gene)) {
            data$rna_cell_line <- filter(data$rna_cell_line, gene %in% target_gene)
        }
        
        if(!is.null(target_cell_line)) {
            data$rna_cell_line <- filter(data$rna_cell_line, cell_line %in% target_cell_line)
        }       
    }
    
    return(data)
}
