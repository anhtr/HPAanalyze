hpa_vis_pathology <- function(data, 
                              target_gene, 
                              target_cancer = NULL, 
                              color = c('#ffffb2', '#fecc5c', '#fd8d3c', '#e31a1c'),
                              custom_theme = FALSE) {
    
    plot_data <- data$pathology %>%
        filter(gene %in% target_gene)
    
    if(!is.null(target_cancer)) {
        plot_data <- filter(plot_data, cancer %in% target_cancer)
    }
    
    plot_data <- plot_data %>%
        select(gene, cancer, high, medium, low, not_detected) %>%
        rename('High' = 'high', 'Medium' = 'medium', 'Low' = 'low', 'Not detected' = 'not_detected') %>%
        melt(measure.vars = c('High', 'Medium', 'Low', 'Not detected'),
             variable.name = 'level',
             value.name = 'patient_count')      
    
    level_colors <- c('Not detected' = color[1],
                      'Low' = color[2],
                      'Medium' = color[3],
                      'High' = color[4])
    
    plot <- ggplot(plot_data, aes(x = gene, y = patient_count, fill = level)) +
        geom_bar(stat = 'identity', position = 'fill') +
        scale_x_discrete(limits = target_gene) +
        scale_fill_manual(values = level_colors) +
        facet_wrap(~ cancer)
    
    if(!custom_theme) {
        plot <- plot + 
            ylab('Patient portions') +
            xlab('Genes') +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    return(plot)       
}