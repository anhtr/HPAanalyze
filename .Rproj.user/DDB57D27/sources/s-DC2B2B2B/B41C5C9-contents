hpa_vis_subcell <- function(data, 
                            target_gene, 
                            color = c('white', 'black'),
                            custom_theme = FALSE) {
    
    plot_data <- data$subcellular_location %>%
        filter(gene %in% target_gene) %>%
        mutate(location = strsplit(go_id, ';')) %>%
        unnest(location) %>%
        select(location, gene) %>%
        table() %>%
        as.tibble() %>%
        mutate(n = factor(n, levels = c('0', '1')))
    
    level_colors <- c('0' = color[1],
                      '1' = color[2])
    
    plot <- ggplot(plot_data, aes(x = gene, y = location)) +
        geom_tile(aes(fill = n)) +
        scale_x_discrete(limits = target_gene) +
        scale_fill_manual(values = level_colors)
    
    if(!custom_theme) {
        plot <- plot + 
            ylab('Subcellular locations') +
            xlab('Genes') +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
            theme(legend.position="none")
    }
    
    return(plot)       
}