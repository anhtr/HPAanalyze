x <- CCNB1_xml %>%
    xml_find_all('entry/antibody')


for (i in seq_along(x)) {
    xml_list[[i]] <- xml_attrs(x[i])
    print(x[i])
}

print(xml_list)
str(xml_list)
named_vector_list_to_tibble(xml_list)

y <- CCNB1_xml %>%
    xml_find_all('entry/antibody') %>%
    xml_attrs() %>%
    named_vector_list_to_tibble()


for (i in seq_along(x)) {
    x1 <- xml_find_all(x[i], 'tissueExpression')
    for (i1 in seq_along(x1)) {
        x2 <- xml_find_all(x1[i1], 'data')
        for (i2 in seq_along(x2)){
            patient_nodes <- xml_find_all(x2[i2], 'patient')
        }
        
    }
}

lapply(x, function(antibody_node){
    x1 <- xml_find_all(antibody_node, 'tissueExpression')
    lapply(x1, function(tissueExpression_node){
        x2 <- xml_find_all(tissueExpression_node, 'data')
        lapply(x2, function(data_node) {
            patient_nodes_to_tibble(xml_find_all(data_node, 'patient'))
        })
    }) %>% unlist(recursive = FALSE) %>% bind_rows()
})

lapply(x1, function(tissueExpression_node){
    x2 <- xml_find_all(tissueExpression_node, 'data')
    lapply(x2, function(data_node) {
        patient_nodes_to_tibble(xml_find_all(data_node, 'patient'))
    })
}) %>% unlist(recursive = FALSE) %>% bind_rows()

lapply(x2, function(data_node) {
    patient_nodes_to_tibble(xml_find_all(data_node, 'patient'))
})

patient_nodes_to_tibble(xml_find_all(x2[4], 'patient'))

patient_nodes_to_tibble <- function(patient_nodes) {

lapply(patient_nodes,
       function(node) {
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
                      xml_find_first(node, x) %>%
                          xml_text() -> temp[names(x)]
                  }) -> info
           
           sample_node <- xml_find_first(node, 'sample')
           samp <- c()
           xml_find_all(sample_node, 'snomedParameters/snomed') %>%
               xml_attrs() %>% named_vector_list_to_tibble() %>%
               unlist() -> samp
           xml_find_all(sample_node, 'assayImage/image/imageUrl') %>%
               xml_text() -> samp['imageUrl']

           # lapply(xml_find_all(node, 'sample'),
           #        function(x) {
           #            samp <- c()
           #            xml_find_all(x, 'snomedParameters/snomed') %>%
           #                xml_attrs() %>% named_vector_list_to_tibble() %>%
           #                unlist() -> samp
           #            xml_find_all(x, 'assayImage/image/imageUrl') %>%
           #                xml_text() -> samp['imageUrl']
           #            return(samp)}
           # ) %>% named_vector_list_to_tibble() %>% unlist() -> sample
           
           result <- c(info, samp)
           
           return(result)
           
       }) %>% named_vector_list_to_tibble()
    # named_vector_list_to_tibble() %>%
    # mutate(imageUrl_1 = if_else(!is.na(imageUrl1), imageUrl1, imageUrl),
    #        snomedCode_1_1 = if_else(!is.na(snomedCode11), snomedCode11, snomedCode1),
    #        tissueDescription_1_1 = if_else(!is.na(tissueDescription11), tissueDescription11, tissueDescription1),
    #        snomedCode_1_2 = if_else(!is.na(snomedCode21), snomedCode21, snomedCode2),
    #        tissueDescription_1_2 = if_else(!is.na(tissueDescription21), tissueDescription21, tissueDescription2),
    #        imageUrl_2 = imageUrl2,
    #        snomedCode_2_1 = snomedCode12,
    #        tissueDescription_2_1 = tissueDescription12,
    #        snomedCode_2_2 = snomedCode22,
    #        tissueDescription_2_2 = tissueDescription22) %>%
    # select(patientId, age, sex, intensity, staining, location,
    #        snomedCode_1_1, tissueDescription_1_1, snomedCode_1_2, tissueDescription_1_2, imageUrl_1,
    #        snomedCode_2_1, tissueDescription_2_1, snomedCode_2_2, tissueDescription_2_2, imageUrl_2)
}


for (i3 in patient_nodes) {

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
               xml_find_first(x3[i3], x) %>%
                   xml_text() -> temp[names(x)]
           }) -> x5
    
    lapply(xml_find_all(x3[i3], 'sample'), 
           function(x) {
               samp <- c()
               xml_find_all(x, 'snomedParameters/snomed') %>%
                   xml_attrs() %>% named_vector_list_to_tibble() %>% 
                   unlist() -> samp
               xml_find_all(x, 'assayImage/image/imageUrl') %>% 
                   xml_text() -> samp['imageUrl']
               return(samp)}
           ) %>% named_vector_list_to_tibble() %>% unlist() -> x7
        
    
    x5 <- c(x5, x7)
}

z1 <- do.call(c, unlist(z, recursive=FALSE))
z2 <- as.data.frame(z)
