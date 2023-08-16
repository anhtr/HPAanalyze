library(dplyr)
library(readr)

hpa_download_list <- 
    read_csv('data_raw/download_list.csv', col_type = 'cdcc') |>
    mutate(table = trimws(table, 'both'))

usethis::use_data(hpa_download_list, internal = TRUE, overwrite = TRUE)