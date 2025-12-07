library(dplyr)
library(readr)

hpa_download_list <- 
    read_csv('data_raw/download_list.csv', col_type = 'cdcc') |>
    mutate(table = trimws(table, 'both'))