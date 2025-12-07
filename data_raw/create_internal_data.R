# Run both data-building scripts first (so both data frames exist)
source("data_raw/gene_ensembl_lookup_table.R")
source("data_raw/make_download_list.R")

# Now save both as internal data in one go
usethis::use_data(
    lookup_df,
    hpa_download_list,
    internal = TRUE,
    overwrite = TRUE
)