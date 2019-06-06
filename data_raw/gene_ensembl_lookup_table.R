temp <- tempfile("query", fileext=c(".tsv.gz"))
download.file(url="https://www.proteinatlas.org/search?format=tsv", destfile = temp, method = "curl", mode = "wb")
query_df <- readr::read_tsv(temp)
unlink(temp)

lookup_df <- dplyr::select(query_df, "gene" = Gene, "ensembl" = Ensembl)

usethis::use_data(lookup_df,  internal = TRUE, overwrite = TRUE)