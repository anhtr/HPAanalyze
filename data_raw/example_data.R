library(HPAanalyze)
hpa_histology_data <- hpaDownload(downloadList = "histology")

hpa_histology_data$metadata <- list(HPAversion = "24.0",
                                    Ensemblversion = "109",
                                    date = Sys.time())

usethis::use_data(hpa_histology_data, internal = FALSE, overwrite = TRUE)


