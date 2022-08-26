library(HPAanalyze)
hpa_histology_data <- hpaDownload(downloadList = "histology")

hpa_histology_data$metadata <- list(HPAversion = "21.1",
                                    Ensemblversion = "103.38",
                                    date = Sys.time(),
                                    packageVersion = "1.15.0")

usethis::use_data(hpa_histology_data, internal = FALSE, overwrite = TRUE)

