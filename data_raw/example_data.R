library(HPAanalyze)
hpa_histology_data <- hpaDownload(downloadList = "histology")

hpa_histology_data$metadata <- list(HPAversion = "20.1",
                                    date = Sys.time(),
                                    packageVersion = "1.9.1")

usethis::use_data(hpa_histology_data, internal = FALSE, overwrite = TRUE)

