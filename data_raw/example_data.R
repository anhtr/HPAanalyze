library(HPAanalyze)
hpa_histology_data <- hpaDownload(downloadList = "histology",
                                  version = "v19")

hpa_histology_data$metadata <- list(HPAversion = "19",
                                    date = Sys.time(),
                                    packageVersion = "1.5.2")

usethis::use_data(hpa_histology_data, internal = FALSE, overwrite = FALSE)
