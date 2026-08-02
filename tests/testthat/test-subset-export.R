data("hpa_histology_data")

test_that("hpaSubset filters by gene, mixing HGNC symbols and ensembl ids", {
    sub <- hpaSubset(
        data = hpa_histology_data,
        targetGene = c("TP53", "ENSG00000146648"), # ENSG00000146648 == EGFR
        targetTissue = "Breast"
    )
    expect_setequal(unique(sub$normal_tissue$gene), c("TP53", "EGFR"))
    expect_setequal(unique(sub$normal_tissue$tissue), "Breast")
    expect_setequal(unique(sub$pathology$gene), c("TP53", "EGFR"))
})

test_that("hpaSubset falls back to the built-in dataset when data is NULL", {
    expect_message(
        sub <- hpaSubset(data = NULL, targetGene = "TP53"),
        "No data provided"
    )
    expect_setequal(unique(sub$normal_tissue$gene), "TP53")
})

test_that("hpaSubset returns unfiltered data when no target is specified", {
    sub <- hpaSubset(data = hpa_histology_data)
    expect_identical(sub, hpa_histology_data)
})

test_that("hpaListParam lists the parameters available for subsetting", {
    params <- hpaListParam(data = hpa_histology_data)
    expect_true(all(c("normal_tissue", "pathology") %in% names(params)))
    expect_true("tissue" %in% names(params$normal_tissue))
    expect_true("cell_type" %in% names(params$normal_tissue))
    expect_true("cancer" %in% names(params$pathology))
})

test_that("hpaExport writes one xlsx file with a sheet per dataset", {
    sub <- hpaSubset(data = hpa_histology_data, targetGene = "TP53", targetTissue = "Breast")
    fileName <- file.path(tempdir(), "hpaanalyze_test_export_xlsx")
    on.exit(unlink(paste0(fileName, ".xlsx")), add = TRUE)

    hpaExport(data = sub, fileName = fileName, fileType = "xlsx")

    expect_true(file.exists(paste0(fileName, ".xlsx")))
    expect_setequal(openxlsx::getSheetNames(paste0(fileName, ".xlsx")), names(sub))
})

test_that("hpaExport writes one csv file per dataset", {
    sub <- hpaSubset(data = hpa_histology_data, targetGene = "TP53", targetTissue = "Breast")
    fileName <- file.path(tempdir(), "hpaanalyze_test_export_csv")
    expectedFiles <- paste0(fileName, "_", names(sub), ".csv")
    on.exit(unlink(expectedFiles), add = TRUE)

    hpaExport(data = sub, fileName = fileName, fileType = "csv")

    expect_true(all(file.exists(expectedFiles)))
})

test_that("hpaExport writes one tsv file per dataset", {
    sub <- hpaSubset(data = hpa_histology_data, targetGene = "TP53", targetTissue = "Breast")
    fileName <- file.path(tempdir(), "hpaanalyze_test_export_tsv")
    expectedFiles <- paste0(fileName, "_", names(sub), ".tsv")
    on.exit(unlink(expectedFiles), add = TRUE)

    hpaExport(data = sub, fileName = fileName, fileType = "tsv")

    expect_true(all(file.exists(expectedFiles)))
})
