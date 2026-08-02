test_that("hpaDownload with downloadList=NULL lists available table names", {
    latestVersion <- paste0("v", max(hpa_download_list$version_number))
    tableNames <- hpaDownload(downloadList = NULL, version = latestVersion)
    expect_type(tableNames, "character")
    expect_true(all(c("normal_tissue", "pathology", "subcellular_location") %in% tableNames))
})

test_that("hpaDownload returns the built-in dataset for the example/built-in version", {
    expect_message(
        out <- hpaDownload(downloadList = "histology", version = "example"),
        "example/built-in datasets"
    )
    expect_identical(out, HPAanalyze::hpa_histology_data)

    expect_message(
        out2 <- hpaDownload(downloadList = "all", version = "built-in")
    )
    expect_identical(out2, HPAanalyze::hpa_histology_data)
})

test_that(".replace_shortcut expands a shortcut in place", {
    out <- .replace_shortcut(c("a", "all", "b"), "all", c("x", "y"))
    expect_equal(out, c("a", "x", "y", "b"))
})

test_that(".replace_shortcut leaves input untouched when shortcut is absent", {
    out <- .replace_shortcut(c("a", "b"), "all", c("x", "y"))
    expect_equal(out, c("a", "b"))
})

test_that(".expand_download_list expands the 'histology' shortcut", {
    out <- .expand_download_list("histology", hpa_download_list)
    expect_equal(out, c("normal_tissue", "pathology", "subcellular_location"))
})
