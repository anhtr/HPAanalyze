test_that("hpaSearch returns a tibble with the requested columns", {
    out <- hpaSearch(search = "TP53", columns = c("g", "gs", "eg"))
    expect_s3_class(out, "tbl_df")
    expect_true("TP53" %in% out$Gene)
    expect_true(all(c("Gene", "Gene synonym", "Ensembl") %in% names(out)))
})

test_that("hpaSearch also accepts columns as a comma-separated string", {
    out <- hpaSearch(search = "TP53", columns = "g,gs,eg")
    expect_true(all(c("Gene", "Gene synonym", "Ensembl") %in% names(out)))
})

test_that("hpaSearch with exact = TRUE filters down to a single gene match", {
    out <- hpaSearch(search = "TP53", columns = c("g", "gs", "eg"), exact = TRUE)
    expect_equal(nrow(out), 1)
    expect_equal(out$Gene, "TP53")
})

test_that("hpaSearch errors when exact = TRUE but 'g' is not in columns", {
    expect_error(
        hpaSearch(search = "TP53", columns = c("gs", "eg"), exact = TRUE),
        "exact = TRUE requires"
    )
})
