test_that("gene_ensembl_convert converts gene symbols to ensembl ids", {
    out <- gene_ensembl_convert(c("TP53", "EGFR"), "ensembl")
    expect_equal(out, c("ENSG00000141510", "ENSG00000146648"))
})

test_that("gene_ensembl_convert converts ensembl ids to gene symbols", {
    out <- gene_ensembl_convert(c("ENSG00000141510", "ENSG00000146648"), "gene")
    expect_equal(out, c("TP53", "EGFR"))
})

test_that("gene_ensembl_convert leaves already-converted ids untouched", {
    expect_equal(gene_ensembl_convert("ENSG00000141510", "ensembl"), "ENSG00000141510")
    expect_equal(gene_ensembl_convert("TP53", "gene"), "TP53")
})

test_that("gene_ensembl_convert warns and passes through unmatched ids", {
    expect_message(
        out <- gene_ensembl_convert("NOTAREALGENE", "ensembl"),
        "Couldn't find all requested genes"
    )
    expect_equal(out, "NOTAREALGENE")
})

test_that("version_to_xml_url builds the expected urls", {
    expect_equal(
        version_to_xml_url("ENSG00000131979", "latest"),
        "https://www.proteinatlas.org/ENSG00000131979.xml"
    )
    expect_equal(
        version_to_xml_url("ENSG00000131979", "v18"),
        "https://v18.proteinatlas.org/ENSG00000131979.xml"
    )
})

test_that("is_null_data falls back to the built-in dataset when data is NULL", {
    expect_message(out <- is_null_data(NULL), "No data provided")
    expect_identical(out, HPAanalyze::hpa_histology_data)
})

test_that("is_null_data passes through non-NULL data unchanged", {
    dummy <- list(a = 1)
    expect_silent(out <- is_null_data(dummy))
    expect_identical(out, dummy)
})

test_that("named_vector_list_to_tibble reshapes a list of named vectors", {
    x <- list(
        c(a = "1", b = "2"),
        c(a = "3", b = "4")
    )
    out <- named_vector_list_to_tibble(x)
    expect_s3_class(out, "tbl_df")
    expect_equal(names(out), c("a", "b"))
    expect_equal(nrow(out), 2)
    expect_equal(out$a, c("1", "3"))
    expect_equal(out$b, c("2", "4"))
})
