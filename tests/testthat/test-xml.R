# These tests use a real HPA xml file bundled as a test fixture (also used,
# unmodified, by the "offline xml" vignette) so they can run without network
# access.
ccnb1xml <- xml2::read_xml(test_path("testdata", "ENSG00000134057.xml"))

test_that("hpaXmlProtClass extracts protein class information", {
    out <- hpaXmlProtClass(ccnb1xml)
    expect_s3_class(out, "tbl_df")
    expect_equal(names(out), c("source", "id", "parent_id", "name"))
    expect_true(nrow(out) > 0)
    expect_true("Transporters" %in% out$name)
})

test_that("hpaXmlTissueExprSum extracts a summary and an image url tibble", {
    out <- hpaXmlTissueExprSum(ccnb1xml)
    expect_type(out, "list")
    expect_type(out$summary, "character")
    expect_s3_class(out$img, "tbl_df")
    expect_equal(names(out$img), c("tissue", "imageUrl"))
    expect_true(nrow(out$img) > 0)
})

test_that("hpaXmlAntibody extracts antibody information", {
    out <- hpaXmlAntibody(ccnb1xml)
    expect_s3_class(out, "tbl_df")
    expect_true(all(c("id", "releaseDate", "releaseVersion") %in% names(out)))
    expect_true(nrow(out) > 0)
})

test_that("hpaXmlTissueExpr extracts one tibble of samples per antibody", {
    out <- hpaXmlTissueExpr(ccnb1xml)
    expect_type(out, "list")
    expect_true(length(out) > 0)
    expect_s3_class(out[[1]], "tbl_df")
    expect_true(all(
        c("patientId", "age", "sex", "staining", "intensity", "quantity", "location") %in%
            names(out[[1]])
    ))
})

test_that("hpaXml dispatches to the requested extraction functions", {
    out <- hpaXml(ccnb1xml, extractType = c("ProtClass", "Antibody"))
    expect_equal(names(out), c("ProtClass", "Antibody"))
    expect_s3_class(out$ProtClass, "tbl_df")
    expect_s3_class(out$Antibody, "tbl_df")
})

test_that("hpaXml accepts an already-imported xml_document as input", {
    out <- hpaXml(ccnb1xml, extractType = "ProtClass")
    expect_equal(names(out), "ProtClass")
})
