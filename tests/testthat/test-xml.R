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

test_that("hpaXmlParse returns a flat named list of tibbles", {
    out <- hpaXmlParse(ccnb1xml)
    expect_type(out, "list")
    expect_true(length(out) > 0)
    expect_true(all(vapply(out, tibble::is_tibble, logical(1))))
    expect_false(is.null(names(out)))
    expect_false(any(duplicated(names(out))))
})

test_that("hpaXmlParse extracts the same protein classes as hpaXmlProtClass", {
    out <- hpaXmlParse(ccnb1xml)
    expect_true(all(c("id", "parent_id", "name", "source") %in%
                        names(out$proteinClasses_proteinClass)))
    expect_equal(nrow(out$proteinClasses_proteinClass),
                 nrow(hpaXmlProtClass(ccnb1xml)))
    expect_true("Transporters" %in% out$proteinClasses_proteinClass$name)
})

test_that("hpaXmlParse gives proteinEvidence exactly 3 rows", {
    out <- hpaXmlParse(ccnb1xml)
    expect_equal(nrow(out$proteinEvidence_evidence), 3)
    expect_true(all(c("HPA", "MS", "UniProt") %in%
                        out$proteinEvidence_evidence$source))
})

test_that("hpaXmlParse tables join together by their id/foreign-key columns", {
    out <- hpaXmlParse(ccnb1xml)
    expect_true(all(out$tissueCell$data_id %in% out$data$data_id))
    expect_true(all(out$patient$data_id %in% out$data$data_id))
    expect_true(all(out$patient_level$patient_id %in% out$patient$patient_id))
    expect_true(all(out$sample$patient_id %in% out$patient$patient_id))
    expect_true(all(out$snomedParameters$sample_id %in% out$sample$sample_id))
    expect_true(all(out$snomedParameters_snomed$snomedParameters_id %in%
                         out$snomedParameters$snomedParameters_id))
    expect_true(all(stats::na.omit(out$image$tissueExpression_id) %in%
                         out$tissueExpression$tissueExpression_id))
})

test_that("hpaXmlParse output does not depend on whether an optional child is present", {
    # <identifier> and <proteinClasses> can both contain child elements per
    # the schema (xref, proteinClass respectively), so they must always be
    # their own tibble named after their own tag -- never folded into an
    # "entry_identifier"-style table just because this particular gene's
    # xref/proteinClass list happens to be empty.
    minimalXml <- xml2::read_xml(paste0(
        '<proteinAtlas schemaVersion="3.1">',
        '<entry version="25" url="https://www.proteinatlas.org/ENSG00000000000">',
        '<name>TESTGENE</name>',
        '<identifier id="ENSG00000000000" db="Ensembl" version="1" ',
        'assembly="GRCh38" gencodeVersion="1"></identifier>',
        '<proteinClasses></proteinClasses>',
        '<proteinEvidence evidence="Evidence at protein level">',
        '<evidence source="HPA" evidence="Evidence at protein level"/>',
        '<evidence source="MS" evidence="Evidence at protein level"/>',
        '<evidence source="UniProt" evidence="Evidence at protein level"/>',
        '</proteinEvidence>',
        '</entry></proteinAtlas>'
    ))

    out <- hpaXmlParse(minimalXml)
    expect_true(all(c("entry", "identifier", "proteinClasses",
                       "proteinEvidence", "proteinEvidence_evidence") %in%
                        names(out)))
    expect_false("entry_identifier" %in% names(out))
    expect_equal(out$entry$name, "TESTGENE")
    expect_equal(nrow(out$identifier), 1)
    expect_equal(nrow(out$proteinClasses), 1)
    # no proteinClass rows for this gene: table simply absent, not present-but-empty
    expect_false("proteinClasses_proteinClass" %in% names(out))
})

test_that("hpaXmlParse supports schema branches the bundled fixture doesn't exercise", {
    # cellTypeExpression is not present in ENSG00000134057.xml; this proves
    # the generic engine needs no per-element code to support it.
    branchXml <- xml2::read_xml(paste0(
        '<proteinAtlas schemaVersion="3.1">',
        '<entry version="25" url="https://www.proteinatlas.org/ENSG00000000001">',
        '<name>BRANCHGENE</name>',
        '<identifier id="ENSG00000000001" db="Ensembl" version="1" ',
        'assembly="GRCh38" gencodeVersion="1"/>',
        '<proteinClasses/>',
        '<proteinEvidence evidence="Evidence at protein level">',
        '<evidence source="HPA" evidence="Evidence at protein level"/>',
        '<evidence source="MS" evidence="Evidence at protein level"/>',
        '<evidence source="UniProt" evidence="Evidence at protein level"/>',
        '</proteinEvidence>',
        '<cellTypeExpression technology="scRNAseq" assayType="single cell type">',
        '<cellTypeSpecificity category="Cell type enhanced">',
        '<cellType>Hepatocytes</cellType>',
        '</cellTypeSpecificity>',
        '<cellTypeDistribution>Detected in some</cellTypeDistribution>',
        '<singleCellTypeExpression name="Hepatocytes" type="tissue" ',
        'unitRNA="nTPM" expRNA="12.3"/>',
        '<singleCellTypeExpression name="T-cells" type="immune" ',
        'unitRNA="nTPM" expRNA="0.5"/>',
        '</cellTypeExpression>',
        '</entry></proteinAtlas>'
    ))

    out <- hpaXmlParse(branchXml)
    expect_true("cellTypeExpression" %in% names(out))
    expect_equal(out$cellTypeExpression$cellTypeDistribution, "Detected in some")
    expect_equal(nrow(out$cellTypeExpression_singleCellTypeExpression), 2)
    expect_true(all(out$cellTypeSpecificity_cellType$cellTypeSpecificity_id %in%
                         out$cellTypeSpecificity$cellTypeSpecificity_id))
})
