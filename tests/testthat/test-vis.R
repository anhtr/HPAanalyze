data("hpa_histology_data")

test_that("hpaVisTissue returns a ggplot object", {
    plot <- suppressMessages(
        hpaVisTissue(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetTissue = c("Breast", "Cerebellum")
        )
    )
    expect_s3_class(plot, "ggplot")
})

test_that("hpaVisTissue warns when targetCellType is not specified", {
    expect_message(
        hpaVisTissue(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetTissue = "Breast"
        ),
        "targetCellType"
    )
})

test_that("hpaVisPatho returns a ggplot object", {
    plot <- suppressMessages(
        hpaVisPatho(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetCancer = c("breast cancer", "glioma")
        )
    )
    expect_s3_class(plot, "ggplot")
})

test_that("hpaVisSubcell returns a ggplot object", {
    plot <- suppressMessages(
        hpaVisSubcell(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR")
        )
    )
    expect_s3_class(plot, "ggplot")
})

test_that("hpaVis with a single visType returns the matching hpaVis* output", {
    plot <- suppressMessages(
        hpaVis(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetTissue = "Breast",
            targetCellType = NULL,
            visType = "Tissue"
        )
    )
    expect_s3_class(plot, "ggplot")
})

test_that("hpaVisTissue and hpaVisSubcell render square tiles by default", {
    tissuePlot <- suppressMessages(
        hpaVisTissue(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetTissue = "Breast"
        )
    )
    # Check the public `ratio` argument coord_equal()/coord_fixed() is built
    # from, rather than the internal S3 class name it currently produces -
    # the latter isn't part of ggplot2's documented contract and has already
    # changed across ggplot2 major versions.
    expect_equal(tissuePlot$coordinates$ratio, 1)

    subcellPlot <- suppressMessages(
        hpaVisSubcell(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR")
        )
    )
    expect_equal(subcellPlot$coordinates$ratio, 1)
})

test_that("customTheme=TRUE returns a barebone ggplot without the default theming", {
    plot <- suppressMessages(
        hpaVisTissue(
            data = hpa_histology_data,
            targetGene = c("TP53", "EGFR"),
            targetTissue = "Breast",
            customTheme = TRUE
        )
    )
    expect_s3_class(plot, "ggplot")
    # the default (non-barebone) theme relabels the y axis to "Tissue / Cell";
    # customTheme=TRUE should leave that relabeling off. (Not asserting the
    # exact automatic label ggplot2 falls back to instead, since whether it's
    # populated eagerly or only at build time is a ggplot2-version-dependent
    # implementation detail, not something this package controls.)
    expect_false(identical(plot$labels$y, "Tissue / Cell"))
})
