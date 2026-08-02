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
    # customTheme=TRUE should leave ggplot2's automatic label untouched
    expect_equal(plot$labels$y, "tissue_cell")
})
