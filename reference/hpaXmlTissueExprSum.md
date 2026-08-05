# Extract tissue expression and download images

Extract tissue expression information and url to download images from
imported xml document resulted from
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md).
It is important to note that the data that HPA provides on their website
and through xml files are not one-to-one equivalents.

## Usage

``` r
hpaXmlTissueExprSum(importedXml, downloadImg = FALSE)
```

## Arguments

- importedXml:

  Input an xml document object resulted from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  call.

- downloadImg:

  Logical argument. The function will download all image from the
  extracted urls into the working folder.

## Value

This function return a list consists of a summary string, which is a
very brief description of the protein, and a tibble of 2 columns: tissue
(name of tissue available) and imageUrl (link to download the
perspective image)

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md)

## Examples

``` r
  GCH1xml <- hpaXmlGet('ENSG00000131979')
  hpaXmlTissueExprSum(GCH1xml)
#> $summary
#> [1] "Cytoplasmic and nuclear expression in several tissues, most abundant in basal cells of squamous epithelia."
#> 
#> $img
#> # A tibble: 7 × 2
#>   tissue          imageUrl                                                      
#>   <chr>           <chr>                                                         
#> 1 Liver           https://images.proteinatlas.org/28612/64078_A_8_4_rna_selecte…
#> 2 Colon           https://images.proteinatlas.org/28612/64078_A_7_3_rna_selecte…
#> 3 Kidney          https://images.proteinatlas.org/28612/64078_A_7_5_rna_selecte…
#> 4 Testis          https://images.proteinatlas.org/28612/64078_A_4_6_rna_selecte…
#> 5 Lymph node      https://images.proteinatlas.org/28612/64078_A_8_8_rna_selecte…
#> 6 Cerebral cortex https://images.proteinatlas.org/28612/64078_B_8_5_rna_selecte…
#> 7 Vagina          https://images.proteinatlas.org/28612/64078_B_2_2_rna_selecte…
#> 
```
