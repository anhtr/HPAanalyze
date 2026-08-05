# Extract antibody information

Extract information about the antibodies used for a specific protein. It
is important to note that the data that HPA provides on their website
and through xml files are not one-to-one equivalents.

## Usage

``` r
hpaXmlAntibody(importedXml)
```

## Arguments

- importedXml:

  Input an xml document object resulted from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  call.

## Value

This function returns a tibble of 4 columns, containing information
about the antibodies used in the project for the inquired protein: id,
releaseDate, releaseVersion, and RRID.

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
  GCH1xml <- hpaXmlGet('ENSG00000131979')
  hpaXmlAntibody(GCH1xml)
#> # A tibble: 1 × 3
#>   id        releaseVersion releaseDate
#>   <chr>     <chr>          <chr>      
#> 1 HPA028612 6.0            2010-03-26 
```
