# Extract tissue expression details

Extract tissue expression information for each sample and url to
download images from imported xml document resulted from
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md).
It is important to note that the data that HPA provides on their website
and through xml files are not one-to-one equivalents. For example, xml
files usually only provide one of the two histology image for each
patient.

## Usage

``` r
hpaXmlTissueExpr(importedXml)
```

## Arguments

- importedXml:

  Input an xml document object resulted from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  call.

## Value

This function returns a list of tibbles, each for an antibody. Each
tibble contains information about all individual samples and their
staining. Due to the variation in amount of information available for
these samples, the number of columns differs, but the tibble essentially
includes: patientId, age, sex, staining, intensity, quantity, location,
imageUrl, snomedCode, and tissueDescription. The last two items may have
more than one column each.

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
# \donttest{
  GCH1xml <- hpaXmlGet('ENSG00000131979')
  hpaXmlTissueExpr(GCH1xml)
#> [[1]]
#> # A tibble: 344 × 20
#>    patientId age   sex    staining intensity quantity location imageUrl         
#>    <chr>     <chr> <chr>  <chr>    <chr>     <chr>    <chr>    <chr>            
#>  1 2259      23    Female NA       NA        NA       NA       https://images.p…
#>  2 2263      77    Female NA       NA        NA       NA       https://images.p…
#>  3 3162      89    Female NA       NA        NA       NA       https://images.p…
#>  4 3500      39    Female NA       NA        NA       NA       https://images.p…
#>  5 3761      59    Male   NA       NA        NA       NA       https://images.p…
#>  6 3910      63    Male   NA       NA        NA       NA       https://images.p…
#>  7 1725      57    Male   NA       NA        NA       NA       https://images.p…
#>  8 1859      61    Male   NA       NA        NA       NA       https://images.p…
#>  9 3238      59    Female NA       NA        NA       NA       https://images.p…
#> 10 1990      14    Male   NA       NA        NA       NA       https://images.p…
#> # ℹ 334 more rows
#> # ℹ 12 more variables: snomedCode1 <chr>, snomedCode2 <chr>, snomedCode3 <chr>,
#> #   snomedCode4 <chr>, snomedCode5 <chr>, snomedCode6 <chr>,
#> #   tissueDescription1 <chr>, tissueDescription2 <chr>,
#> #   tissueDescription3 <chr>, tissueDescription4 <chr>,
#> #   tissueDescription5 <chr>, tissueDescription6 <chr>
#> 
# }
```
