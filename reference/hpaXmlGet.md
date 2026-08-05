# Download and import xml file

Download and import individual xml file for a specified protein. This
function calls
[`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md) under
the hood. It is important to note that the data that HPA provides on
their website and through xml files are not one-to-one equivalents.

## Usage

``` r
hpaXmlGet(targetEnsemblId, version = "latest")
```

## Arguments

- targetEnsemblId:

  A string of one ensembl ID, start with ENSG. For example
  `'ENSG00000131979'`. You can also use HGNC gene symbol and it will be
  converted to ensembl id.

- version:

  A string indicate which version to be downloaded. Possible value:

  - `'latest'`: Download latest version.

  - `'v?'` with '?' is a integer: Download a specific version of the
    dataset. For example: 'v18' download version 18. Currently support
    version 13 and above.

## Value

This function return an object of class `"xml_document" "xml_node"`
containing the content of the imported XML file. (See documentations for
package `xml2` for more information.)

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
  GCH1xml <- hpaXmlGet('ENSG00000131979')
```
