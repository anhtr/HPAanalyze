# 4. Tutorial: Working with Human Protein Atlas (HPA) xml files offline

``` r

library(HPAanalyze)
library(dplyr)
library(xml2)
```

## The case

The Human Protein Atlas allow you to download very detailed data for
each protein in the form of an xml file, and `hpaXmlGet` and `hpaXml`
allow you to retrieve those files automatically from HPA server and
parse them. However, due to technical limitation, you will not be able
to save those `"xml_document"/"xml_node"` objects. The question is: How
do you keep a version of these files to use when you are not connected
to the internet, or for reproducibility?

## The solution

### Download and keep a local version of the xml files for yourself

Look at the [“Downloadable
data”](https://www.proteinatlas.org/about/download) page from HPA
website, you will see how these files are downloaded. Basically, you add
`[ensembl_id].xml` to `http://www.proteinatlas.org` to download
individual entries (that’s what `hpaXmlGet` does behind the scene), or
download the [whole big
set](https://www.proteinatlas.org/download/proteinatlas.xml.gz).

From there, you can import the file using
[`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md). The
output should be exactly the same as `hpaXmlGet`.

``` r

## same as hpaXmlGet("ENSG00000134057")
CCNB1xml <- xml2::read_xml("data/ENSG00000134057.xml")
```

### Business as usual with `hpaXml` functions

Since the umbrella function `hpaXml` take either the *ensembl id* or the
imported `xml_document` object, you can feed what you just imported to
it and get the expected result.

``` r

CCNB1_parsed <- hpaXml(CCNB1xml)
```

You can obviously use other `hpaXml` functions as well.

``` r

hpaXmlProtClass(CCNB1xml)
hpaXmlTissueExprSum(CCNB1xml)
hpaXmlAntibody(CCNB1xml)
hpaXmlTissueExpr(CCNB1xml)
```

If you want everything the xml file contains rather than one specific
piece of information, use
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
instead – see the [“Parse an entire HPA xml file into relational
tibbles”](https://anhtr.github.io/HPAanalyze/articles/g_HPAanalyze_case_relational_xml.md)
vignette.

``` r

CCNB1_all <- hpaXmlParse(CCNB1xml)
```

### Save your parsed objects

It is recommended that you save your parsed objects for reproducibility.
Unlike the `xml_document` object, these parsed objects are just regular
R lists of standard vectors or data frames. You can save them just as
usual.

``` r

saveRDS(CCNB1_parsed, "data/CCNB1_parsed.rds")
```

## Copyright

**Anh Tran, 2018-2025**

Please cite: **Tran, A.N., Dussaq, A.M., Kennell, T. et al. HPAanalyze:
an R package that facilitates the retrieval and analysis of the Human
Protein Atlas data. BMC Bioinformatics 20, 463 (2019)
<https://doi.org/10.1186/s12859-019-3059-z>**
