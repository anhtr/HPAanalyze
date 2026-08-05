# 7. Tutorial: Parse an entire HPA xml file into relational tibbles with hpaXmlParse()

``` r

library(HPAanalyze)
library(dplyr)
library(xml2)
```

## The case

The `hpaXml` function family
([`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md))
each extract one specific, hand-picked piece of information from an
imported HPA xml file. That covers the most common use cases, but the
xml files – built from the schema at
<https://www.proteinatlas.org/download/proteinatlas.xsd> – contain much
more: protein structure predictions, RNA expression per cell line and
tissue, single-cell type expression, western blot lanes, and more, with
new sections added to the schema on almost every HPA release.

[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
fills that gap: it generically parses *everything* available in an
imported xml document, based on the schema, into a plain R list of
tibbles – without needing new package code every time HPA adds something
to the schema.

## The solution

### Import an xml file as usual

[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
takes the same kind of input as the other `hpaXml` functions: the
`"xml_document"`/`"xml_node"` object returned by
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
(or, for offline/reproducible work,
[`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md) on a
locally saved copy – see the [“Working with HPA xml files
offline”](https://anhtr.github.io/HPAanalyze/articles/d_HPAanalyze_case_offline_xml.md)
vignette).

``` r

CCNB1xml <- hpaXmlGet('ENSG00000134057')
## or, offline: CCNB1xml <- xml2::read_xml("data/ENSG00000134057.xml")
```

### Parse everything with `hpaXmlParse()`

``` r

CCNB1 <- hpaXmlParse(CCNB1xml)
length(CCNB1)
#> [1] 37

names(CCNB1)
#>  [1] "antibody"                     "antibodyTargetWeights"
#>  [3] "antibodyTargetWeights_weight" "assayImage"
#>  [5] "blotLanes"                    "cellExpression"
#>  [7] "data"                         "data_level"
#>  [9] "data_location"                "entry"
#> [11] "entry_synonym"                "identifier"
#> [13] "identifier_xref"              "image"
#> [15] "image_channel"                "image_imageUrl"
#> [17] "lane"                         "lane_weight"
#> [19] "patient"                      "patient_level"
#> [21] "patient_location"             "proteinClasses"
#> [23] "proteinClasses_proteinClass"  "proteinEvidence"
#> [25] "proteinEvidence_evidence"     "rnaExpression"
#> [27] "sample"                       "snomedParameters"
#> [29] "snomedParameters_snomed"      "subAssay"
#> [31] "tissueCell"                   "tissueCell_cellType"
#> [33] "tissueCell_level"             "tissueCell_location"
#> [35] "tissueExpression"             "tissueExpression_validation"
#> [37] "westernBlot"
```

Every element of the returned list is a tibble – some with a single row
(`entry`, `identifier`, `proteinClasses`), some with over a thousand
(`snomedParameters_snomed`). The exact set of tables present depends on
what that gene’s xml file actually contains; a gene with no western blot
data, for example, simply won’t have a `westernBlot` entry.

``` r

CCNB1$entry
#> # A tibble: 1 x 4
#>   version url                                         entry_id name
#>   <chr>   <chr>                                          <int> <chr>
#> 1 18      http://v18.proteinatlas.org/ENSG00000134057        1 CCNB1
```

### How the tables relate to each other

Unlike a straight translation of the xml tree into nested R lists
(which, for this schema, would mean digging 6-7 levels deep to reach a
single value),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
normalizes everything into a **flat, single-level list of tibbles** –
similar to a small relational database extracted from the xml file:

- Every row, except the single top-level `entry` row, carries a foreign
  key column `"<parent>_id"` pointing back at the row (in another
  tibble) it belongs to.
- Tibbles for nested entities (`entry`, `data`, `patient`, `image`,
  `antibody`, …) additionally have a surrogate key column `"<name>_id"`,
  which is what their own children point at. Tibbles for leaf/property
  elements (`tissueCell_level`, `patient_location`, …) are never
  referenced by anything, so they carry the parent’s foreign key only.
- In a leaf/property tibble, the element’s text content sits in a column
  literally named `value`, and its xml attributes keep their own names
  as columns.
- A leaf element that the schema guarantees can never repeat gets no
  tibble of its own: it is merged into its parent’s row, with its text
  content in a column named after the tag (that’s why `CCNB1$entry` has
  a `name` column rather than a separate `entry_name` table) and any
  attributes in columns named `"<tag>_<attribute>"`.

For example, `CCNB1$tissueCell` rows carry a `data_id` that matches the
`data_id` of their parent row in `CCNB1$data`, and
`CCNB1$tissueCell_level` rows carry a `tissueCell_id` pointing back at
`CCNB1$tissueCell`:

``` r

CCNB1$data %>% select(data_id, tissueExpression_id, tissue) %>% head(3)
#> # A tibble: 3 x 3
#>   data_id tissueExpression_id tissue
#>     <int>                <int> <chr>
#> 1       1                    1 adrenal gland
#> 2       2                    1 appendix
#> 3       3                    1 bone marrow

CCNB1$tissueCell %>% head(3)
#> # A tibble: 3 x 3
#>   data_id tissueCell_id quantity
#>     <int>         <int> <chr>
#> 1       1             1 <NA>
#> 2       2             2 <NA>
#> 3       2             3 <NA>

CCNB1$tissueCell_level %>% head(3)
#> # A tibble: 3 x 4
#>   type       value        tissueCell_id count
#>   <chr>      <chr>                <int> <chr>
#> 1 expression not detected             1 <NA>
#> 2 expression medium                   2 <NA>
#> 3 expression low                      3 <NA>
```

Some xml tags mean different things depending on where they appear –
`level` is a staining intensity under `tissueCell` but an RNA abundance
under `data`; `location` appears under both `tissueCell` and `patient`.
To keep tables unambiguous, this kind of leaf/property-style element is
named `"<parent>_<tag>"` (`tissueCell_level`, `patient_location`,
`tissueCell_cellType`, …). Bigger nested entities that represent “the
same kind of record” wherever they occur – `image`, `data`, `patient`,
`antibody`, and so on – keep their own plain tag name and are
disambiguated by their foreign key columns instead (e.g. rows in
`CCNB1$image` that came from an antibody’s western blot carry a
`westernBlot_id`, while rows from a tissue assay carry a
`tissueExpression_id`).

### Worked example: joining tables together

To get every IHC staining level recorded for liver samples, join `data`
(which tissue), `tissueCell` (which cell type), `tissueCell_cellType`,
and `tissueCell_level` (the actual staining/intensity values) on their
id columns:

``` r

cellType <- CCNB1$tissueCell_cellType %>% rename(cellType = value)
level    <- CCNB1$tissueCell_level    %>% rename(level = value)

CCNB1$data %>%
    filter(tissue == "liver", !is.na(tissueExpression_id)) %>%
    inner_join(CCNB1$tissueCell, by = "data_id") %>%
    inner_join(cellType, by = "tissueCell_id") %>%
    inner_join(level, by = "tissueCell_id") %>%
    select(tissueExpression_id, tissue, cellType, level_type = type, level)

#> # A tibble: 6 x 5
#>   tissueExpression_id tissue cellType        level_type level
#>                  <int> <chr>  <chr>           <chr>      <chr>
#> 1                    1 liver  bile duct cells expression not detected
#> 2                    1 liver  hepatocytes     expression not detected
#> 3                    2 liver  bile duct cells staining   not detected
#> 4                    2 liver  bile duct cells intensity  Negative
#> 5                    2 liver  hepatocytes     staining   not detected
#> 6                    2 liver  hepatocytes     intensity  Negative
```

`tissueExpression_id` 1 is the gene-level tissue assay (`expression`,
from HPA’s “annotated protein expression”); `tissueExpression_id` 2 is
the antibody-level assay (`staining`/`intensity`, one row per IHC score
type). Join in `CCNB1$tissueExpression` on `tissueExpression_id` to see
which antibody and technology each row came from.

### Why this shape, and what to expect across genes

[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
decides how to shape each part of the tree purely from the xml schema
itself (is this tag’s type allowed to contain child elements; is this
tag allowed, per the schema, to occur more than once), never from how
many times something happens to occur in one particular gene’s file.
That means:

- The same tag is always classified the same way – an `<identifier>`
  with no `<xref>` children still gets its own `identifier` tibble,
  exactly like a gene whose identifier does have several; a
  `<tissueCell>` with one `<level>` still gets a `tissueCell_level`
  tibble, just like one with four.
- A future HPA schema release that adds an entirely new section (say, a
  new `proteinstructure` or `cellTypeExpression` branch) is picked up
  automatically, as its own new tibble(s), with no changes needed to
  this package.
- An element that’s simply absent for a given gene (no western blot data
  for this antibody, no RNA cell-line data for this gene) means that
  table is absent from the returned list – not present with zero rows.
  Guard accordingly, e.g. `if (!is.null(CCNB1$westernBlot)) ...` or
  `length(CCNB1$westernBlot) > 0`.

### Save your parsed objects

As with the other `hpaXml` functions, the output of
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md)
is a plain R list of tibbles and can be saved for offline use and
reproducibility.

``` r

saveRDS(CCNB1, "data/CCNB1_parsed_relational.rds")
```

## Copyright

**Anh Tran, 2018-2026**

Please cite: **Tran, A.N., Dussaq, A.M., Kennell, T. et al. HPAanalyze:
an R package that facilitates the retrieval and analysis of the Human
Protein Atlas data. BMC Bioinformatics 20, 463 (2019)
<https://doi.org/10.1186/s12859-019-3059-z>**
