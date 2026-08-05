# 8. Tutorial: Search the Human Protein Atlas from R with hpaSearch()

``` r

library(HPAanalyze)
library(dplyr)
```

## The case

The Human Protein Atlas website lets you
[search](https://www.proteinatlas.org/search) by a simple gene name or
build an advanced field query (protein class, expression pattern,
prognostic association, and so on), then download the matching rows as a
tsv file – that manual, browser-based workflow is covered in the
[“Combine HPAanalyze with your HPA
queries”](https://anhtr.github.io/HPAanalyze/articles/c_HPAanalyze_case_query.md)
vignette.
[`hpaSearch()`](https://anhtr.github.io/HPAanalyze/reference/hpaSearch.md)
automates that whole round-trip from R: it builds the query url,
downloads the tsv, and returns it as a tibble, without ever leaving R or
opening a browser.

## The solution

### A simple gene search

By default,
[`hpaSearch()`](https://anhtr.github.io/HPAanalyze/reference/hpaSearch.md)
retrieves the gene name (`g`), gene synonyms (`gs`) and Ensembl id
(`eg`) columns.

``` r

hpaSearch(search = "TP53")

#> # A tibble: 1 x 3
#>   Gene  `Gene synonym` Ensembl
#>   <chr> <chr>          <chr>
#> 1 TP53  LFS1, p53      ENSG00000141510
```

The HPA search itself always matches partial and related terms, so a
broader term returns more than one row – searching `"TP53"` also picks
up `"TP53BP1"`, `"TP53BP2"`, `"TP53INP1"`, and dozens of other related
genes:

``` r

nrow(hpaSearch(search = "TP53"))
#> [1] 45
```

### Exact matches

If you’re searching for one specific gene symbol and only want that
gene, set `exact = TRUE`. This filters the result, after retrieval, down
to rows whose `Gene` column is an exact match for `search`. Since the
filtering happens on the `Gene` column, `columns` must include `"g"`
(the default already does).

``` r

hpaSearch(search = "TP53", exact = TRUE)

#> # A tibble: 1 x 3
#>   Gene  `Gene synonym` Ensembl
#>   <chr> <chr>          <chr>
#> 1 TP53  LFS1, p53      ENSG00000141510
```

`exact` only makes sense for a single gene symbol lookup like this one –
it is not meaningful when `search` is an advanced field query (see
below), since those don’t return results keyed on a single gene symbol
match.

### Choosing result columns

`columns` accepts any of the column codes listed at
<https://www.proteinatlas.org/about/help/dataaccess> (protein class,
RNA/protein expression summaries, subcellular location, prognostic
p-values, and many more), as either a character vector or a
comma-separated string – the two calls below are equivalent.

``` r

hpaSearch(search = "TP53", columns = c("g", "eg", "pc", "subloc"))
hpaSearch(search = "TP53", columns = "g,eg,pc,subloc")
```

### Advanced field queries

`search` also accepts the same advanced field query syntax used by the
website’s “Fields \>\>” query builder, so a query you build by clicking
through the website can be reproduced entirely in code. This is the
exact same search used in the [“Combine HPAanalyze with your HPA
queries”](https://anhtr.github.io/HPAanalyze/articles/c_HPAanalyze_case_query.md)
vignette – CD markers expressed at a low level in the cerebral cortex
with an unfavorable prognostic association in glioma – but obtained with
one function call instead of building the query in a browser and
downloading a tsv file by hand.

``` r

cd_markers <- hpaSearch(
  search = "protein_class:CD markers AND normal_expression:Cerebral cortex;Any;Not detected,Low AND prognostic:Glioma;Unfavourable",
  columns = c("g", "gs", "eg")
)
cd_markers

#> # A tibble: 6 x 3
#>   Gene  `Gene synonym`         Ensembl
#>   <chr> <chr>                  <chr>
#> 1 CD81  TAPA-1, TAPA1, TSPAN28 ENSG00000110651
#> 2 NRP1  CD304, NRP1            ENSG00000099250
#> 3 PRNP  ASCR, CD230, PRIP      ENSG00000171867
#> 4 SDC1  CD138, SDC, SYND1      ENSG00000115884
#> 5 THY1  CD90                   ENSG00000154096
#> # ... with 1 more row
```

(See <https://www.proteinatlas.org/about/help/dataaccess> for the full
field/value syntax used to build queries like this one.)

### Feeding results straight into the rest of HPAanalyze

Because
[`hpaSearch()`](https://anhtr.github.io/HPAanalyze/reference/hpaSearch.md)
returns gene names and Ensembl ids in the same tibble, its output plugs
directly into the rest of the package, with no manual tsv download,
unzipping, or reformatting needed.

``` r

## Visualize the search hits with the hpaVis family
latest_datasets <- hpaDownload()

hpaVis(data = latest_datasets,
       targetGene = cd_markers$Gene,
       targetTissue = "cerebral cortex",
       targetCancer = "glioma")

## ...or pull the full xml for each hit
cd_markers_xml <- lapply(cd_markers$Ensembl, hpaXmlGet)
names(cd_markers_xml) <- cd_markers$Gene

cd_markers_protclass <- lapply(cd_markers_xml, hpaXmlProtClass)
```

## Copyright

**Anh Tran, 2018-2026**

Please cite: **Tran, A.N., Dussaq, A.M., Kennell, T. et al. HPAanalyze:
an R package that facilitates the retrieval and analysis of the Human
Protein Atlas data. BMC Bioinformatics 20, 463 (2019)
<https://doi.org/10.1186/s12859-019-3059-z>**
