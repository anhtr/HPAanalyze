# Search the Human Protein Atlas

Query the Human Protein Atlas search API directly from R, equivalent to
searching at <https://www.proteinatlas.org/search> and downloading the
resulting tsv file. See
<https://www.proteinatlas.org/about/help/dataaccess> for the full query
syntax and list of available column codes.

## Usage

``` r
hpaSearch(search, columns = c("g", "gs", "eg"), exact = FALSE)
```

## Arguments

- search:

  A string with the search term(s) or query, using the same syntax as
  <https://www.proteinatlas.org/search>. This can be a simple term (e.g.
  `'TP53'`) or an advanced field query (e.g.
  `'protein_class:CD markers AND normal_expression:Cerebral cortex;Any;Not detected,Low'`).

- columns:

  A vector or comma-separated string of column codes to retrieve, e.g.
  `c('g', 'gs', 'eg')` or `'g,gs,eg'`. See
  <https://www.proteinatlas.org/about/help/dataaccess> for the full list
  of available codes. Defaults to gene name, gene synonym, and Ensembl
  id.

- exact:

  Logical. The HPA search itself always matches on partial and related
  terms (e.g. searching `'TP53'` also returns `'TP53BP1'` and other
  related genes). If `TRUE`, the result is filtered down, after
  retrieval, to rows whose `Gene` column is an exact match for `search`.
  Requires `'g'` (gene name) to be included in `columns`, since that's
  what the filtering is done on. Only meaningful when `search` is a
  single gene symbol, not an advanced field query.

## Value

A tibble with one row per matching protein/gene and one column per
requested field, using the HPA's own column names (e.g. `Gene`,
`Ensembl`).

## See also

[`hpaSubset`](https://anhtr.github.io/HPAanalyze/reference/hpaListParam.md)
[`hpaXmlGet`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)

## Examples

``` r
  hpaSearch(search = 'TP53', columns = c('g', 'gs', 'eg'))
#> # A tibble: 45 × 3
#>    Gene     `Gene synonym`                                               Ensembl
#>    <chr>    <chr>                                                        <chr>  
#>  1 TP53     LFS1, p53                                                    ENSG00…
#>  2 TP53INP1 DKFZp434M1317, FLJ22139, P53DINP1, SIP, Teap, TP53INP1A, TP… ENSG00…
#>  3 TP53TG3  P53TG3, TP53TG3A                                             ENSG00…
#>  4 TP53RK   BUD32, C20orf64, dJ101A2.2, Nori-2p, prpk, TPRKB             ENSG00…
#>  5 TP53TG3B NA                                                           ENSG00…
#>  6 TP53TG3C NA                                                           ENSG00…
#>  7 TP53TG3D NA                                                           ENSG00…
#>  8 TP53TG3E NA                                                           ENSG00…
#>  9 TP53TG3F NA                                                           ENSG00…
#> 10 TP53TG5  C20orf10, CLG01, dJ453C12.5                                  ENSG00…
#> # ℹ 35 more rows
  hpaSearch(search = 'TP53', columns = c('g', 'gs', 'eg'), exact = TRUE)
#> # A tibble: 1 × 3
#>   Gene  `Gene synonym` Ensembl        
#>   <chr> <chr>          <chr>          
#> 1 TP53  LFS1, p53      ENSG00000141510
```
