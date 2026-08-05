# Download datasets

Download the latest version of HPA datasets and import them in R. It is
recommended to only download the datasets you need, as some of them may
be very big.

## Usage

``` r
hpaDownload(downloadList = "histology", version = "latest")
```

## Arguments

- downloadList:

  A vector or string indicate which datasets to download. Common values:

  - `'normal_tissue'`

  - `'pathology'`

  - `'subcellular_location'`

  For the full list of possible values for a specific version, set
  downloadList as NULL:
  `hpaDownload(downloadList = NULL, version = <version>)`

  You can also use the following shortcuts:

  - `'all'`: download everything (not recommended!!!)

  - `'histology'`: same as
    `c('normal_tissue', 'pathology', 'subcellular_location')`

  See <https://www.proteinatlas.org/about/download> for more
  information.

- version:

  A string indicate which version to be downloaded. Possible value:

  - `'latest'`: Download latest version. Due to the constantly changing
    nature of the API, this will download the latest version known to
    work with this package. Require Internet connection. This is the
    default option.

  - `'example'` or `'built-in'`: Load the built-in histology dataset
    from 'HPAanalyze' ('hpa_histology_data'). Do not require internet
    connection.

  - `'v23'`: version 23

  - `'v24'`: version 24

## Value

This function will return a list of tibbles corresponding to requested
datasets.

## See also

`hpaDownload`
[`hpa_histology_data`](https://anhtr.github.io/HPAanalyze/reference/hpa_histology_data.md)

Other downloadable datasets functions:
[`hpaExport()`](https://anhtr.github.io/HPAanalyze/reference/hpaExport.md),
[`hpaSubset()`](https://anhtr.github.io/HPAanalyze/reference/hpaListParam.md)

## Examples

``` r
  histologyData <- hpaDownload(downloadList='histology', version='example')
#> Only the followings are example/built-in datasets: 
#>  - Normal tissue 
#>  - Pathology 
#>  - Subcellular location 
#> Other datasets will not be loaded
  # tissueTranscriptData <- hpaDownload('RNA transcript tissue')
```
