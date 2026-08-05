# HPA histology dataset

Dataset downloaded with `hpaDownload('histology', version = 'latest')`.
This should be the most updated dataset at the time of generation. Check
metadata for more information.

## Usage

``` r
hpa_histology_data
```

## Format

A list of 3 tibbles

- normal_tissue:

  Normal tissue IHC data

- pathology:

  Cancer IHC data

- subcellular_location:

  Subcellular location IF data

## See also

[`hpaDownload`](https://anhtr.github.io/HPAanalyze/reference/hpaDownload.md)

## Examples

``` r
  # load data
  data("hpa_histology_data")

  # access data frames
  normal_tissue_data <- hpa_histology_data$normal_tissue
  cancer_data <- hpa_histology_data$pathology
  subcell_location_data <- hpa_histology_data$subcellular_location
  
  # see metadata
  hpa_histology_data$metadata
#> $HPAversion
#> [1] "25.1"
#> 
#> $Ensemblversion
#> [1] "109"
#> 
#> $date
#> [1] "2026-08-05 19:21:13 UTC"
#> 
```
