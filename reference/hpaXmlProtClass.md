# Extract protein classes

Extract protein class information from imported xml document resulted
from
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md).
It is important to note that the data that HPA provides on their website
and through xml files are not one-to-one equivalents.

## Usage

``` r
hpaXmlProtClass(importedXml)
```

## Arguments

- importedXml:

  Input an xml document object resulted from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  call.

## Value

This function return a tibble of 4 columns.

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
  GCH1xml <- hpaXmlGet('ENSG00000131979')
  hpaXmlProtClass(GCH1xml)
#> # A tibble: 17 × 4
#>    source          id    parent_id name                                         
#>    <chr>           <chr> <chr>     <chr>                                        
#>  1 NA              Ez    NA        Enzymes                                      
#>  2 ENZYME          Ec    Ez        ENZYME proteins                              
#>  3 ENZYME          Eh    Ec        Hydrolases                                   
#>  4 Metabolic Atlas Mp    NA        Metabolic proteins                           
#>  5 HPA             Za    NA        Predicted intracellular proteins             
#>  6 HPA             Zm    Za        Intracellular proteins predicted by MDM and …
#>  7 UniProt         Dr    NA        Disease related genes                        
#>  8 HPA             Pd    NA        Potential drug targets                       
#>  9 KEGG            Ha    NA        Human disease related genes                  
#> 10 KEGG            Hd    Ha        Congenital disorders of metabolism           
#> 11 KEGG            Hd4   Hd        Congenital disorders of amino acid metabolism
#> 12 KEGG            Hk    Ha        Nervous system diseases                      
#> 13 KEGG            Hk5   Hk        Other nervous and sensory system diseases    
#> 14 UniProt         Ua    NA        UniProt - Evidence at protein level          
#> 15 neXtProt        Nx    NA        Mapped to neXtProt                           
#> 16 neXtProt        Na    Nx        neXtProt - Evidence at protein level         
#> 17 Kim et al 2014  Ea    NA        Protein evidence (Kim et al 2014)            
```
