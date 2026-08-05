# Extract details about an individual protein from XML file in one function

This function is the umbrella function for the hpaXml function family.
It take the input of either one Ensembl gene id or a imported XML object
resulting from a
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
function call. By default, it will extract all information available for
HPAanalyze user from the XML file by calling every hpaXml function and
put all results into a list.

## Usage

``` r
hpaXml(
  inputXml,
  extractType = c("ProtClass", "TissueExprSum", "Antibody", "TissueExpr"),
  ...
)
```

## Arguments

- inputXml:

  Input can be either one Ensembl gene id (start with ENSG) or a
  imported XML object resulting from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  function call. You can also use HGNC gene symbol and it will be
  converted to ensembl id.

- extractType:

  A vector of strings indicate which information is desired for
  extraction. By default this function will call all `hpaXml` functions
  available. Other options are `'ProtClass'`, `'TissueExprSum'`,
  `'Antibody'`, `'TissueExpr'`.

- ...:

  Additional arguments to be passed downstream to other hpaXml functions
  being called behind the scene. See help files of other hpaXml
  functions for more information.

## Value

This function returns a list. Each element of the list is information
extracted from the XML file specified using other hpaXml functions. See
help file for each XML function for more information.

## See also

Other xml functions:
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlParse()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlParse.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
 
  hpaXml(inputXml='ENSG00000131979', extractType=c('ProtClass', 'TissueExprSum', 'Antibody'))
#> $ProtClass
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
#> 
#> $TissueExprSum
#> $TissueExprSum$summary
#> [1] "Cytoplasmic and nuclear expression in several tissues, most abundant in basal cells of squamous epithelia."
#> 
#> $TissueExprSum$img
#> # A tibble: 7 × 2
#>   tissue          imageUrl                                                      
#>   <chr>           <chr>                                                         
#> 1 Liver           https://images.proteinatlas.org/28612/64078_A_8_4_rna_selecte…
#> 2 Colon           https://images.proteinatlas.org/28612/64078_A_7_3_rna_selecte…
#> 3 Kidney          https://images.proteinatlas.org/28612/64078_A_7_5_rna_selecte…
#> 4 Testis          https://images.proteinatlas.org/28612/64078_A_4_6_rna_selecte…
#> 5 Lymph node      https://images.proteinatlas.org/28612/64078_A_8_8_rna_selecte…
#> 6 Cerebral cortex https://images.proteinatlas.org/28612/64078_B_8_5_rna_selecte…
#> 7 Vagina          https://images.proteinatlas.org/28612/64078_B_2_2_rna_selecte…
#> 
#> 
#> $Antibody
#> # A tibble: 1 × 3
#>   id        releaseVersion releaseDate
#>   <chr>     <chr>          <chr>      
#> 1 HPA028612 6.0            2010-03-26 
#> 
```
