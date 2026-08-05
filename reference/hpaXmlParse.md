# Parse an entire imported xml document into a list of tibbles

Parse everything available in an imported xml document resulted from
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
based on the Human Protein Atlas xml schema
(<https://www.proteinatlas.org/download/proteinatlas.xsd>). Unlike the
other `hpaXml` functions, which each extract one specific piece of
information, `hpaXmlParse()` generically walks the entire xml tree and
normalizes it into a flat, single-level named list of tibbles, similar
to a small relational database extracted from the xml file.

## Usage

``` r
hpaXmlParse(importedXml)
```

## Arguments

- importedXml:

  Input an xml document object resulted from a
  [`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md)
  call.

## Value

This function returns a flat, named list of tibbles. See Details for how
the tibbles are named and joined to each other.

## Details

Every row, except the single top-level `result$entry` row, carries a
foreign key column named `"<parent>_id"` pointing back at the row (in
another tibble) it belongs to. For example, rows in `result$tissueCell`
carry a `data_id` that matches the `data_id` of their parent row in
`result$data`, and `result$tissueCell_level` rows carry a
`tissueCell_id` pointing back at `result$tissueCell`.

Two kinds of tibble are produced. Tibbles for nested entities (`entry`,
`data`, `patient`, `image`, `antibody`, ...) keep their plain tag name,
since they represent the same kind of record wherever they occur, and
additionally have a surrogate key column named `"<name>_id"` that their
own children point at. Tibbles for leaf/property-style elements are
instead named `"<parent>_<tag>"` (e.g. `tissueCell_level`,
`patient_location`), because some xml tags are reused with different
meaning under different parents (for example `level` means staining
intensity under `tissueCell` but RNA abundance under `data`); nothing
refers back to their rows, so they carry the parent's foreign key only
and have no surrogate key of their own.

In a leaf/property tibble, the element's own text content is held in a
column literally named `"value"` and its xml attributes keep their own
names as columns, so e.g. `result$tissueCell_cellType$value` holds the
cell type name. A leaf element that the schema guarantees can never
repeat gets no tibble at all and is merged into its parent's row
instead: its text content becomes a column named after the tag (e.g.
`result$entry$name`, `result$data$tissue`) and any attributes become
columns named `"<tag>_<attribute>"`.

Because the function is driven entirely by the xml structure itself
(whether an xml tag is, per the schema, allowed to occur more than once)
rather than by hand-written per-element extraction code, its output
shape does not depend on which gene was parsed, gracefully tolerates xml
files that are missing elements the schema allows, and requires no code
changes to support new elements introduced by future HPA schema
revisions. `hpaXmlParse()` only includes tibbles for elements that are
actually present in the imported document; an element absent for a given
gene (for example an antibody with no western blot data) simply has no
corresponding entry in the returned list.

## See also

Other xml functions:
[`hpaXml()`](https://anhtr.github.io/HPAanalyze/reference/hpaXml.md),
[`hpaXmlAntibody()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlAntibody.md),
[`hpaXmlGet()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlGet.md),
[`hpaXmlProtClass()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlProtClass.md),
[`hpaXmlTissueExpr()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExpr.md),
[`hpaXmlTissueExprSum()`](https://anhtr.github.io/HPAanalyze/reference/hpaXmlTissueExprSum.md)

## Examples

``` r
  GCH1xml <- hpaXmlGet('ENSG00000131979')
  hpaXmlParse(GCH1xml)
#> $antibody
#> # A tibble: 5 × 12
#>   name   start_position end_position identity_percent chain_id antibody_id id   
#>   <chr>  <chr>          <chr>        <chr>               <int>       <int> <chr>
#> 1 HPA02… 65             183          100                     1           1 NA   
#> 2 HPA02… 65             183          100                     2           2 NA   
#> 3 HPA02… 65             183          100                     3           3 NA   
#> 4 HPA02… 65             183          100                     4           4 NA   
#> 5 NA     NA             NA           NA                     NA           5 HPA0…
#> # ℹ 5 more variables: releaseVersion <chr>, releaseDate <chr>, entry_id <int>,
#> #   antigenSequence_source <chr>, antigenSequence <chr>
#> 
#> $antibodyTargetWeights
#> # A tibble: 1 × 3
#>   source antibody_id antibodyTargetWeights_id
#>   <chr>        <int>                    <int>
#> 1 HPA              5                        1
#> 
#> $antibodyTargetWeights_weight
#> # A tibble: 3 × 3
#>   value unit  antibodyTargetWeights_id
#>   <chr> <chr>                    <int>
#> 1 27.9  kDa                          1
#> 2 25.8  kDa                          1
#> 3 23.5  kDa                          1
#> 
#> $assayImage
#> # A tibble: 525 × 3
#>    sample_id assayImage_id data_id
#>        <int>         <int>   <int>
#>  1         1             1      NA
#>  2         2             2      NA
#>  3         3             3      NA
#>  4         4             4      NA
#>  5         5             5      NA
#>  6         6             6      NA
#>  7         7             7      NA
#>  8         8             8      NA
#>  9         9             9      NA
#> 10        10            10      NA
#> # ℹ 515 more rows
#> 
#> $blotLanes
#> # A tibble: 1 × 2
#>   westernBlot_id blotLanes_id
#>            <int>        <int>
#> 1              1            1
#> 
#> $cancerExpression
#> # A tibble: 1 × 7
#>   source technology assayType entry_id cancerExpression_id rnaCancerSpecificity 
#>   <chr>  <chr>      <chr>        <int>               <int> <chr>                
#> 1 HPA    RNA        cancer           1                   1 Low cancer specifici…
#> # ℹ 1 more variable: rnaCancerDistribution <chr>
#> 
#> $cellExpression
#> # A tibble: 2 × 8
#>   source technology entry_id cellExpression_id summary         verification_type
#>   <chr>  <chr>         <int>             <int> <chr>           <chr>            
#> 1 HPA    ICC/IF            1                 1 Mainly localiz… reliability      
#> 2 HPA    ICC/IF           NA                 2 NA              NA               
#> # ℹ 2 more variables: verification <chr>, antibody_id <int>
#> 
#> $cellTypeExpression
#> # A tibble: 1 × 7
#>   technology assayType entry_id cellTypeExpression_id cellTypeDistribution
#>   <chr>      <chr>        <int>                 <int> <chr>               
#> 1 scRNAseq   cell             1                     1 Detected in many    
#> # ℹ 2 more variables: cellTypeExpressionCluster_clusterID <chr>,
#> #   cellTypeExpressionCluster <chr>
#> 
#> $cellTypeExpression_singleCellTypeExpression
#> # A tibble: 154 × 5
#>    name                     type            unitRNA expRNA cellTypeExpression_id
#>    <chr>                    <chr>           <chr>   <chr>                  <int>
#>  1 Brain excitatory neurons normalizedRNAE… nCPM    6.7                        1
#>  2 Brain inhibitory neurons normalizedRNAE… nCPM    11.9                       1
#>  3 Retinal amacrine cells   normalizedRNAE… nCPM    3.5                        1
#>  4 Retinal horizontal cells normalizedRNAE… nCPM    2.0                        1
#>  5 Retinal ganglion cells   normalizedRNAE… nCPM    3.9                        1
#>  6 Retinal bipolar cells    normalizedRNAE… nCPM    2.1                        1
#>  7 Rod photoreceptor cells  normalizedRNAE… nCPM    9.5                        1
#>  8 Cone photoreceptor cells normalizedRNAE… nCPM    35.2                       1
#>  9 Other brain neurons      normalizedRNAE… nCPM    13.7                       1
#> 10 Astrocytes               normalizedRNAE… nCPM    1.3                        1
#> # ℹ 144 more rows
#> 
#> $cellTypeSpecificity
#> # A tibble: 1 × 3
#>   category           cellTypeExpression_id cellTypeSpecificity_id
#>   <chr>                              <int>                  <int>
#> 1 Cell type enhanced                     1                      1
#> 
#> $cellTypeSpecificity_cellType
#> # A tibble: 4 × 2
#>   value                  cellTypeSpecificity_id
#>   <chr>                                   <int>
#> 1 monocytes                                   1
#> 2 Neuroendocrine cells                        1
#> 3 Neutrophils                                 1
#> 4 Pancreatic islet cells                      1
#> 
#> $chain
#> # A tibble: 4 × 7
#>   chain length gene  ensembl_peptide_id ensembl_transcript_id structure_id
#>   <chr> <chr>  <chr> <chr>              <chr>                        <int>
#> 1 A     250    GCH1  ENSP00000378890    ENST00000395514                  1
#> 2 A     250    GCH1  ENSP00000419045    ENST00000491895                  2
#> 3 A     233    GCH1  ENSP00000444011    ENST00000543643                  3
#> 4 A     213    GCH1  ENSP00000445246    ENST00000536224                  4
#> # ℹ 1 more variable: chain_id <int>
#> 
#> $data
#> # A tibble: 540 × 26
#>    tissueExpression_id data_id tissue_organ          tissue_ontologyTerms tissue
#>                  <int>   <int> <chr>                 <chr>                <chr> 
#>  1                   1       1 Connective & soft ti… UBERON:0001013       Adipo…
#>  2                   1       2 Endocrine tissues     UBERON:0002369       Adren…
#>  3                   1       3 Bone marrow & lympho… UBERON:0013689       Appen…
#>  4                   1       4 Bone marrow & lympho… UBERON:0002371       Bone …
#>  5                   1       5 Female tissues        UBERON:0000310,UBER… Breast
#>  6                   1       6 Respiratory system    UBERON:0002185       Bronc…
#>  7                   1       7 Brain                 UBERON:0001873       Cauda…
#>  8                   1       8 Brain                 UBERON:0002037       Cereb…
#>  9                   1       9 Brain                 UBERON:0000956       Cereb…
#> 10                   1      10 Female tissues        UBERON:0000002       Cervix
#> # ℹ 530 more rows
#> # ℹ 21 more variables: cancerExpression_id <int>,
#> #   survivalAnalysis_prognosticType <chr>, survivalAnalysis_isPrognostic <chr>,
#> #   survivalAnalysis_prognostic <chr>, survivalAnalysis_pValue <chr>,
#> #   survivalAnalysis_source <chr>, survivalAnalysis_dataSource <chr>,
#> #   cellExpression_id <int>, rnaExpression_id <int>, RNASample_sampleId <chr>,
#> #   RNASample_unitRNA <chr>, RNASample_expRNA <chr>, RNASample_sex <chr>, …
#> 
#> $data_RNASample
#> # A tibble: 1,436 × 6
#>    sampleId unitRNA expRNA sex    age   data_id
#>    <chr>    <chr>   <chr>  <chr>  <chr>   <int>
#>  1 86       nTPM    0      Female 80        133
#>  2 115      nTPM    0      Female 45        133
#>  3 137      nTPM    0.4    Female 57        133
#>  4 329      nTPM    1      Female 74        133
#>  5 331      nTPM    1.5    Female 59        133
#>  6 87       nTPM    1.2    Female 62        134
#>  7 88       nTPM    5.7    Female 36        134
#>  8 89       nTPM    9.9    Female 63        134
#>  9 92       nTPM    9.7    Male   26        135
#> 10 93       nTPM    1.5    Female 37        135
#> # ℹ 1,426 more rows
#> 
#> $data_level
#> # A tibble: 1,210 × 5
#>    type       value        data_id unitRNA expRNA
#>    <chr>      <chr>          <int> <chr>   <chr> 
#>  1 expression not detected       1 NA      NA    
#>  2 expression medium             2 NA      NA    
#>  3 expression low                3 NA      NA    
#>  4 expression low                4 NA      NA    
#>  5 expression low                5 NA      NA    
#>  6 expression low                6 NA      NA    
#>  7 expression not detected       7 NA      NA    
#>  8 expression not detected       8 NA      NA    
#>  9 expression not detected       9 NA      NA    
#> 10 expression medium            10 NA      NA    
#> # ℹ 1,200 more rows
#> 
#> $data_location
#> # A tibble: 9 × 5
#>   status     GOId       value            data_id singleCellVariationIntensity
#>   <chr>      <chr>      <chr>              <int> <chr>                       
#> 1 additional GO:0005829 cytosol               81 NA                          
#> 2 additional GO:0031965 nuclear membrane      81 NA                          
#> 3 main       GO:0005654 nucleoplasm           81 NA                          
#> 4 NA         GO:0005654 nucleoplasm          538 NA                          
#> 5 NA         GO:0005654 nucleoplasm          539 NA                          
#> 6 NA         GO:0005829 cytosol              539 NA                          
#> 7 NA         GO:0005654 nucleoplasm          540 true                        
#> 8 NA         GO:0031965 nuclear membrane     540 NA                          
#> 9 NA         GO:0005829 cytosol              540 NA                          
#> 
#> $entry
#> # A tibble: 1 × 5
#>   version url                                   entry_id name  predictedLocation
#>   <chr>   <chr>                                    <int> <chr> <chr>            
#> 1 25      https://v25.proteinatlas.org/ENSG000…        1 GCH1  Intracellular    
#> 
#> $entry_synonym
#> # A tibble: 5 × 2
#>   value  entry_id
#>   <chr>     <int>
#> 1 DYT14         1
#> 2 DYT5          1
#> 3 DYT5a         1
#> 4 GCH           1
#> 5 GTPCH1        1
#> 
#> $identifier
#> # A tibble: 1 × 7
#>   id              db      version assembly gencodeVersion entry_id identifier_id
#>   <chr>           <chr>   <chr>   <chr>    <chr>             <int>         <int>
#> 1 ENSG00000131979 Ensembl 109     GRCh38.… 43                    1             1
#> 
#> $identifier_xref
#> # A tibble: 2 × 3
#>   id     db                identifier_id
#>   <chr>  <chr>                     <int>
#> 1 P30793 Uniprot/SWISSPROT             1
#> 2 2643   NCBI GeneID                   1
#> 
#> $image
#> # A tibble: 540 × 13
#>    imageType   tissueExpression_id image_id tissue_organ    tissue_ontologyTerms
#>    <chr>                     <int>    <int> <chr>           <chr>               
#>  1 selected                      1        1 Liver & Gallbl… UBERON:0002107      
#>  2 selected                      1        2 Gastrointestin… UBERON:0001155      
#>  3 selected                      1        3 Kidney & Urina… UBERON:0002113      
#>  4 selected                      1        4 Male tissues    UBERON:0000473      
#>  5 selected                      1        5 Bone marrow & … UBERON:0000029      
#>  6 selected                      1        6 Brain           UBERON:0000956      
#>  7 selected                      1        7 Female tissues  UBERON:0000996      
#>  8 selected                     NA        8 NA              NA                  
#>  9 selected                      2        9 NA              NA                  
#> 10 sampleImage                  NA       10 NA              NA                  
#> # ℹ 530 more rows
#> # ℹ 8 more variables: tissue <chr>, cellExpression_id <int>, description <chr>,
#> #   assayImage_id <int>, subAssay_id <int>, scale <chr>, westernBlot_id <int>,
#> #   proteinArray_id <int>
#> 
#> $image_channel
#> # A tibble: 24 × 3
#>    color  value          image_id
#>    <chr>  <chr>             <int>
#>  1 blue   Nucleus             533
#>  2 red    Microtubules        533
#>  3 green  Target protein      533
#>  4 yellow ER                  533
#>  5 blue   Nucleus             534
#>  6 red    Microtubules        534
#>  7 green  Target protein      534
#>  8 yellow ER                  534
#>  9 blue   Nucleus             535
#> 10 red    Microtubules        535
#> # ℹ 14 more rows
#> 
#> $image_imageUrl
#> # A tibble: 540 × 2
#>    value                                                              image_id
#>    <chr>                                                                 <int>
#>  1 https://images.proteinatlas.org/28612/64078_A_8_4_rna_selected.jpg        1
#>  2 https://images.proteinatlas.org/28612/64078_A_7_3_rna_selected.jpg        2
#>  3 https://images.proteinatlas.org/28612/64078_A_7_5_rna_selected.jpg        3
#>  4 https://images.proteinatlas.org/28612/64078_A_4_6_rna_selected.jpg        4
#>  5 https://images.proteinatlas.org/28612/64078_A_8_8_rna_selected.jpg        5
#>  6 https://images.proteinatlas.org/28612/64078_B_8_5_rna_selected.jpg        6
#>  7 https://images.proteinatlas.org/28612/64078_B_2_2_rna_selected.jpg        7
#>  8 https://images.proteinatlas.org/28612/256_C7_2_selected.jpg               8
#>  9 https://images.proteinatlas.org/28612/ihc_selected.jpg                    9
#> 10 https://images.proteinatlas.org/28612/64078_B_3_8.jpg                    10
#> # ℹ 530 more rows
#> 
#> $image_imageUrlTif
#> # A tibble: 516 × 2
#>    value                                                 image_id
#>    <chr>                                                    <int>
#>  1 https://images.proteinatlas.org/28612/64078_B_4_5.tif       16
#>  2 https://images.proteinatlas.org/28612/64078_B_5_5.tif       17
#>  3 https://images.proteinatlas.org/28612/64078_B_6_5.tif       18
#>  4 https://images.proteinatlas.org/28612/64078_A_3_2.tif       19
#>  5 https://images.proteinatlas.org/28612/64078_A_1_2.tif       20
#>  6 https://images.proteinatlas.org/28612/64078_A_2_2.tif       21
#>  7 https://images.proteinatlas.org/28612/64078_B_4_4.tif       22
#>  8 https://images.proteinatlas.org/28612/64078_B_6_4.tif       23
#>  9 https://images.proteinatlas.org/28612/64078_B_5_4.tif       24
#> 10 https://images.proteinatlas.org/28612/64078_B_1_4.tif       25
#> # ℹ 506 more rows
#> 
#> $lane
#> # A tibble: 6 × 4
#>   laneId laneContent  blotLanes_id lane_id
#>   <chr>  <chr>               <int>   <int>
#> 1 1      Marker                  1       1
#> 2 2      RT-4                    1       2
#> 3 3      U-251MG                 1       3
#> 4 4      Human Plasma            1       4
#> 5 5      Liver                   1       5
#> 6 6      Tonsil                  1       6
#> 
#> $lane_weight
#> # A tibble: 9 × 3
#>   value unit  lane_id
#>   <chr> <chr>   <int>
#> 1 230   kDa         1
#> 2 130   kDa         1
#> 3 95    kDa         1
#> 4 72    kDa         1
#> 5 56    kDa         1
#> 6 36    kDa         1
#> 7 28    kDa         1
#> 8 17    kDa         1
#> 9 11    kDa         1
#> 
#> $patient
#> # A tibble: 344 × 6
#>    data_id patient_id sex    age   patientId quantity
#>      <int>      <int> <chr>  <chr> <chr>     <chr>   
#>  1     469          1 Female 23    2259      NA      
#>  2     469          2 Female 77    2263      NA      
#>  3     469          3 Female 89    3162      NA      
#>  4     469          4 Female 39    3500      NA      
#>  5     469          5 Male   59    3761      NA      
#>  6     469          6 Male   63    3910      NA      
#>  7     470          7 Male   57    1725      NA      
#>  8     470          8 Male   61    1859      NA      
#>  9     470          9 Female 59    3238      NA      
#> 10     471         10 Male   14    1990      NA      
#> # ℹ 334 more rows
#> 
#> $patient_level
#> # A tibble: 410 × 3
#>    type      value    patient_id
#>    <chr>     <chr>         <int>
#>  1 staining  Medium          140
#>  2 intensity Moderate        140
#>  3 staining  High            141
#>  4 intensity Strong          141
#>  5 staining  High            142
#>  6 intensity Strong          142
#>  7 staining  Medium          143
#>  8 intensity Moderate        143
#>  9 staining  Medium          144
#> 10 intensity Moderate        144
#> # ℹ 400 more rows
#> 
#> $patient_location
#> # A tibble: 205 × 2
#>    value                          patient_id
#>    <chr>                               <int>
#>  1 cytoplasmic/membranous                140
#>  2 cytoplasmic/membranous                141
#>  3 cytoplasmic/membranous                142
#>  4 cytoplasmic/membranous                143
#>  5 cytoplasmic/membranous,nuclear        144
#>  6 cytoplasmic/membranous,nuclear        145
#>  7 none                                  146
#>  8 cytoplasmic/membranous                147
#>  9 cytoplasmic/membranous,nuclear        148
#> 10 cytoplasmic/membranous,nuclear        149
#> # ℹ 195 more rows
#> 
#> $proteinArray
#> # A tibble: 1 × 8
#>   source technology antibody_id proteinArray_id verification_type
#>   <chr>  <chr>            <int>           <int> <chr>            
#> 1 HPA    PA                   5               1 validation       
#> # ℹ 3 more variables: verification_description <chr>, verification <chr>,
#> #   antibodyDilution_dilution <chr>
#> 
#> $proteinClasses
#> # A tibble: 1 × 2
#>   entry_id proteinClasses_id
#>      <int>             <int>
#> 1        1                 1
#> 
#> $proteinClasses_proteinClass
#> # A tibble: 17 × 5
#>    source            id    parent_id name                      proteinClasses_id
#>    <chr>             <chr> <chr>     <chr>                                 <int>
#>  1 ""                Ez    ""        Enzymes                                   1
#>  2 "ENZYME"          Ec    "Ez"      ENZYME proteins                           1
#>  3 "ENZYME"          Eh    "Ec"      Hydrolases                                1
#>  4 "Metabolic Atlas" Mp    ""        Metabolic proteins                        1
#>  5 "HPA"             Za    ""        Predicted intracellular …                 1
#>  6 "HPA"             Zm    "Za"      Intracellular proteins p…                 1
#>  7 "UniProt"         Dr    ""        Disease related genes                     1
#>  8 "HPA"             Pd    ""        Potential drug targets                    1
#>  9 "KEGG"            Ha    ""        Human disease related ge…                 1
#> 10 "KEGG"            Hd    "Ha"      Congenital disorders of …                 1
#> 11 "KEGG"            Hd4   "Hd"      Congenital disorders of …                 1
#> 12 "KEGG"            Hk    "Ha"      Nervous system diseases                   1
#> 13 "KEGG"            Hk5   "Hk"      Other nervous and sensor…                 1
#> 14 "UniProt"         Ua    ""        UniProt - Evidence at pr…                 1
#> 15 "neXtProt"        Nx    ""        Mapped to neXtProt                        1
#> 16 "neXtProt"        Na    "Nx"      neXtProt - Evidence at p…                 1
#> 17 "Kim et al 2014"  Ea    ""        Protein evidence (Kim et…                 1
#> 
#> $proteinEvidence
#> # A tibble: 1 × 3
#>   evidence                  entry_id proteinEvidence_id
#>   <chr>                        <int>              <int>
#> 1 Evidence at protein level        1                  1
#> 
#> $proteinEvidence_evidence
#> # A tibble: 3 × 3
#>   source  evidence                  proteinEvidence_id
#>   <chr>   <chr>                                  <int>
#> 1 HPA     Evidence at protein level                  1
#> 2 MS      Not available                              1
#> 3 UniProt Evidence at protein level                  1
#> 
#> $proteinstructure
#> # A tibble: 1 × 2
#>   entry_id proteinstructure_id
#>      <int>               <int>
#> 1        1                   1
#> 
#> $rnaExpression
#> # A tibble: 11 × 9
#>    source technology assayType  entry_id rnaExpression_id rnaDistribution_desc…¹
#>    <chr>  <chr>      <chr>         <int>            <int> <chr>                 
#>  1 HPA    RNAseq     consensus…        1                1 n>33% and n<all       
#>  2 HPA    RNAseq     tissue            1                2 NA                    
#>  3 HPA    RNAseq     humanBrain        1                3 NA                    
#>  4 HPA    RNAseq     humanBrai…        1                4 n=all                 
#>  5 HPA    RNAseq     mouseBrai…        1                5 n>33% and n<all       
#>  6 HPA    RNAseq     mouseBrain        1                6 NA                    
#>  7 HPA    RNAseq     pigBrainR…        1                7 n=all                 
#>  8 HPA    RNAseq     pigBrain          1                8 NA                    
#>  9 HPA    RNAseq     cellLine          1                9 n>33% and n<all       
#> 10 HPA    RNAseq     immuneCell        1               10 n>33% and n<all       
#> 11 HPA    RNAseq     immuneCel…        1               11 n>33% and n<all       
#> # ℹ abbreviated name: ¹​rnaDistribution_description
#> # ℹ 3 more variables: rnaDistribution <chr>,
#> #   rnaExpressionCluster_clusterID <chr>, rnaExpressionCluster <chr>
#> 
#> $rnaSpecificity
#> # A tibble: 7 × 6
#>   description  specificity rnaExpression_id rnaSpecificity_id immuneCell_lineage
#>   <chr>        <chr>                  <int>             <int> <chr>             
#> 1 At least fo… Tissue enh…                1                 1 NA                
#> 2 nTPM >= 1 i… Low region…                4                 2 NA                
#> 3 nTPM >= 1 i… Low region…                5                 3 NA                
#> 4 nTPM >= 1 i… Low region…                7                 4 NA                
#> 5 nTPM >= 1 i… Low cancer…                9                 5 NA                
#> 6 At least fo… Immune cel…               10                 6 Monocytes         
#> 7 nTPM >= 1 i… Low lineag…               11                 7 NA                
#> # ℹ 1 more variable: immuneCell <chr>
#> 
#> $rnaSpecificity_tissue
#> # A tibble: 2 × 4
#>   organ                          ontologyTerms  value       rnaSpecificity_id
#>   <chr>                          <chr>          <chr>                   <int>
#> 1 Bone marrow & Lymphoid tissues UBERON:0002371 bone marrow                 1
#> 2 Liver & Gallbladder            UBERON:0002107 liver                       1
#> 
#> $sample
#> # A tibble: 522 × 2
#>    patient_id sample_id
#>         <int>     <int>
#>  1          1         1
#>  2          2         2
#>  3          3         3
#>  4          4         4
#>  5          5         5
#>  6          6         6
#>  7          7         7
#>  8          8         8
#>  9          9         9
#> 10         10        10
#> # ℹ 512 more rows
#> 
#> $snomedParameters
#> # A tibble: 522 × 2
#>    sample_id snomedParameters_id
#>        <int>               <int>
#>  1         1                   1
#>  2         2                   2
#>  3         3                   3
#>  4         4                   4
#>  5         5                   5
#>  6         6                   6
#>  7         7                   7
#>  8         8                   8
#>  9         9                   9
#> 10        10                  10
#> # ℹ 512 more rows
#> 
#> $snomedParameters_snomed
#> # A tibble: 1,147 × 3
#>    tissueDescription    snomedCode snomedParameters_id
#>    <chr>                <chr>                    <int>
#>  1 Normal tissue, NOS   M-00100                      1
#>  2 Breast               T-04000                      1
#>  3 Normal tissue, NOS   M-00100                      2
#>  4 Basal cell carcinoma M-80903                      2
#>  5 Cartilage tissue     T-1X700                      2
#>  6 Nasopharynx          T-23000                      2
#>  7 Oral tissue          T-51000                      2
#>  8 Normal tissue, NOS   M-00100                      3
#>  9 Basal cell carcinoma M-80903                      3
#> 10 Skin                 T-01000                      3
#> # ℹ 1,137 more rows
#> 
#> $structure
#> # A tibble: 4 × 6
#>   url                        name  type  method proteinstructure_id structure_id
#>   <chr>                      <chr> <chr> <chr>                <int>        <int>
#> 1 https://v25.proteinatlas.… ENSP… pred… Alpha…                   1            1
#> 2 https://v25.proteinatlas.… ENSP… pred… Alpha…                   1            2
#> 3 https://v25.proteinatlas.… ENSP… pred… Alpha…                   1            3
#> 4 https://v25.proteinatlas.… ENSP… pred… Alpha…                   1            4
#> 
#> $subAssay
#> # A tibble: 1 × 6
#>   type  subtype     cellExpression_id subAssay_id verification_type verification
#>   <chr> <chr>                   <int>       <int> <chr>             <chr>       
#> 1 human human cell…                 2           1 validation        supported   
#> 
#> $tissueCell
#> # A tibble: 182 × 3
#>    data_id tissueCell_id quantity
#>      <int>         <int> <chr>   
#>  1       1             1 NA      
#>  2       2             2 NA      
#>  3       3             3 NA      
#>  4       3             4 NA      
#>  5       4             5 NA      
#>  6       5             6 NA      
#>  7       5             7 NA      
#>  8       5             8 NA      
#>  9       6             9 NA      
#> 10       7            10 NA      
#> # ℹ 172 more rows
#> 
#> $tissueCell_cellType
#> # A tibble: 182 × 2
#>    value                        tissueCell_id
#>    <chr>                                <int>
#>  1 Adipocytes                               1
#>  2 Glandular cells                          2
#>  3 Glandular cells                          3
#>  4 Lymphoid tissue                          4
#>  5 Hematopoietic cells                      5
#>  6 Adipocytes                               6
#>  7 Glandular cells                          7
#>  8 Myoepithelial cells                      8
#>  9 Respiratory epithelial cells             9
#> 10 Glial cells                             10
#> # ℹ 172 more rows
#> 
#> $tissueCell_level
#> # A tibble: 300 × 4
#>    type       value        tissueCell_id count
#>    <chr>      <chr>                <int> <chr>
#>  1 expression not detected             1 NA   
#>  2 expression medium                   2 NA   
#>  3 expression low                      3 NA   
#>  4 expression low                      4 NA   
#>  5 expression low                      5 NA   
#>  6 expression not detected             6 NA   
#>  7 expression low                      7 NA   
#>  8 expression low                      8 NA   
#>  9 expression low                      9 NA   
#> 10 expression not detected            10 NA   
#> # ℹ 290 more rows
#> 
#> $tissueCell_location
#> # A tibble: 81 × 2
#>    value                          tissueCell_id
#>    <chr>                                  <int>
#>  1 none                                      82
#>  2 cytoplasmic/membranous,nuclear            83
#>  3 cytoplasmic/membranous,nuclear            84
#>  4 cytoplasmic/membranous                    85
#>  5 cytoplasmic/membranous,nuclear            86
#>  6 none                                      87
#>  7 cytoplasmic/membranous,nuclear            88
#>  8 cytoplasmic/membranous,nuclear            89
#>  9 cytoplasmic/membranous,nuclear            90
#> 10 cytoplasmic/membranous                    91
#> # ℹ 71 more rows
#> 
#> $tissueExpression
#> # A tibble: 3 × 11
#>   source technology assayType entry_id tissueExpression_id summary_type summary 
#>   <chr>  <chr>      <chr>        <int>               <int> <chr>        <chr>   
#> 1 HPA    IHC        tissue           1                   1 tissue       Cytopla…
#> 2 HPA    IHC        tissue          NA                   2 tissue       Basal c…
#> 3 HPA    IHC        cancer          NA                   3 cancer       A major…
#> # ℹ 4 more variables: verification_type <chr>, verification_description <chr>,
#> #   verification <chr>, antibody_id <int>
#> 
#> $tissueExpression_validation
#> # A tibble: 2 × 3
#>   type                 value                                 tissueExpression_id
#>   <chr>                <chr>                                               <int>
#> 1 RNAConsistency       Low consistency between antibody sta…                   2
#> 2 literatureConformity Partly consistent with extensive gen…                   2
#> 
#> $westernBlot
#> # A tibble: 1 × 8
#>   source technology antibody_id westernBlot_id verification_type
#>   <chr>  <chr>            <int>          <int> <chr>            
#> 1 HPA    WB                   5              1 validation       
#> # ℹ 3 more variables: verification_description <chr>, verification <chr>,
#> #   antibodyDilution_dilution <chr>
#> 
```
