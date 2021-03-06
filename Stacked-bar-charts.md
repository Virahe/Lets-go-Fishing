Stacked bar charts
================
Virginia\_Ahedo
16/10/2020

#### Setup

#### Clean workspace

#### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

#### Load required libraries

``` r
require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
require(DescTools) #Necessary for mode calculation
```

    ## Loading required package: DescTools

``` r
## Libraries necessary for the plots
require(ggplot2)
```

    ## Loading required package: ggplot2

#### Load hc\_interpretation.Rda files

``` r
load("hc2_interpretation.Rda")
load("hc7_interpretation.Rda")
load("hc15_interpretation.Rda")
```

#### Preprocessing the interpretation files

##### Select just columns from EA001 to EA005 and hcx.cluster

``` r
names(hc2_interpretation)
```

    ##  [1] "society_id"            "society_name"          "Gathering"            
    ##  [4] "Hunting"               "Fishing"               "Husbandry"            
    ##  [7] "Agriculture"           "Agriculture_intensity" "major_crop_type"      
    ## [10] "settlement_patterns"   "mean_size_communities" "plow"                 
    ## [13] "domestic_animals"      "milking"               "population_size"      
    ## [16] "hc2.cluster"

##### Create variable cols\_2\_drop

``` r
cols_2_drop = c("society_id", "society_name", "Agriculture_intensity", "major_crop_type",
                "settlement_patterns", "mean_size_communities", "plow", "domestic_animals",
                "milking", "population_size")
```

##### Drop the above-selected columns from the hcx\_interpretation dataframes.

``` r
hc2_interpretation_s <- select(hc2_interpretation, -all_of(cols_2_drop))
hc7_interpretation_s <- select(hc7_interpretation, -all_of(cols_2_drop))
hc15_interpretation_s <- select(hc15_interpretation, -all_of(cols_2_drop))
```

#### Check that the sum from cols EA001 to EA005 is 100.

##### hc\_2

``` r
EA001_005_sum_hc2 = rowSums(hc2_interpretation_s[,1:5])
EA001_005_sum_hc2
```

    ##    [1] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [19] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [37] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [55] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [73] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [91] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [109] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [127] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [145] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [163] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [181] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [199] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [217] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [235] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [253] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [271] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [289] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [307] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [325] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [343] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [361] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [379] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [397] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [415] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [433] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [451] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [469] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [487] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [505] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [523] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [541] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [559] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [577] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [595] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [613] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [631] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [649] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [667] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [685] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [703] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [721] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [739] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [757] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [775] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [793] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [811] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [829] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [847] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [865] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [883] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [901] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [919] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [937] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [955] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [973] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [991] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1009] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1027] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1045] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1063] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1081] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1099] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1117] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1135] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1153] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1171] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1189] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1207] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1225] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1243] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1261] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1279] 100 100 100 100 100 100 100 100 100 100 100 100

##### hc\_7

``` r
EA001_005_sum_hc7 = rowSums(hc7_interpretation_s[,1:5])
EA001_005_sum_hc7
```

    ##    [1] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [19] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [37] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [55] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [73] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [91] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [109] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [127] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [145] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [163] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [181] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [199] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [217] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [235] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [253] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [271] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [289] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [307] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [325] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [343] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [361] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [379] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [397] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [415] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [433] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [451] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [469] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [487] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [505] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [523] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [541] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [559] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [577] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [595] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [613] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [631] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [649] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [667] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [685] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [703] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [721] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [739] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [757] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [775] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [793] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [811] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [829] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [847] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [865] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [883] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [901] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [919] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [937] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [955] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [973] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [991] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1009] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1027] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1045] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1063] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1081] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1099] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1117] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1135] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1153] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1171] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1189] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1207] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1225] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1243] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1261] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1279] 100 100 100 100 100 100 100 100 100 100 100 100

##### hc\_15

``` r
EA001_005_sum_hc15 = rowSums(hc15_interpretation_s[,1:5])
EA001_005_sum_hc15
```

    ##    [1] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [19] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [37] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [55] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [73] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##   [91] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [109] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [127] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [145] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [163] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [181] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [199] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [217] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [235] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [253] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [271] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [289] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [307] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [325] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [343] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [361] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [379] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [397] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [415] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [433] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [451] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [469] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [487] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [505] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [523] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [541] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [559] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [577] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [595] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [613] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [631] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [649] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [667] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [685] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [703] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [721] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [739] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [757] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [775] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [793] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [811] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [829] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [847] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [865] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [883] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [901] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [919] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [937] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [955] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [973] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ##  [991] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1009] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1027] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1045] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1063] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1081] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1099] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1117] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1135] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1153] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1171] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1189] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1207] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1225] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1243] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1261] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
    ## [1279] 100 100 100 100 100 100 100 100 100 100 100 100

#### Obtention of different statistics for each k = 2, 7, 15.

##### The statistics to calculate are: colMeans, std, median, mode.

#### K = 2

``` r
for (k in 1:2){
  
  #Mean
  assign(paste("hc2_colMeans_k_", k, sep = ""),
         colMeans(hc2_interpretation_s[hc2_interpretation_s$hc2.cluster == k, 1:5]))
  
  #Standard deviation
  assign(paste("hc2_sd_k_", k, sep = ""),
         apply(hc2_interpretation_s[hc2_interpretation_s$hc2.cluster == k, 1:5], 2, sd))
  
  #Median
  assign(paste("hc2_median_k_", k, sep = ""),
         apply(hc2_interpretation_s[hc2_interpretation_s$hc2.cluster == k, 1:5], 2, median))
  
  #Mode
  assign(paste("hc2_mode_k_", k, sep = ""),
         apply(hc2_interpretation_s[hc2_interpretation_s$hc2.cluster == k, 1:5], 2, Mode))
  
  #Create a dataframe with all the statistics for each cluster
  assign(paste("hc2_statistics_k_", k, sep = ""),
         rbind(get(paste0("hc2_colMeans_k_", k)), get(paste0("hc2_median_k_", k)), 
               get(paste0("hc2_mode_k_", k)), get(paste0("hc2_sd_k_", k))))
  #Include column with cluster_id
  clust_id = rep(paste("c", k, sep = ""), 4)
  labelling <- cbind(get(paste0("hc2_statistics_k_", k)), clust_id)
  rownames(labelling) <- c("Mean", "Median", "Mode", "SD")
  assign(paste("hc2_statistics_k_", k, sep = ""), data.frame(labelling))
}
```

#### K = 7

``` r
for (l in 1:7){
  
  #Mean
  assign(paste("hc7_colMeans_k_", l, sep = ""),
         colMeans(hc7_interpretation_s[hc7_interpretation_s$hc7.cluster == l, 1:5]))
  
  #Standard deviation
  assign(paste("hc7_sd_k_", l, sep = ""),
         apply(hc7_interpretation_s[hc7_interpretation_s$hc7.cluster == l, 1:5], 2, sd))
  
  #Median
  assign(paste("hc7_median_k_", l, sep = ""),
         apply(hc7_interpretation_s[hc7_interpretation_s$hc7.cluster  == l, 1:5], 2, median))
  
  #Mode
  assign(paste("hc7_mode_k_", l, sep = ""),
         apply(hc7_interpretation_s[hc7_interpretation_s$hc7.cluster == l, 1:5], 2, Mode))
  
  #Create a dataframe with all the statistics for each cluster
  assign(paste("hc7_statistics_k_", l, sep = ""),
         rbind(get(paste0("hc7_colMeans_k_", l)), get(paste0("hc7_median_k_", l)), 
               get(paste0("hc7_mode_k_", l)), get(paste0("hc7_sd_k_", l))))
  clust_id = rep(paste("c", l, sep = ""), 4)
  labelling <- cbind(get(paste0("hc7_statistics_k_", l)), clust_id)
  rownames(labelling) <- c("Mean", "Median", "Mode", "SD")
  assign(paste("hc7_statistics_k_", l, sep = ""), data.frame(labelling))  
}
```

#### K = 15

``` r
for (m in 1:15){
  
  #Mean
  assign(paste("hc15_colMeans_k_", m, sep = ""),
         colMeans(hc15_interpretation_s[hc15_interpretation_s$hc15.cluster == m, 1:5]))
  
  #Standard deviation
  assign(paste("hc15_sd_k_", m, sep = ""),
         apply(hc15_interpretation_s[hc15_interpretation_s$hc15.cluster == m, 1:5], 2, sd))
  
  #Median
  assign(paste("hc15_median_k_", m, sep = ""),
         apply(hc15_interpretation_s[hc15_interpretation_s$hc15.cluster == m, 1:5], 2, median))
  
  #Mode
  assign(paste("hc15_mode_k_", m, sep = ""), 
         apply(hc15_interpretation_s[hc15_interpretation_s$hc15.cluster == m, 1:5], 2, Mode))
  
  #Create a dataframe with all the statistics for each cluster
  assign(paste("hc15_statistics_k_", m, sep = ""),
         rbind(get(paste0("hc15_colMeans_k_", m)), get(paste0("hc15_median_k_", m)), 
               get(paste0("hc15_mode_k_", m)), get(paste0("hc15_sd_k_", m))))
  clust_id = rep(paste("c", m, sep = ""), 4)
  labelling <- cbind(get(paste0("hc15_statistics_k_", m)), clust_id)
  rownames(labelling) <- c("Mean", "Median", "Mode", "SD")
  assign(paste("hc15_statistics_k_", m, sep = ""), data.frame(labelling))    
}
```

#### Plots of the average values for the different Ks.

##### Fist step: for each K, rowBind the mean values of each cluster in a dataframe.

##### Second step: for each K, rowBind the sd of each cluster in a dataframe

#### K = 2

##### Mean values dataframe

``` r
mean_df_2 <- data.frame()
sd_df_2 <- data.frame()
for (i in 1:2){
  mean_df_2 = rbind(mean_df_2, get(paste0("hc2_colMeans_k_", i)))
  sd_df_2 = rbind(sd_df_2, get(paste0("hc2_sd_k_", i)))
}

# Name rows and cols from mean_df_2 and sd_df_2
colnames(mean_df_2) <- names(hc2_colMeans_k_1)
rownames(mean_df_2) <- c("c1", "c2")
colnames(sd_df_2) <- names(hc2_colMeans_k_1)
rownames(sd_df_2) <- c("c1", "c2")
```

#### K = 7

##### Mean values dataframe

``` r
mean_df_7 <- data.frame()
sd_df_7 <- data.frame()
for (j in 1:7){
  mean_df_7 = rbind(mean_df_7, get(paste0("hc7_colMeans_k_", j)))
  sd_df_7 = rbind(sd_df_7, get(paste0("hc7_sd_k_", j)))
}

# Name rows and cols from mean_df_7 and sd_df_7
colnames(mean_df_7) <- names(hc7_colMeans_k_1)
rownames(mean_df_7) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")
colnames(sd_df_7) <- names(hc7_colMeans_k_1)
rownames(sd_df_7) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")
```

#### K = 15

##### Mean values dataframe

``` r
mean_df_15 <- data.frame()
sd_df_15 <- data.frame()
for (k in 1:15){
  mean_df_15 = rbind(mean_df_15, get(paste0("hc15_colMeans_k_", k)))
  sd_df_15 = rbind(sd_df_15, get(paste0("hc15_sd_k_", k)))
}

# Name rows and cols from mean_df_15 and sd_df_15
colnames(mean_df_15) <- names(hc15_colMeans_k_1)
rownames(mean_df_15) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8",
                          "c9", "c10", "c11", "c12", "c13", "c14", "c15")
colnames(sd_df_15) <- names(hc15_colMeans_k_1)
rownames(sd_df_15) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8",
                        "c9", "c10", "c11", "c12", "c13", "c14", "c15")
```

### Percentage stacked bar charts for each K

##### <https://community.rstudio.com/t/create-a-percentage-stacked-bar-chart/26223/5>

##### <http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization>

##### <https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot.html>

#### colMeans plot (without sd)

##### K = 2

``` r
#par(mar) --> sets the margin sizes in the following order: bottom, left, top and right
pdf("hc2_mean.pdf", width = 8, height = 6)
par(oma = c(4, 4, 4, 4), mar=c(3, 2, 2, 10))
barplot(t(mean_df_2), legend.text = TRUE, horiz = TRUE, 
        col = c("#abd9e9", "#d7191c", "#2c7bb6", "#fdae61", "#ffffbf"),
        main = "K = 2: Mean value of subsistence vars. per cluster",
        xpd = TRUE, plot = TRUE,
        args.legend = list(x = 140, y = 1.5, bty = "n", cex = 0.8))
dev.off()
```

    ## png 
    ##   2

##### K = 7

``` r
pdf("hc7_mean.pdf", width = 8, height = 6)
par(oma = c(4, 4, 4, 4), mar=c(3, 2, 2, 10))
barplot(t(mean_df_7), legend.text = TRUE, horiz = TRUE, 
        col = c("#abd9e9", "#d7191c", "#2c7bb6", "#fdae61", "#ffffbf"),
        main = "K = 7: Mean value of subsistence vars. per cluster",
        xpd = TRUE, plot = TRUE,
        args.legend = list(x = 140, y = 5.5, bty = "n", cex = 0.8))
dev.off()
```

    ## png 
    ##   2

##### K = 15

``` r
pdf("hc15_mean.pdf", width = 11, height = 9)
par(oma = c(4, 4, 4, 4), mar=c(3, 2, 2, 10))
barplot(t(mean_df_15), legend.text = TRUE, horiz = TRUE, 
        col = c("#abd9e9", "#d7191c", "#2c7bb6", "#fdae61", "#ffffbf"),
        main = "K = 15: Mean value of subsistence vars. per cluster",
        xpd = TRUE, plot = TRUE,
        cex.names = 0.95,
        space = 0.5,
        args.legend = list(x = 127, y = 10.5, bty = "n"))
dev.off()
```

    ## png 
    ##   2

#### To try to include error bars: ggplot

##### The data structure required by ggplot is different

##### Create ggplot\_required dataframes for K = 2, 7, 15

##### K = 2

``` r
require(varhandle)
```

    ## Loading required package: varhandle

``` r
gg_mean_v_2 <- vector()
gg_sd_v_2 <- vector()
for (i in 1:2){
  gg_mean_v_2 = c(gg_mean_v_2, get(paste0("hc2_colMeans_k_", i)))
  gg_sd_v_2 = c(gg_sd_v_2, get(paste0("hc2_sd_k_", i)))
}
cluster_id_2 = c(rep("c1",length(hc2_colMeans_k_1)), rep("c2", length(hc2_colMeans_k_1)))
variables_2 = rep(names(hc2_colMeans_k_1),2)
gg_mean_df_2 = data.frame(cbind(cluster_id_2, variables_2, gg_mean_v_2))
colnames(gg_mean_df_2) <- c("cluster_id", "subsistence_vars", "value")
rownames(gg_mean_df_2) <- NULL
gg_mean_df_2$value <- unfactor(gg_mean_df_2$value)
```

#### Add error bars to stacked bar plot in ggplot2 - R. To that end it is necessary to compute the y position before plotting:

``` r
y_pos_c1 = c(gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Fishing"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Fishing"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c1" & 
                                         gg_mean_df_2$subsistence_vars == "Agriculture"])

y_pos_c2 = c(gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Fishing"],
             gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                       gg_mean_df_2$subsistence_vars == "Husbandry"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Hunting"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Gathering"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Fishing"]+
               gg_mean_df_2$value[gg_mean_df_2$cluster_id == "c2" & 
                                         gg_mean_df_2$subsistence_vars == "Agriculture"])
```

##### Create k2\_plot dataset

``` r
y_pos = c(y_pos_c1, y_pos_c2)
gg_mean_df_2_thursday = cbind(gg_mean_df_2, y_pos, gg_sd_v_2)
```

``` r
ggplot(data=gg_mean_df_2_thursday, aes(x=cluster_id, y=value, fill=subsistence_vars)) + 
  geom_bar(stat="identity",
       #position="stack",
       width = 0.4) +                           
  xlab(" ") + ylab("Percentage (%)") + 
  theme_classic(base_size = 16) + 
  theme(axis.text.y=element_text(size=16, face="bold")) + 
  theme(axis.title.y=element_text(size=16, face="bold", vjust=1)) + 
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1, size=16, face="bold")) +
  theme(legend.position="right")+
  coord_flip() +
  geom_errorbar(aes(ymax = y_pos + gg_sd_v_2, ymin = y_pos - gg_sd_v_2),
                position = "identity",
                width = 0.1)
```

![](Stacked-bar-charts_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->
