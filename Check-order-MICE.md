Check order MICE
================
Virginia\_Ahedo
19/10/2020

#### Setup

#### Clean workspace

#### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

#### Load required libraries

``` r
require(mice)
```

    ## Loading required package: mice

    ## 
    ## Attaching package: 'mice'

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

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
require(varhandle)
```

    ## Loading required package: varhandle

``` r
require(RWeka)
```

    ## Loading required package: RWeka

#### Load Multiple Imputation (MICE) results

``` r
imp_data_mice = readRDS("data_imp_MICE_def.Rda")
```

#### Load hierarchical clustering results for K = 15

``` r
load("hc_15.Rda")
str(hc_15)
```

    ## 'data.frame':    1290 obs. of  2 variables:
    ##  $ society_id  : Factor w/ 1290 levels "Aa1","Aa2","Aa3",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ hc15.cluster: int  1 2 3 2 4 5 1 1 1 3 ...

#### Load “Database\_for\_MICE.Rda”

``` r
load("Database_for_MICE.Rda")
```

##### Drop row 1125 from EA\_ecoClimate\_final

``` r
data = EA_ecoClimate_final[-1125,]
str(data)
```

    ## 'data.frame':    1290 obs. of  34 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ Gathering            : int  8 4 1 4 3 1 7 5 6 1 ...
    ##  $ Hunting              : int  2 6 3 3 7 1 3 4 4 3 ...
    ##  $ Fishing              : int  0 0 1 1 0 1 0 1 0 0 ...
    ##  $ Husbandry            : int  0 0 5 2 0 2 0 0 0 6 ...
    ##  $ Agriculture          : int  0 0 0 0 0 5 0 0 0 0 ...
    ##  $ Agriculture_intensity: int  1 1 1 NA 1 3 1 1 1 1 ...
    ##  $ Major_crop_type      : int  1 1 1 NA 1 6 1 1 1 1 ...
    ##  $ Settlement_pattern   : int  1 1 1 NA 1 5 1 1 1 1 ...
    ##  $ Political_complexity : int  1 1 2 NA 1 2 1 1 1 1 ...
    ##  $ Plow_cultivation     : int  1 1 1 NA 1 1 1 1 1 1 ...
    ##  $ Animal_type          : Factor w/ 7 levels "Absence or near absence",..: 1 1 2 NA 1 2 1 1 1 2 ...
    ##  $ Milking              : Factor w/ 2 levels "Absence or near absence",..: 1 1 2 NA 1 2 1 1 1 2 ...
    ##  $ Population_size      : num  3500 1100 2500 29000 40000 23400 300 NA 600 100000 ...
    ##  $ Amphibian_richness   : num  11.6 27 3 5 39.5 ...
    ##  $ Bird_richness        : num  205 461 136 216 339 ...
    ##  $ Mammal_richness      : num  73 173.5 55.8 70 159.9 ...
    ##  $ Plants_richness      : num  1090 2127 1584 1232 2038 ...
    ##  $ Annual_NPP_Var       : num  0.4118 0.3358 0.0265 0.1067 1.5974 ...
    ##  $ Monthly_mean_NPP     : num  0.536 3.046 0.119 0.324 3.486 ...
    ##  $ NPP_Constancy        : num  0.288 0.238 0.67 0.507 0.064 ...
    ##  $ NPP_Contingency      : num  0.2062 0.1477 0.0969 0.158 0.2623 ...
    ##  $ NPP_Predictability   : num  0.494 0.385 0.767 0.665 0.326 ...
    ##  $ Biome                : Factor w/ 16 levels "Boreal Forests/Taiga",..: 14 14 2 2 15 14 14 2 14 2 ...
    ##  $ Annual_Mean_Temp     : num  21 19 20.4 22.7 21.4 ...
    ##  $ Annual_Precip_Var    : num  1.16e+10 3.36e+09 2.16e+09 3.65e+09 9.48e+09 ...
    ##  $ Annual_Temp_Var      : num  14.599 1.212 20.63 8.882 0.306 ...
    ##  $ Monthly_Mean_Precip  : num  87000 50566 39670 42805 243308 ...
    ##  $ Precip_Constancy     : num  0.203 0.44 0.304 0.195 0.674 ...
    ##  $ Precip_Contingency   : num  0.276 0.147 0.16 0.207 0.114 ...
    ##  $ Precip_Predictability: num  0.478 0.587 0.464 0.402 0.788 ...
    ##  $ Temp_Constancy       : num  0.458 0.667 0.428 0.493 0.769 ...
    ##  $ Temp_Contingency     : num  0.2061 0.0938 0.2151 0.152 0.0446 ...
    ##  $ Temp_Predictability  : num  0.664 0.761 0.643 0.645 0.814 ...

##### Given that we dropped the col “society\_id” before performing the multiple imputation via MICE, it is necessary to check that mice preserves the order of rows in the imputed datasets before joining them with the clustering results for K = 15 (cluster assignments).

#### Obtention of the 1st imputed dataset

``` r
imputedDataSet_1<-complete(imp_data_mice, 1)
```

##### Check that the “society\_id” order in hc\_15 and in data is the same

``` r
names(data)
```

    ##  [1] "society_id"            "Gathering"             "Hunting"              
    ##  [4] "Fishing"               "Husbandry"             "Agriculture"          
    ##  [7] "Agriculture_intensity" "Major_crop_type"       "Settlement_pattern"   
    ## [10] "Political_complexity"  "Plow_cultivation"      "Animal_type"          
    ## [13] "Milking"               "Population_size"       "Amphibian_richness"   
    ## [16] "Bird_richness"         "Mammal_richness"       "Plants_richness"      
    ## [19] "Annual_NPP_Var"        "Monthly_mean_NPP"      "NPP_Constancy"        
    ## [22] "NPP_Contingency"       "NPP_Predictability"    "Biome"                
    ## [25] "Annual_Mean_Temp"      "Annual_Precip_Var"     "Annual_Temp_Var"      
    ## [28] "Monthly_Mean_Precip"   "Precip_Constancy"      "Precip_Contingency"   
    ## [31] "Precip_Predictability" "Temp_Constancy"        "Temp_Contingency"     
    ## [34] "Temp_Predictability"

``` r
str(hc_15$society_id)
```

    ##  Factor w/ 1290 levels "Aa1","Aa2","Aa3",..: 1 2 3 4 5 6 7 8 9 10 ...

``` r
str(data$society_id)
```

    ##  chr [1:1290] "Aa1" "Aa2" "Aa3" "Aa4" "Aa5" "Aa6" "Aa7" "Aa8" "Aa9" "Ab1" ...

#### Load “compare” package

``` r
require(compare)
```

    ## Loading required package: compare

    ## 
    ## Attaching package: 'compare'

    ## The following object is masked from 'package:base':
    ## 
    ##     isTRUE

``` r
compare(unfactor(hc_15$society_id), data$society_id, ignoreOrder = F)
```

    ## TRUE

##### Check that the order in imputedDataSet\_1 is the same as in data (EA001 to EA005)

``` r
names(imputedDataSet_1)
```

    ##  [1] "Gathering"             "Hunting"               "Fishing"              
    ##  [4] "Husbandry"             "Agriculture"           "Agriculture_intensity"
    ##  [7] "Major_crop_type"       "Settlement_pattern"    "Political_complexity" 
    ## [10] "Plow_cultivation"      "Animal_type"           "Milking"              
    ## [13] "Population_size"       "Amphibian_richness"    "Bird_richness"        
    ## [16] "Mammal_richness"       "Plants_richness"       "Annual_NPP_Var"       
    ## [19] "Monthly_mean_NPP"      "NPP_Constancy"         "NPP_Contingency"      
    ## [22] "NPP_Predictability"    "Biome"                 "Annual_Mean_Temp"     
    ## [25] "Annual_Precip_Var"     "Annual_Temp_Var"       "Monthly_Mean_Precip"  
    ## [28] "Precip_Constancy"      "Precip_Contingency"    "Precip_Predictability"
    ## [31] "Temp_Constancy"        "Temp_Contingency"      "Temp_Predictability"

``` r
str(imputedDataSet_1[,1:5])
```

    ## 'data.frame':    1290 obs. of  5 variables:
    ##  $ Gathering  : int  8 4 1 4 3 1 7 5 6 1 ...
    ##  $ Hunting    : int  2 6 3 3 7 1 3 4 4 3 ...
    ##  $ Fishing    : int  0 0 1 1 0 1 0 1 0 0 ...
    ##  $ Husbandry  : int  0 0 5 2 0 2 0 0 0 6 ...
    ##  $ Agriculture: int  0 0 0 0 0 5 0 0 0 0 ...

``` r
str(data[,2:6])
```

    ## 'data.frame':    1290 obs. of  5 variables:
    ##  $ Gathering  : int  8 4 1 4 3 1 7 5 6 1 ...
    ##  $ Hunting    : int  2 6 3 3 7 1 3 4 4 3 ...
    ##  $ Fishing    : int  0 0 1 1 0 1 0 1 0 0 ...
    ##  $ Husbandry  : int  0 0 5 2 0 2 0 0 0 6 ...
    ##  $ Agriculture: int  0 0 0 0 0 5 0 0 0 0 ...

``` r
comparison = compare(imputedDataSet_1[,1:5], data[,2:6], ignoreOrder = F)
comparison$detailedResult
```

    ##   Gathering     Hunting     Fishing   Husbandry Agriculture 
    ##        TRUE        TRUE        TRUE        TRUE        TRUE

``` r
comparison$transform
```

    ## character(0)

``` r
comparison$result
```

    ## [1] FALSE

##### Let’s try to find the difference with all.equal()

``` r
all.equal(imputedDataSet_1[,1:5], data[,2:6])
```

    ## [1] "Attributes: < Component \"row.names\": Mean relative difference: 0.0008281573 >"

##### Obtain row.names for both imputedDataSet\_1 and data

``` r
rn_imputed = row.names(imputedDataSet_1)
rn_data = row.names(data)
```

``` r
which(rn_imputed %in% rn_data == FALSE)
```

    ## [1] 1125

``` r
length(row.names(data))
```

    ## [1] 1290

``` r
length(row.names(imputedDataSet_1))
```

    ## [1] 1290

``` r
all.equal(rn_data, rn_imputed)
```

    ## [1] "166 string mismatches"

##### Check if by setting row.names \<- NULL we solve the problem

``` r
row.names(data) <- NULL
row.names(imputedDataSet_1) <- NULL
```

##### Second check

``` r
compare(imputedDataSet_1[,1:5], data[,2:6], ignoreOrder = F)
```

    ## TRUE
