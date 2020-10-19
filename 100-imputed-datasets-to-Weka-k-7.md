100 imputed datasets to Weka K = 7
================
Virginia\_Ahedo
11/10/2020

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

#### Load hierarchical clustering results for K = 7

``` r
load("hc_7.Rda")
str(hc_7)
```

    ## 'data.frame':    1290 obs. of  2 variables:
    ##  $ society_id : Factor w/ 1290 levels "Aa1","Aa2","Aa3",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ hc7.cluster: int  1 1 2 1 3 4 1 1 1 2 ...

##### We transform hc\_7$hc7.cluster into factor so that randomForest function assumes classification

``` r
cluster = as.factor(hc_7$hc7.cluster)
```

#### Obtain the 100 datasets imputed with MICE and join each dataset with the result obtained from hc for K = 7.

##### Drop variables from EA001 to EA005, as after a conversation with the experts on the field, we agreed not to consider them in the RF analysis (they will be highly correlated with the cluster assignments).

``` r
for (k in 1:100){
  # Obtention of the imputed datasets
  imputedDataSet<-complete(imp_data_mice, k)
  not_for_RF = c("Gathering", "Hunting", "Fishing", "Husbandry", "Agriculture")
  imputedDataSet <- select(imputedDataSet, -all_of(not_for_RF))
  imputedDataSet = cbind(imputedDataSet, cluster)
  
  # Saving the imputed datasets as .arff
  arff_name = sprintf('MICE_imputed_df_K7_%s.arff', k)
  path <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing/Multiple_Imputation_4_Weka_K7"
  write.arff(imputedDataSet, file.path(path, file = arff_name))
}
```
