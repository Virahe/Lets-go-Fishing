Creation\_subsistence\_database
================
Virginia\_Ahedo
5/10/2020

### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

### Load required libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(varhandle)
```

### Data source: Ethnographic Atlas.

### Variables considered:

#### Subsistence variables: percentage of dependence on the different subsistence strategies:

##### EA001: % of dependence on gathering

##### EA002: % of dependence on hunting

##### EA003: % of dependence on fishing

##### EA004: % of dependence on animal husbandry

##### EA005: % of dependence on agriculture

#### EA028 (Agriculture Intensity): Intensity of cultivation

``` r
EA001 = read.csv("EA001.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA002 = read.csv("EA002.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA003 = read.csv("EA003.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA004 = read.csv("EA004.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA005 = read.csv("EA005.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA028 = read.csv("EA028.csv", header = T, sep = ",", fileEncoding = "UTF-8")
```

#### Check dimensions:

``` r
cat("Dimensions of dataset EA001: ", dim(EA001))
```

    ## Dimensions of dataset EA001:  1290 12

``` r
cat("\nDimensions of dataset EA002: ", dim(EA002))
```

    ## 
    ## Dimensions of dataset EA002:  1290 12

``` r
cat("\nDimensions of dataset EA003: ", dim(EA003))
```

    ## 
    ## Dimensions of dataset EA003:  1290 12

``` r
cat("\nDimensions of dataset EA004: ", dim(EA004))
```

    ## 
    ## Dimensions of dataset EA004:  1290 12

``` r
cat("\nDimensions of dataset EA005: ", dim(EA005))
```

    ## 
    ## Dimensions of dataset EA005:  1290 12

``` r
cat("\nDimensions of dataset EA028: ", dim(EA028))
```

    ## 
    ## Dimensions of dataset EA028:  1188 12

``` r
# As we see, for variable EA028 the sample of societies is reduced to 1188. After carefully thinking about this fact, we have decided to maintain the sample size of 1290 societies, which implies not dropping those societies which have NAs for EA028. Therefore, we will perform a left join including the attribute all = T, so that it does not drop registers with NAs. 
# More precisely, the clustering analysis will be conducted on variables EA001 to EA005, being EA028 only used as a supplementary variable for PCA analysis. Note that EA028 is an ordinal variable (ordered categorical).
```

#### Preprocessing:

#### Check existing columns:

``` r
names(EA001)
```

    ##  [1] "society_id"          "society_name"        "society_xd_id"      
    ##  [4] "language_glottocode" "language_name"       "language_family"    
    ##  [7] "variable_id"         "code"                "code_label"         
    ## [10] "focal_year"          "sub_case"            "comment"

#### Drop unnecessary columns before building the final db

##### So as to ease future left joins we will keep “society\_name” just for EA001 and “society\_id” for all the rest of variables

##### We will keep “code\_label” in all cases

##### Columns to drop

``` r
cols2drop_EA001 = c("society_xd_id", "language_glottocode", "language_name","language_family",
                    "variable_id", "code", "focal_year", "sub_case", "comment")
cols2drop_rest = c("society_name", "society_xd_id", "language_glottocode", "language_name",
              "language_family","variable_id", "code", "focal_year", "sub_case", "comment")
```

##### Drop the corresponding cols

##### Unfactor columns “society\_id” and “society\_name” in all variables

``` r
EA001_s <- select(EA001, -all_of(cols2drop_EA001))
print(str(EA001_s))
```

    ## 'data.frame':    1290 obs. of  3 variables:
    ##  $ society_id  : Factor w/ 1290 levels "Aa1","Aa2","Aa3",..: 11 13 14 16 18 20 22 24 25 27 ...
    ##  $ society_name: Factor w/ 1290 levels "!Kung","/Xam",..: 921 1289 1166 901 810 36 836 169 245 1165 ...
    ##  $ code_label  : Factor w/ 9 levels "0-5%","16-25%",..: 1 1 1 1 1 1 1 1 1 1 ...
    ## NULL

``` r
EA001_s[,1:2] <- unfactor(EA001_s[,1:2])
names(EA001_s)[names(EA001_s) == "code_label"] <- "Gathering"

EA002_s <- select(EA002, -all_of(cols2drop_rest))
EA002_s[,1] <- unfactor(EA002_s[,1])
names(EA002_s)[names(EA002_s) == "code_label"] <- "Hunting"

EA003_s <- select(EA003, -all_of(cols2drop_rest))
EA003_s[,1] <- unfactor(EA003_s[,1])
names(EA003_s)[names(EA003_s) == "code_label"] <- "Fishing"

EA004_s <- select(EA004, -all_of(cols2drop_rest))
EA004_s[,1] <- unfactor(EA004_s[,1])
names(EA004_s)[names(EA004_s) == "code_label"] <- "Husbandry"

EA005_s <- select(EA005, -all_of(cols2drop_rest))
EA005_s[,1] <- unfactor(EA005_s[,1])
names(EA005_s)[names(EA005_s) == "code_label"] <- "Agriculture"

EA028_s <- select(EA028, -all_of(cols2drop_rest))
EA028_s[,1] <- unfactor(EA028_s[,1])
names(EA028_s)[names(EA028_s) == "code_label"] <- "Agriculture_intensity"
```

#### Build unified data base

##### Left join keeping all societies: Include attribute all = T, so that it does not drop registers with NAs

``` r
EA_unified_01 = merge(EA001_s, EA002_s, by.x="society_id", all = T)
EA_unified_02 = merge(EA_unified_01, EA003_s, by.x="society_id", all = T)
EA_unified_03 = merge(EA_unified_02, EA004_s, by.x="society_id", all = T)
EA_unified_04 = merge(EA_unified_03, EA005_s, by.x="society_id", all = T)
EA_unified_05 = merge(EA_unified_04, EA028_s, by.x="society_id", all = T)
```

#### Check the structure of the unified data base

``` r
str(EA_unified_05)
```

    ## 'data.frame':    1290 obs. of  8 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ society_name         : chr  "!Kung" "Dorobo" "Nama" "Bergdama" ...
    ##  $ Gathering            : Factor w/ 9 levels "0-5%","16-25%",..: 9 4 7 4 3 7 8 5 6 7 ...
    ##  $ Hunting              : Factor w/ 10 levels "0-5%","16-25%",..: 2 6 3 3 8 7 3 4 4 3 ...
    ##  $ Fishing              : Factor w/ 10 levels "0-5%","16-25%",..: 1 1 7 7 1 7 1 7 1 1 ...
    ##  $ Husbandry            : Factor w/ 10 levels "0-5%","16-25%",..: 1 1 5 2 1 2 1 1 1 6 ...
    ##  $ Agriculture          : Factor w/ 10 levels "0-5%","16-25%",..: 1 1 1 1 1 5 1 1 1 1 ...
    ##  $ Agriculture_intensity: Factor w/ 6 levels "Casual","Extensive/shifting",..: 6 6 6 NA 6 2 6 6 6 6 ...

#### Turn factors into numeric values for variables EA001 to EA005 –\> Take the interval mean value

``` r
## Alternative: work with the column "code"" present in all the .csv files (less interpretable)

## Levels of the factor % of dependence on xxx subsistence strategy
factor_levels = c("0-5%", "6-15%", "16-25%", "26-35%", "36-45%", "46-55%", "56-65%",
                  "66-75%", "76-85%", "86-100%")
interval_mean_vals = c(2.5, 10.5, 20.5, 30.5, 40.5, 50.5, 60.5, 70.5, 80.5, 93)

library(foreach)

# Iterate over columns: 3,4,5,6,7 (Gathering, Hunting, Fishing, Husbandry, Agriculture)
for (i in 3:7){
  foreach(j = factor_levels, k = interval_mean_vals) %do% {
    levels(EA_unified_05[,i])[levels(EA_unified_05[,i]) == j] <- k
  }
}
```

#### Check the structure of the dataset obtained

``` r
str(EA_unified_05)
```

    ## 'data.frame':    1290 obs. of  8 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ society_name         : chr  "!Kung" "Dorobo" "Nama" "Bergdama" ...
    ##  $ Gathering            : Factor w/ 9 levels "2.5","20.5","30.5",..: 9 4 7 4 3 7 8 5 6 7 ...
    ##  $ Hunting              : Factor w/ 10 levels "2.5","20.5","30.5",..: 2 6 3 3 8 7 3 4 4 3 ...
    ##  $ Fishing              : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 7 7 1 7 1 7 1 1 ...
    ##  $ Husbandry            : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 5 2 1 2 1 1 1 6 ...
    ##  $ Agriculture          : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 1 1 1 5 1 1 1 1 ...
    ##  $ Agriculture_intensity: Factor w/ 6 levels "Casual","Extensive/shifting",..: 6 6 6 NA 6 2 6 6 6 6 ...

#### Create Complete NAMED subsistence database: with BOTH society\_id and society\_name

``` r
named_clust_db = EA_unified_05
str(named_clust_db)
```

    ## 'data.frame':    1290 obs. of  8 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ society_name         : chr  "!Kung" "Dorobo" "Nama" "Bergdama" ...
    ##  $ Gathering            : Factor w/ 9 levels "2.5","20.5","30.5",..: 9 4 7 4 3 7 8 5 6 7 ...
    ##  $ Hunting              : Factor w/ 10 levels "2.5","20.5","30.5",..: 2 6 3 3 8 7 3 4 4 3 ...
    ##  $ Fishing              : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 7 7 1 7 1 7 1 1 ...
    ##  $ Husbandry            : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 5 2 1 2 1 1 1 6 ...
    ##  $ Agriculture          : Factor w/ 10 levels "2.5","20.5","30.5",..: 1 1 1 1 1 5 1 1 1 1 ...
    ##  $ Agriculture_intensity: Factor w/ 6 levels "Casual","Extensive/shifting",..: 6 6 6 NA 6 2 6 6 6 6 ...

#### Unfactor variables from Gathering to Agriculture in named\_clust\_db

``` r
named_clust_db[,3:7] = unfactor(named_clust_db[,3:7])
str(named_clust_db)
```

    ## 'data.frame':    1290 obs. of  8 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ society_name         : chr  "!Kung" "Dorobo" "Nama" "Bergdama" ...
    ##  $ Gathering            : num  80.5 40.5 10.5 40.5 30.5 10.5 70.5 50.5 60.5 10.5 ...
    ##  $ Hunting              : num  20.5 60.5 30.5 30.5 70.5 10.5 30.5 40.5 40.5 30.5 ...
    ##  $ Fishing              : num  2.5 2.5 10.5 10.5 2.5 10.5 2.5 10.5 2.5 2.5 ...
    ##  $ Husbandry            : num  2.5 2.5 50.5 20.5 2.5 20.5 2.5 2.5 2.5 60.5 ...
    ##  $ Agriculture          : num  2.5 2.5 2.5 2.5 2.5 50.5 2.5 2.5 2.5 2.5 ...
    ##  $ Agriculture_intensity: Factor w/ 6 levels "Casual","Extensive/shifting",..: 6 6 6 NA 6 2 6 6 6 6 ...

#### Scale variables EA001 to EA005 so that they sum up to 100.

``` r
## Create a column which would be the sum of EA001 to EA005 (cols 3:7 in named_clust_db)
EA001_to_EA005_sum <- array(dim = c(dim(named_clust_db)[1],1))
for (l in 1:dim(named_clust_db)[1]){
  EA001_to_EA005_sum[l,1] = sum(named_clust_db[l,3:7])
}

## Calculation of conversion factor
conversion_Factors = 100/EA001_to_EA005_sum

## Multiply columns EA001 to EA005 by the conversion factor
for (i in 1:dim(named_clust_db)[1]){
  named_clust_db[i,3:7] <- named_clust_db[i,3:7]*conversion_Factors[i,1]
}
```

#### Save the NAMED Clustering database: named\_clust\_db (with both society\_id and society\_name)

``` r
save(named_clust_db, file = "Named_Clustering_db.Rda")
write.csv(named_clust_db, file = "Named_Clustering_db.csv")
```

#### Create ID Clustering database with society\_id and without society\_name

``` r
id_clust_db = select(named_clust_db, -c("society_name"))
```

#### Save the ID Clustering database: id\_clust\_db (with society\_id and without society\_name)

``` r
save(id_clust_db, file = "Id_Clustering_db.Rda")
write.csv(id_clust_db, file = "Id_Clustering_db.csv")
```

#### Create final\_clust\_db

##### Drop “society\_name” & “society\_id” in order to perform PCA and Clustering Analysis

``` r
final_clust_db = select(named_clust_db, -c("society_id", "society_name"))
```

#### Save the FINAL Clustering database: final\_clust\_db

``` r
save(final_clust_db, file = "Final_Clustering_db.Rda")
write.csv(final_clust_db, file = "Final_Clustering_db.csv")
```
