Create MICE database
================
Virginia\_Ahedo
10/10/2020

### Setup

### Clean workspace

### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

### Load required libraries

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
require(xlsx)
```

    ## Loading required package: xlsx

### Variables included in the MICE database

#### 1\. Ethnographic Atlas (EA): Economy and subsistence

##### \-EA001: % dependence on gathering

##### \-EA002: % dependence on hunting

##### \-EA003: % dependence on fishing

##### \-EA004: % dependence on husbandry

##### \-EA005: % dependence on agriculture

##### \-EA028: Intensity of cultivation

##### \-EA029: Major crop type

##### \-EA030: Prevailing type settlement pattern

##### \-EA033: Jurisdictional hierarchy beyond local community (Political complexity)

##### \-EA039: Plow cultivation

##### \-EA040: Domestic animals type

##### \-EA041: Milking

##### \-EA202: Population size

##### EA031: Mean size of local communities was not included due to the significant amount of missing data it had.

##### It is also important to note that in the supervised learning analysis we will not consider variables from EA001 to EA005, as the clustering analysis has been conducted exclusively on the five of them, and hence they are closely related to the output.

##### However, for multiple imputation we DO consider variables EA001 to EA005 to help get more accurate estimates of the missing values.

#### 2\. Variables from Jenkins et al. 2013:

##### \- Amphibian Richness

##### \- Bird Richness

##### \- Mammal Richness

#### 3\. Variables from Kreft and Jetz

##### \- Vascular plants Richness

#### 4\. Variables from Moderate Resolution Imaging Spectroradiometer

##### \- Annual NPP Variance

##### \- Monthly mean NPP

##### \- NPP Constancy

##### \- NPP Contingency

##### \- NPP Predictability

#### 5\. Variables from Terrestrial Ecoregions of the World

##### \- Biome

#### 6\. Variables from Baseline Historical ecoClimate model

##### \- Annual Mean Temperature

##### \- Annual Precipitation Variance

##### \- Annual Temperature Variance

##### \- Monthly Mean Precipitation

##### \- Precipitation Constancy

##### \- Precipitation Contingency

##### \- Precipitation Predictability

##### \- Temperature Constancy

##### \- Temperature Contingency

##### \- Temperature Predictability

### RAW DATA

#### 1\. Ethnographic Atlas

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/EA"))
# 1. Ethnographic Atlas (EA):
# -EA001: % dependence on gathering
# -EA002: % dependence on hunting
# -EA003: % dependence on fishing
# -EA004: % dependence on husbandry
# -EA005: % dependence on agriculture
# -EA028: Intensity of cultivation --> NUMERICAL
# -EA029: Major crop type --> NUMERICAL
# -EA030: Prevailing type settlement pattern --> NUMERICAL
# -EA033: Jurisdictional hierarchy beyond local community (Political complexity) --> NUMERICAL
# -EA039: Plow cultivation --> NUMERICAL
# -EA040: Domestic animals type --> CATEGORICAL
# -EA041: Milking --> CATEGORICAL
# -EA202: Population size

EA001 = read.csv("EA001.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA002 = read.csv("EA002.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA003 = read.csv("EA003.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA004 = read.csv("EA004.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA005 = read.csv("EA005.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA028 = read.csv("EA028.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA029 = read.csv("EA029.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA030 = read.csv("EA030.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA033 = read.csv("EA033.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA039 = read.csv("EA039.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA040 = read.csv("EA040.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA041 = read.csv("EA041.csv", header = T, sep = ",", fileEncoding = "UTF-8")
EA202 = read.csv("EA202.csv", header = T, sep = ",", fileEncoding = "UTF-8")
```

##### Check EA dimensionality coherence

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
cat("\nDimensions of dataset EA029: ", dim(EA029))
```

    ## 
    ## Dimensions of dataset EA029:  1179 12

``` r
cat("\nDimensions of dataset EA030: ", dim(EA030))
```

    ## 
    ## Dimensions of dataset EA030:  1187 12

``` r
cat("\nDimensions of dataset EA033: ", dim(EA033))
```

    ## 
    ## Dimensions of dataset EA033:  1155 12

``` r
cat("\nDimensions of dataset EA039: ", dim(EA039))
```

    ## 
    ## Dimensions of dataset EA039:  1182 12

``` r
cat("\nDimensions of dataset EA040: ", dim(EA040))
```

    ## 
    ## Dimensions of dataset EA040:  1182 12

``` r
cat("\nDimensions of dataset EA041: ", dim(EA041))
```

    ## 
    ## Dimensions of dataset EA041:  1182 12

``` r
cat("\nDimensions of dataset EA202: ", dim(EA202))
```

    ## 
    ## Dimensions of dataset EA202:  953 12

#### Drop unnecessary columns before building the unified db

##### For ordinal variables, we will keep the data in the code column and set it as.numeric()

``` r
#Columns to drop in the variables that are NUMERICAL
num_vars_cols2drop = c("society_name", "society_xd_id", "language_glottocode", "language_name",
                       "language_family","variable_id", "code_label", "focal_year", "sub_case",
                       "comment")

#Columns to drop in the variables that are CATEGORICAL
cat_vars_cols2drop = c("society_name", "society_xd_id", "language_glottocode", "language_name",
                       "language_family","variable_id", "code", "focal_year", "sub_case", "comment")
```

##### Preprocessing of EA NUMERICAL VARIABLES

``` r
# Percentage of dependence on different subsistence economies
EA001_s <- unfactor(select(EA001, -all_of(num_vars_cols2drop)))
names(EA001_s)[names(EA001_s) == "code"] <- "Gathering"

EA002_s <- unfactor(select(EA002, -all_of(num_vars_cols2drop)))
names(EA002_s)[names(EA002_s) == "code"] <- "Hunting"

EA003_s <- unfactor(select(EA003, -all_of(num_vars_cols2drop)))
names(EA003_s)[names(EA003_s) == "code"] <- "Fishing"

EA004_s <- unfactor(select(EA004, -all_of(num_vars_cols2drop)))
names(EA004_s)[names(EA004_s) == "code"] <- "Husbandry"

EA005_s <- unfactor(select(EA005, -all_of(num_vars_cols2drop)))
names(EA005_s)[names(EA005_s) == "code"] <- "Agriculture"

# Agriculture-related variables: EA028, EA029, EA039
# Variables from EA028 to EA039 have been considered as NUMERICAL CONTINUOUS variables
# in order not to lose information about order. Consequently, we have dropped code_label col,
# which is a factor (char)

EA028_s <- unfactor(select(EA028, -all_of(num_vars_cols2drop)))
names(EA028_s)[names(EA028_s) == "code"] <- "Agriculture_intensity"

EA029_s <- unfactor(select(EA029, -all_of(num_vars_cols2drop)))
names(EA029_s)[names(EA029_s) == "code"] <- "Major_crop_type"

EA039_s <- unfactor(select(EA039, -all_of(num_vars_cols2drop)))
names(EA039_s)[names(EA039_s) == "code"] <- "Plow_cultivation"

# EA Variables considered independently

# EA030: Prevailing type settlement pattern --> NUMERICAL
EA030_s <- unfactor(select(EA030, -all_of(num_vars_cols2drop)))
names(EA030_s)[names(EA030_s) == "code"] <- "Settlement_pattern"

# EA033: Political complexity --> NUMERICAL
EA033_s <- unfactor(select(EA033, -all_of(num_vars_cols2drop)))
names(EA033_s)[names(EA033_s) == "code"] <- "Political_complexity"

# EA202: Population size. Considered numerical & continuous 
EA202_s <- unfactor(select(EA202, -all_of(num_vars_cols2drop)))
names(EA202_s)[names(EA202_s) == "code"] <- "Population_size"
```

##### Preprocessing of EA CATEGORICAL variables

``` r
# EA040: Domestic animals type: considered as FACTOR
EA040_s <- select(EA040, -all_of(cat_vars_cols2drop))
names(EA040_s)[names(EA040_s) == "code_label"] <- "Animal_type"
EA040_s$society_id <- as.character(EA040_s$society_id)

# EA041: Milking: considered as FACTOR
EA041_s <- select(EA041, -all_of(cat_vars_cols2drop))
names(EA041_s)[names(EA041_s) == "code_label"] <- "Milking"
EA041_s$society_id <- as.character(EA041_s$society_id)
```

##### Build unified\_EA\_db: Include attribute all = T, so that it does not drop registers with NAs

``` r
## Inclusion of all = T
EA_unified_01 = merge(EA001_s, EA002_s, by.x="society_id", all = T)
EA_unified_02 = merge(EA_unified_01, EA003_s, by.x="society_id", all = T)
EA_unified_03 = merge(EA_unified_02, EA004_s, by.x="society_id", all = T)
EA_unified_04 = merge(EA_unified_03, EA005_s, by.x="society_id", all = T)
EA_unified_05 = merge(EA_unified_04, EA028_s, by.x="society_id", all = T)
EA_unified_06 = merge(EA_unified_05, EA029_s, by.x="society_id", all = T)
EA_unified_07 = merge(EA_unified_06, EA030_s, by.x="society_id", all = T)
EA_unified_08 = merge(EA_unified_07, EA033_s, by.x="society_id", all = T)
EA_unified_09 = merge(EA_unified_08, EA039_s, by.x="society_id", all = T)
EA_unified_10 = merge(EA_unified_09, EA040_s, by.x="society_id", all = T)
EA_unified_11 = merge(EA_unified_10, EA041_s, by.x="society_id", all = T)
EA_unified_12 = merge(EA_unified_11, EA202_s, by.x="society_id", all = T)
```

#### 2\. Variables from Jenkins et al. 2013: all these varibles are NUMERICAL

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/Jenkins"))
# 2. Variables from Jenkins et al. 2013:
# - Amphibian Richness
# - Bird Richness
# - Mammal Richness
Amphibians = read.csv("AmphibianRichness.csv", header = T, sep = ",", fileEncoding = "UTF-8")
Birds = read.csv("BirdRichness.csv", header = T, sep = ",", fileEncoding = "UTF-8")
Mammals = read.csv("MammalRichness.csv", header = T, sep = ",", fileEncoding = "UTF-8")
```

##### Check Jenkins et al. 2013 dimensionality coherence

``` r
cat("Dimensions of dataset Amphibians: ", dim(Amphibians))
```

    ## Dimensions of dataset Amphibians:  1793 12

``` r
cat("\nDimensions of dataset Birds: ", dim(Birds))
```

    ## 
    ## Dimensions of dataset Birds:  1901 12

``` r
cat("\nDimensions of dataset Mammals: ", dim(Mammals))
```

    ## 
    ## Dimensions of dataset Mammals:  1897 12

##### Drop unnecessary columns from Jenkins et al.

##### Check structure

``` r
str(Amphibians)
```

    ## 'data.frame':    1793 obs. of  12 variables:
    ##  $ society_id         : Factor w/ 1793 levels "Aa1","Aa2","Aa3",..: 564 578 589 594 633 634 636 638 639 640 ...
    ##  $ society_name       : Factor w/ 1331 levels "!Ko","!Kung",..: 1313 1227 405 627 797 413 599 288 290 812 ...
    ##  $ society_xd_id      : Factor w/ 1294 levels "xd1","xd10","xd100",..: 981 969 59 4 10 11 13 58 28 23 ...
    ##  $ language_glottocode: Factor w/ 1200 levels "aari1239","abip1241",..: 744 174 753 527 755 358 509 248 755 1098 ...
    ##  $ language_name      : Factor w/ 1200 levels "//Xegwi","/Gwi",..: 768 183 755 333 748 363 512 252 748 1095 ...
    ##  $ language_family    : Factor w/ 144 levels "","Abkhaz-Adyge",..: 142 37 45 37 9 9 9 9 9 9 ...
    ##  $ variable_id        : Factor w/ 1 level "AmphibianRichness": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code               : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code_label         : logi  NA NA NA NA NA NA ...
    ##  $ focal_year         : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
    ##  $ sub_case           : logi  NA NA NA NA NA NA ...
    ##  $ comment            : logi  NA NA NA NA NA NA ...

##### The columns to drop are the same as in the EA numeric variables

``` r
Amphibians_s <- unfactor(select(Amphibians, -all_of(num_vars_cols2drop)))
names(Amphibians_s)[names(Amphibians_s) == "code"] <- "Amphibian_richness"

Birds_s <- unfactor(select(Birds, -all_of(num_vars_cols2drop)))
names(Birds_s)[names(Birds_s) == "code"] <- "Bird_richness"

Mammals_s <- unfactor(select(Mammals, -all_of(num_vars_cols2drop)))
names(Mammals_s)[names(Mammals_s) == "code"] <- "Mammal_richness"
```

##### Check how many of our societies from EA are included in Jenkins

``` r
sum(EA_unified_12$society_id %in% Amphibians_s$society_id)
```

    ## [1] 1159

``` r
sum(EA_unified_12$society_id %in% Birds_s$society_id)
```

    ## [1] 1226

``` r
sum(EA_unified_12$society_id %in% Mammals_s$society_id)
```

    ## [1] 1225

##### Merge EA and variables from Jenkins et al. 2013:

##### Left join

``` r
EA_Jenkins_01 = merge(EA_unified_12, Amphibians_s, by = "society_id", all.x = T)
EA_Jenkins_02 = merge(EA_Jenkins_01, Birds_s, by = "society_id", all.x = T)
EA_Jenkins_03 = merge(EA_Jenkins_02, Mammals_s, by = "society_id", all.x = T)
```

#### 3\. Variables from Kreft and Jetz: NUMERICAL

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/Kreft and Jetz"))
# 3. Variables from Kreft and Jetz
# - Vascular plants Richness
Vascular_plants = read.csv("VascularPlantsRichness.csv", header = T, sep = ",", fileEncoding = "UTF-8")
```

##### Check dimensions

``` r
cat("Dimensions of dataset VascularPlants: ", dim(Vascular_plants))
```

    ## Dimensions of dataset VascularPlants:  1836 12

##### Drop unnecessary columns from Kreft and Jetz

##### Check structure

``` r
str(Vascular_plants)
```

    ## 'data.frame':    1836 obs. of  12 variables:
    ##  $ society_id         : Factor w/ 1836 levels "Aa1","Aa2","Aa3",..: 1118 823 673 1127 818 672 1138 815 821 1135 ...
    ##  $ society_name       : Factor w/ 1350 levels "!Ko","!Kung",..: 463 129 102 102 1344 519 519 634 555 469 ...
    ##  $ society_xd_id      : Factor w/ 1313 levels "xd1","xd10","xd100",..: 1202 870 33 33 863 44 44 860 868 28 ...
    ##  $ language_glottocode: Factor w/ 1213 levels "aari1239","abab1239",..: 788 367 71 71 1208 442 442 367 1056 1156 ...
    ##  $ language_name      : Factor w/ 1213 levels "//Xegwi","/Gwi",..: 784 370 748 748 1207 446 446 370 1047 1144 ...
    ##  $ language_family    : Factor w/ 142 levels "","Abkhaz-Adyge",..: 12 3 36 36 3 36 36 3 3 36 ...
    ##  $ variable_id        : Factor w/ 1 level "VascularPlantsRichness": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code               : num  2.75 284.42 291.44 291.44 330.43 ...
    ##  $ code_label         : logi  NA NA NA NA NA NA ...
    ##  $ focal_year         : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
    ##  $ sub_case           : logi  NA NA NA NA NA NA ...
    ##  $ comment            : logi  NA NA NA NA NA NA ...

##### The columns to drop are the same as in the EA numeric variables

``` r
Vascular_plants_s <- unfactor(select(Vascular_plants, -all_of(num_vars_cols2drop)))
names(Vascular_plants_s)[names(Vascular_plants_s) == "code"] <- "Plants_richness"
```

##### Merge with EA\_Jenkins\_03

##### Left join

``` r
EA_Kreft = merge(EA_Jenkins_03, Vascular_plants_s, by = "society_id", all.x = T)
```

#### 4\. Variables from Moderate Resolution Imaging Spectroradiometer: NUMERICAL

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/Spectroradiometer"))
# 4. Variables from Moderate Resolution Imaging Spectroradiometer
# - Annual NPP Variance
# - Monthly mean NPP
# - NPP Constancy
# - NPP Contingency
# - NPP Predictability

Annual_NPP_Var = read.csv("AnnualNetPrimaryProductionVariance.csv", header = T, sep = ",", 
                          fileEncoding = "UTF-8")
Monthly_mean_NPP = read.csv("MonthlyMeanNetPrimaryProduction.csv", header = T, sep = ",", 
                            fileEncoding = "UTF-8")
NPP_Constancy = read.csv("NetPrimaryProductionConstancy.csv", header = T, sep = ",", 
                         fileEncoding = "UTF-8")
NPP_Contingency = read.csv("NetPrimaryProductionContingency.csv", header = T, sep = ",", 
                           fileEncoding = "UTF-8")
NPP_Predictability = read.csv("NetPrimaryProductionPredictability.csv", header = T, sep = ",",
                              fileEncoding = "UTF-8")
```

##### Check Moderate Resolution Imaging Spectroradiometer dimensionality coherence

``` r
cat("Dimensions of dataset Annual_NPP_Variance: ", dim(Annual_NPP_Var))
```

    ## Dimensions of dataset Annual_NPP_Variance:  1977 12

``` r
cat("\nDimensions of dataset Monthly_mean_NPP: ", dim(Monthly_mean_NPP))
```

    ## 
    ## Dimensions of dataset Monthly_mean_NPP:  1977 12

``` r
cat("\nDimensions of dataset NPP_Constancy: ", dim(NPP_Constancy))
```

    ## 
    ## Dimensions of dataset NPP_Constancy:  1969 12

``` r
cat("\nDimensions of dataset NPP_Contingency: ", dim(NPP_Contingency))
```

    ## 
    ## Dimensions of dataset NPP_Contingency:  1969 12

``` r
cat("\nDimensions of dataset NPP_Predictability: ", dim(NPP_Predictability))
```

    ## 
    ## Dimensions of dataset NPP_Predictability:  1969 12

##### Drop unnecessary columns from Resolution Imaging Spectroradiometer variables

##### Check structure

``` r
str(Annual_NPP_Var)
```

    ## 'data.frame':    1977 obs. of  12 variables:
    ##  $ society_id         : Factor w/ 1977 levels "Aa1","Aa2","Aa3",..: 315 792 826 828 831 834 835 839 841 843 ...
    ##  $ society_name       : Factor w/ 1456 levels "!Ko","!Kung",..: 1260 180 1068 99 316 319 677 1170 594 136 ...
    ##  $ society_xd_id      : Factor w/ 1418 levels "xd1","xd10","xd100",..: 678 831 868 870 874 877 878 884 886 888 ...
    ##  $ language_glottocode: Factor w/ 1313 levels "aari1239","abab1239",..: 1138 132 388 351 268 388 388 1033 1142 388 ...
    ##  $ language_name      : Factor w/ 1313 levels "//Xegwi","/Gwi",..: 1130 133 392 356 268 392 392 1025 1134 392 ...
    ##  $ language_family    : Factor w/ 147 levels "","Abkhaz-Adyge",..: 10 3 3 3 107 3 3 3 3 3 ...
    ##  $ variable_id        : Factor w/ 1 level "AnnualNetPrimaryProductionVariance": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ code_label         : logi  NA NA NA NA NA NA ...
    ##  $ focal_year         : logi  NA NA NA NA NA NA ...
    ##  $ sub_case           : logi  NA NA NA NA NA NA ...
    ##  $ comment            : Factor w/ 5 levels "","Note, estimate is from a point 101 km away (coordinates: 7.36,143.55); data were not available at society's reported location",..: 1 1 1 1 1 1 1 1 1 1 ...

##### The columns to drop are the same as in the EA numeric variables

``` r
Annual_NPP_Var_s <- unfactor(select(Annual_NPP_Var, -all_of(num_vars_cols2drop)))
names(Annual_NPP_Var_s)[names(Annual_NPP_Var_s) == "code"] <- "Annual_NPP_Var"

Monthly_mean_NPP_s <- unfactor(select(Monthly_mean_NPP, -all_of(num_vars_cols2drop)))
names(Monthly_mean_NPP_s)[names(Monthly_mean_NPP_s) == "code"] <- "Monthly_mean_NPP"

NPP_Constancy_s <- unfactor(select(NPP_Constancy, -all_of(num_vars_cols2drop)))
names(NPP_Constancy_s)[names(NPP_Constancy_s) == "code"] <- "NPP_Constancy"

NPP_Contingency_s <- unfactor(select(NPP_Contingency, -all_of(num_vars_cols2drop)))
names(NPP_Contingency_s)[names(NPP_Contingency_s) == "code"] <- "NPP_Contingency"

NPP_Predictability_s <- unfactor(select(NPP_Predictability, -all_of(num_vars_cols2drop)))
names(NPP_Predictability_s)[names(NPP_Predictability_s) == "code"] <- "NPP_Predictability"
```

##### Merge variables from Resolution Imaging Spectroradiometer

##### Left join

``` r
EA_RIS_01 = merge(EA_Kreft, Annual_NPP_Var_s, by = "society_id", all.x = T)
EA_RIS_02 = merge(EA_RIS_01, Monthly_mean_NPP_s, by = "society_id", all.x = T)
EA_RIS_03 = merge(EA_RIS_02, NPP_Constancy_s, by = "society_id", all.x = T)
EA_RIS_04 = merge(EA_RIS_03, NPP_Contingency_s, by = "society_id", all.x = T)
EA_RIS_05 = merge(EA_RIS_04, NPP_Predictability_s, by = "society_id", all.x = T)
```

#### 5\. Variables from Terrestrial Ecoregions of the World

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/EcoRegions"))
# 5. Variables from Terrestrial Ecoregions of the World
# - Biome
Biome = read.csv("Biome.csv", header = T, sep = ",", fileEncoding = "UTF-8")
```

##### Check Biome dimensions

``` r
cat("Dimensions of Biome: ", dim(Biome))
```

    ## Dimensions of Biome:  1987 12

##### Drop unnecessary columns from Biome

##### Check structure

``` r
str(Biome)
```

    ## 'data.frame':    1987 obs. of  12 variables:
    ##  $ society_id         : Factor w/ 1987 levels "Aa1","Aa2","Aa3",..: 5 11 12 17 21 25 40 47 68 74 ...
    ##  $ society_name       : Factor w/ 1465 levels "!Ko","!Kung",..: 820 1048 1417 719 1216 279 847 347 776 1444 ...
    ##  $ society_xd_id      : Factor w/ 1427 levels "xd1","xd10","xd100",..: 882 111 219 516 561 605 772 849 1081 1148 ...
    ##  $ language_glottocode: Factor w/ 1322 levels "aari1239","abab1239",..: 127 741 1279 607 1098 248 1176 279 658 1306 ...
    ##  $ language_name      : Factor w/ 1322 levels "//Xegwi","/Gwi",..: 127 744 1276 609 1090 251 1172 279 660 1304 ...
    ##  $ language_family    : Factor w/ 147 levels "","Abkhaz-Adyge",..: 10 10 10 10 10 10 10 10 10 10 ...
    ##  $ variable_id        : Factor w/ 1 level "Biome": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code_label         : Factor w/ 16 levels "Boreal Forests/Taiga",..: 15 15 15 15 15 15 15 15 15 15 ...
    ##  $ focal_year         : int  1988 1988 1988 1988 1988 1988 1988 1988 1988 1988 ...
    ##  $ sub_case           : logi  NA NA NA NA NA NA ...
    ##  $ comment            : logi  NA NA NA NA NA NA ...

##### This variable is a FACTOR, we keep code\_label and drop the same cols. as in the EA categorical variables

``` r
Biome_s <- select(Biome, -all_of(cat_vars_cols2drop))
names(Biome_s)[names(Biome_s) == "code_label"] <- "Biome"
Biome_s$society_id <- as.character(Biome_s$society_id)
```

##### Merge with EA\_RIS\_05

##### Left join

``` r
EA_Biome = merge(EA_RIS_05, Biome_s, by = "society_id", all.x = T)
```

#### 6\. Variables from Baseline Historical ecoClimate model: NUMERICAL

``` r
# Set the working directory to the corresponding subfolder within the current working directory
setwd(paste0(getwd(), "/Supervised Learning/EcoClimate"))
# 6. Variables from Baseline Historical ecoClimate model
# - Annual Mean Temperature
# - Annual Precipitation Variance
# - Annual Temperature Variance
# - Monthly Mean Precipitation
# - Precipitation Constancy
# - Precipitation Contingency
# - Precipitation Predictability
# - Temperature Constancy
# - Temperature Contingency
# - Temperature Predictability

Annual_Mean_Temp = read.csv("AnnualMeanTemperature.csv", header = T, sep = ",", fileEncoding = "UTF-8")
Annual_Precip_Var = read.csv("AnnualPrecipitationVariance.csv", header = T, sep = ",")
Annual_Temp_Var = read.csv("AnnualTemperatureVariance.csv", header = T, sep = ",")
Monthly_Mean_Precip = read.csv("MonthlyMeanPrecipitation.csv", header = T, sep = ",")
Precip_Constancy = read.csv("PrecipitationConstancy.csv", header = T, sep = ",")
Precip_Contingency = read.csv("PrecipitationContingency.csv", header = T, sep = ",")
Precip_Predictability = read.csv("PrecipitationPredictability.csv", header = T, sep = ",")
Temp_Constancy = read.csv("TemperatureConstancy.csv", header = T, sep = ",")
Temp_Contingency = read.csv("TemperatureContingency.csv", header = T, sep = ",")
Temp_Predictability = read.csv("TemperaturePredictability.csv", header = T, sep = ",")
```

##### Check Baseline Historical ecoClimate model variables dimensionality coherence

``` r
cat("Dimensions of dataset Annual_Mean_Temp: ", dim(Annual_Mean_Temp))
```

    ## Dimensions of dataset Annual_Mean_Temp:  1988 12

``` r
cat("\nDimensions of dataset Annual_Precip_Var: ", dim(Annual_Precip_Var))
```

    ## 
    ## Dimensions of dataset Annual_Precip_Var:  1988 12

``` r
cat("\nDimensions of dataset Annual_Temp_Var: ", dim(Annual_Temp_Var))
```

    ## 
    ## Dimensions of dataset Annual_Temp_Var:  1988 12

``` r
cat("\nDimensions of dataset Monthly_Mean_Precip: ", dim(Monthly_Mean_Precip))
```

    ## 
    ## Dimensions of dataset Monthly_Mean_Precip:  1988 12

``` r
cat("\nDimensions of dataset Precip_Constancy: ", dim(Precip_Constancy))
```

    ## 
    ## Dimensions of dataset Precip_Constancy:  1988 12

``` r
cat("\nDimensions of dataset Precip_Contingency: ", dim(Precip_Contingency))
```

    ## 
    ## Dimensions of dataset Precip_Contingency:  1988 12

``` r
cat("\nDimensions of dataset Precip_Predictability: ", dim(Precip_Predictability))
```

    ## 
    ## Dimensions of dataset Precip_Predictability:  1988 12

``` r
cat("\nDimensions of dataset Temp_Constancy: ", dim(Temp_Constancy))
```

    ## 
    ## Dimensions of dataset Temp_Constancy:  1988 12

``` r
cat("\nDimensions of dataset Temp_Contingency: ", dim(Temp_Contingency))
```

    ## 
    ## Dimensions of dataset Temp_Contingency:  1988 12

``` r
cat("\nDimensions of dataset Temp_Predictability: ", dim(Temp_Predictability))
```

    ## 
    ## Dimensions of dataset Temp_Predictability:  1988 12

##### Drop unnecessary columns from BH ecoClimate model

##### Check structure

``` r
str(Annual_Mean_Temp)
```

    ## 'data.frame':    1988 obs. of  12 variables:
    ##  $ society_id         : Factor w/ 1988 levels "Aa1","Aa2","Aa3",..: 689 1251 686 693 1243 1274 584 580 973 685 ...
    ##  $ society_name       : Factor w/ 1466 levels "!Ko","!Kung",..: 922 500 500 509 509 922 927 1448 1448 31 ...
    ##  $ society_xd_id      : Factor w/ 1428 levels "xd1","xd10","xd100",..: 68 29 29 36 36 68 1017 1032 1032 28 ...
    ##  $ language_glottocode: Factor w/ 1322 levels "aari1239","abab1239",..: 782 1259 1259 926 926 782 789 809 809 175 ...
    ##  $ language_name      : Factor w/ 1322 levels "//Xegwi","/Gwi",..: 775 1247 1247 919 919 775 791 839 839 531 ...
    ##  $ language_family    : Factor w/ 147 levels "","Abkhaz-Adyge",..: 38 38 38 38 38 38 135 145 145 38 ...
    ##  $ variable_id        : Factor w/ 1 level "AnnualMeanTemperature": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ code               : num  -19.5 -19.3 -18.7 -18.4 -18.3 ...
    ##  $ code_label         : logi  NA NA NA NA NA NA ...
    ##  $ focal_year         : logi  NA NA NA NA NA NA ...
    ##  $ sub_case           : logi  NA NA NA NA NA NA ...
    ##  $ comment            : logi  NA NA NA NA NA NA ...

##### The columns to drop are the same as in the EA numeric variables

``` r
Annual_Mean_Temp_s <- unfactor(select(Annual_Mean_Temp, -all_of(num_vars_cols2drop)))
names(Annual_Mean_Temp_s)[names(Annual_Mean_Temp_s) == "code"] <- "Annual_Mean_Temp"

Annual_Precip_Var_s <- unfactor(select(Annual_Precip_Var, -all_of(num_vars_cols2drop)))
names(Annual_Precip_Var_s)[names(Annual_Precip_Var_s) == "code"] <- "Annual_Precip_Var"

Annual_Temp_Var_s <- unfactor(select(Annual_Temp_Var, -all_of(num_vars_cols2drop)))
names(Annual_Temp_Var_s)[names(Annual_Temp_Var_s) == "code"] <- "Annual_Temp_Var"

Monthly_Mean_Precip_s <- unfactor(select(Monthly_Mean_Precip, -all_of(num_vars_cols2drop)))
names(Monthly_Mean_Precip_s)[names(Monthly_Mean_Precip_s) == "code"] <- "Monthly_Mean_Precip"

Precip_Constancy_s <- unfactor(select(Precip_Constancy, -all_of(num_vars_cols2drop)))
names(Precip_Constancy_s)[names(Precip_Constancy_s) == "code"] <- "Precip_Constancy"

Precip_Contingency_s <- unfactor(select(Precip_Contingency, -all_of(num_vars_cols2drop)))
names(Precip_Contingency_s)[names(Precip_Contingency_s) == "code"] <- "Precip_Contingency"

Precip_Predictability_s <- unfactor(select(Precip_Predictability, -all_of(num_vars_cols2drop)))
names(Precip_Predictability_s)[names(Precip_Predictability_s) == "code"] <- "Precip_Predictability"

Temp_Constancy_s <- unfactor(select(Temp_Constancy, -all_of(num_vars_cols2drop)))
names(Temp_Constancy_s)[names(Temp_Constancy_s) == "code"] <- "Temp_Constancy"

Temp_Contingency_s <- unfactor(select(Temp_Contingency, -all_of(num_vars_cols2drop)))
names(Temp_Contingency_s)[names(Temp_Contingency_s) == "code"] <- "Temp_Contingency"

Temp_Predictability_s <- unfactor(select(Temp_Predictability, -all_of(num_vars_cols2drop)))
names(Temp_Predictability_s)[names(Temp_Predictability_s) == "code"] <- "Temp_Predictability"
```

##### Merge with EA\_Biome

##### Left join

``` r
EA_ecoClimate_01 = merge(EA_Biome, Annual_Mean_Temp_s, by = "society_id", all.x = T)
EA_ecoClimate_02 = merge(EA_ecoClimate_01, Annual_Precip_Var_s, by = "society_id", all.x = T)
EA_ecoClimate_03 = merge(EA_ecoClimate_02, Annual_Temp_Var_s, by = "society_id", all.x = T)
EA_ecoClimate_04 = merge(EA_ecoClimate_03, Monthly_Mean_Precip_s, by = "society_id", all.x = T)
EA_ecoClimate_05 = merge(EA_ecoClimate_04, Precip_Constancy_s, by = "society_id", all.x = T)
EA_ecoClimate_06 = merge(EA_ecoClimate_05, Precip_Contingency_s, by = "society_id", all.x = T)
EA_ecoClimate_07 = merge(EA_ecoClimate_06, Precip_Predictability_s, by = "society_id", all.x = T)
EA_ecoClimate_08 = merge(EA_ecoClimate_07, Temp_Constancy_s, by = "society_id", all.x = T)
EA_ecoClimate_09 = merge(EA_ecoClimate_08, Temp_Contingency_s, by = "society_id", all.x = T)
EA_ecoClimate_final = merge(EA_ecoClimate_09, Temp_Predictability_s, by = "society_id", all.x = T)
```

## Save MICE database

``` r
save(EA_ecoClimate_final, file = "Database_for_MICE.Rda")
write.csv(EA_ecoClimate_final, file = "Database_for_MICE.csv")
```
