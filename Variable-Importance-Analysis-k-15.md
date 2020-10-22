Variable Importance Analysis for k = 15
================
Virginia\_Ahedo
20/10/2020

#### Setup

#### Clean workspace

#### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

#### Parallel execution

``` r
require(doParallel)
```

    ## Loading required package: doParallel

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
cl <- makeCluster(6)
registerDoParallel(cl)
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
require(randomForestSRC)
```

    ## Loading required package: randomForestSRC

    ## 
    ##  randomForestSRC 2.9.3 
    ##  
    ##  Type rfsrc.news() to see new features, changes, and bug fixes. 
    ## 

``` r
require(RWeka)
```

    ## Loading required package: RWeka

#### RandomForestSRC: rfsrc function details

##### <https://cran.r-project.org/web/packages/randomForestSRC/randomForestSRC.pdf>

``` r
#rfsrc function arguments:
# - data: Data frame containing the y-outcome and x-variables
# - ntree: number of trees. ntree should be set fairly high - we recommend using 1000 times the number of features
# - mtry: default: sqrt(p) for classification
# - importance: To obtain VIMP use the option importance. Setting this to "permute" returns permutation VIMP from permuting OOB cases. 
# - VIMP depends upon block.size. When block.size=1, VIMP is calculated by tree. The difference between prediction error under the perturbed predictor and the original predictor is calculated for each tree and averaged over the forest. This yields Breiman-Cutler VIMP (Breiman 2001).
# - block.size: default: 10.
# - bootstrap:  Bootstrap protocol. The default is by.root which bootstraps the data by sampling with or without replacement (by default sampling is without replacement; see the option samptype below).
# - samptype: Type of bootstrap when by.root is in effect. Unlike Breiman's random forests, the default action here is sampling without replacement. Thus out-of-bag (OOB) technically means out-of-sample. Choice: "swr" sampling with replacement 
# - sampsize: For sampling without replacement it is 0.632 the sample size.
# -forest: vimp.rfsrc requires forest = T in the original call
```

#### Variable importance analyses on an example dataset

##### Set seed

``` r
set.seed(7)
```

##### Load example dataset

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing/Multiple_Imputation_4_Weka_K15/")
example_dataset = read.arff(file = "MICE_imputed_df_K15_1.arff")
```

##### Check dimensions of example dataset

``` r
dim(example_dataset)
```

    ## [1] 1290   29

##### Check variables included in example\_dataset

``` r
names(example_dataset)
```

    ##  [1] "Agriculture_intensity" "Major_crop_type"       "Settlement_pattern"   
    ##  [4] "Political_complexity"  "Plow_cultivation"      "Animal_type"          
    ##  [7] "Milking"               "Population_size"       "Amphibian_richness"   
    ## [10] "Bird_richness"         "Mammal_richness"       "Plants_richness"      
    ## [13] "Annual_NPP_Var"        "Monthly_mean_NPP"      "NPP_Constancy"        
    ## [16] "NPP_Contingency"       "NPP_Predictability"    "Biome"                
    ## [19] "Annual_Mean_Temp"      "Annual_Precip_Var"     "Annual_Temp_Var"      
    ## [22] "Monthly_Mean_Precip"   "Precip_Constancy"      "Precip_Contingency"   
    ## [25] "Precip_Predictability" "Temp_Constancy"        "Temp_Contingency"     
    ## [28] "Temp_Predictability"   "cluster"

##### Note that “Gathering”, “Hunting”, “Fishing”, “Husbandry”, “Agriculture” have already been dropped and cluster has already been included.

##### RF model for the example\_dataset

``` r
# Random Forest with RandomForestSRC:
# - formula: symbolic description of the model to be fit.
# - data: imputedDataSet
# - ntree: 3000.
# - importance = "permute" (permutation VIMP from permuting OOB cases)
# - block.size = 1 (Difference between prediction error under the perturbed predictor and the original is
#   calculated for each tree and averaged over the forest. This yields Breiman 2001)
# - bootstrap = "by.root" (default)
# - samptype = "swr" sampling with replacement (Type of bootstrap when "by.root" is in effect)
# - forest = T (vimp.rfsrc requires forest = T in the original call)

rf_example = rfsrc(formula = cluster ~ ., data = example_dataset, ntree = 3000, importance = "permute",
                   block.size = 1, bootstrap = "by.root", samptype = "swr", forest = T)
```

##### Individual variable importance on example\_dataset

``` r
var_imp_example = vimp.rfsrc(rf_example)$importance
dim(var_imp_example)
```

    ## [1] 28 16

##### Show var\_imp\_example

``` r
var_imp_example
```

    ##                               all            1             2             3
    ## Agriculture_intensity 0.053961928  0.065601201  0.4094280304  0.1038523058
    ## Major_crop_type       0.026210028 -0.058896106  0.2287360407  0.0167278882
    ## Settlement_pattern    0.044337021  0.431663154  0.2603439689  0.1661172229
    ## Political_complexity  0.005069610  0.056721481  0.0055840673  0.0009293271
    ## Plow_cultivation      0.001698839  0.002718282  0.0009482378 -0.0002323318
    ## Animal_type           0.067269752  0.305897315  0.2534955845  0.0908417261
    ## Milking               0.036180671  0.107100304  0.0807055768  0.1486923393
    ## Population_size       0.014490963  0.124859745  0.1413927990 -0.0211421920
    ## Amphibian_richness    0.010878673  0.011598002  0.0250756231  0.0306677950
    ## Bird_richness         0.018887580  0.082816986  0.0073751833  0.0283444772
    ## Mammal_richness       0.011111672  0.010873127  0.0352955199  0.0439107065
    ## Plants_richness       0.013409727  0.040593009  0.0244434645  0.0803867960
    ## Annual_NPP_Var        0.007626337  0.167808598 -0.0135914091  0.0592446040
    ## Monthly_mean_NPP      0.011702242  0.136638967 -0.0015803964  0.0634265760
    ## NPP_Constancy         0.007075943  0.117248556 -0.0156986044  0.0576182815
    ## NPP_Contingency       0.008905737  0.089522082 -0.0121163725  0.0669115527
    ## NPP_Predictability    0.008371373  0.052372230  0.0148557263  0.0264858229
    ## Biome                 0.009278474  0.070131671  0.0009482378  0.0157985611
    ## Annual_Mean_Temp      0.016727530  0.051828574  0.0476226119  0.0341527717
    ## Annual_Precip_Var     0.007436920  0.032075726  0.0367705565  0.0097579348
    ## Annual_Temp_Var       0.010440157  0.054909293  0.0303436111  0.0225361827
    ## Monthly_Mean_Precip   0.014744642  0.136095310  0.0465690143  0.0227685145
    ## Precip_Constancy      0.012880959  0.072849953  0.0405635079  0.0313647903
    ## Precip_Contingency    0.010099001  0.065963639  0.0288685745  0.0292738043
    ## Precip_Predictability 0.017857448  0.302635377  0.0698535214  0.0116165890
    ## Temp_Constancy        0.011494894  0.057265137  0.0232845071  0.0304354632
    ## Temp_Contingency      0.008387108  0.075024578  0.0181218789  0.0188188742
    ## Temp_Predictability   0.024523212  0.113624180  0.0392991908  0.0641235713
    ##                                  4           5             6             7
    ## Agriculture_intensity  0.467015920 0.108804641  0.1003387637  0.0151790096
    ## Major_crop_type        0.160076597 0.040425730  0.0623125261 -0.0009293271
    ## Settlement_pattern     0.380936995 0.145048399  0.0458988571  0.0284219211
    ## Political_complexity  -0.003397852 0.044681070 -0.0213154887  0.0025556496
    ## Plow_cultivation       0.001510157 0.008730784  0.0109919593  0.0044143038
    ## Animal_type            0.337897533 0.287822392  0.3255253895 -0.0007744393
    ## Milking                0.097405099 0.012619284  0.5036991082 -0.0046466356
    ## Population_size       -0.022274809 0.049449985  0.0210184087  0.0057308506
    ## Amphibian_richness     0.014724027 0.048716306  0.0405514174 -0.0135526872
    ## Bird_richness          0.066446889 0.049449985  0.0672143458 -0.0076669487
    ## Mammal_richness        0.074375211 0.027843130  0.0424081673 -0.0058857384
    ## Plants_richness        0.072487515 0.060271755  0.0351297078  0.0113068133
    ## Annual_NPP_Var        -0.005663087 0.034152772 -0.0019310199  0.0034075328
    ## Monthly_mean_NPP       0.041906845 0.051174132  0.0284454082 -0.0079767245
    ## NPP_Constancy         -0.004152931 0.028870281 -0.0004456200 -0.0008518832
    ## NPP_Contingency       -0.020009575 0.029860748  0.0077240795  0.0045691917
    ## NPP_Predictability     0.016234183 0.045818273  0.0205727887 -0.0037173085
    ## Biome                  0.075507829 0.028466757  0.0225038086  0.0072797291
    ## Annual_Mean_Temp      -0.002642774 0.036867385  0.0438935672  0.0116165890
    ## Annual_Precip_Var      0.085323846 0.045854957  0.0066100296  0.0060406263
    ## Annual_Temp_Var        0.102313108 0.037050805  0.0285196782  0.0015488785
    ## Monthly_Mean_Precip    0.115149439 0.077733322 -0.0009655099 -0.0016263225
    ## Precip_Constancy       0.049835167 0.081621823  0.0064614896  0.0063504020
    ## Precip_Contingency     0.052477941 0.047102211  0.0057187896  0.0024007617
    ## Precip_Predictability  0.056630871 0.067205024  0.0283711382  0.0006195514
    ## Temp_Constancy         0.102690647 0.040719201  0.0337185779 -0.0041045281
    ## Temp_Contingency       0.043417001 0.024981780  0.0233950485 -0.0058082945
    ## Temp_Predictability    0.158943979 0.063463260  0.0483497669 -0.0093707151
    ##                                 8             9            10            11
    ## Agriculture_intensity 0.099930279  0.1727397443  0.0808087863  0.0277868809
    ## Major_crop_type       0.063018091  0.0786389789  0.0517767967  0.0283909435
    ## Settlement_pattern    0.093914409  0.0782233395 -0.0070268510 -0.0042284384
    ## Political_complexity  0.010026449  0.0042395221  0.0053625968  0.0060406263
    ## Plow_cultivation      0.006350085  0.0012469183  0.0014793370 -0.0006040626
    ## Animal_type           0.077946360  0.1618499914  0.1667952523  0.0024162505
    ## Milking               0.036392298  0.0673335866  0.0545505537  0.0126853152
    ## Population_size       0.031824693  0.0244395981  0.0229297243  0.0114771899
    ## Amphibian_richness    0.031341938  0.0227770404  0.0318057466  0.0241625051
    ## Bird_richness         0.058673296  0.1334202549  0.0491879569  0.0271828183
    ## Mammal_richness       0.036095218  0.0397351289  0.0772953608  0.0253706304
    ## Plants_richness       0.005050360 -0.0103909856  0.0460443657  0.0356396951
    ## Annual_NPP_Var        0.016227994  0.0075646375  0.0512220453 -0.0030203131
    ## Monthly_mean_NPP      0.038880343  0.0066502308  0.0207107187  0.0193300041
    ## NPP_Constancy         0.014631189  0.0013300462  0.0484482884  0.0066446889
    ## NPP_Contingency       0.011103364  0.0169580885  0.0088760223 -0.0036243758
    ## NPP_Predictability    0.021761109  0.0004987673  0.0138687848 -0.0235584425
    ## Biome                 0.003416420  0.0466347433  0.0408666860  0.0253706304
    ## Annual_Mean_Temp      0.019978629  0.1662557693  0.0057324311  0.0132893778
    ## Annual_Precip_Var     0.008095429  0.0007481510  0.0075816024 -0.0054365637
    ## Annual_Temp_Var       0.026551523  0.0561944500  0.0234844757 -0.0018121879
    ## Monthly_Mean_Precip   0.054031422  0.0067333587  0.0007396685  0.0066446889
    ## Precip_Constancy      0.017304909  0.0347474558  0.0177520446  0.0060406263
    ## Precip_Contingency    0.020387114  0.0390701058  0.0136838677  0.0453046971
    ## Precip_Predictability 0.043299407  0.0194519250  0.0436404430 -0.0042284384
    ## Temp_Constancy        0.031230533  0.0636759597  0.0393873489 -0.0102690647
    ## Temp_Contingency      0.030562103  0.0265177952  0.0312509952  0.0102690647
    ## Temp_Predictability   0.079468895  0.1928566924  0.0085061880 -0.0084568768
    ##                                 12            13           14            15
    ## Agriculture_intensity  0.198179009  0.3010097378  0.443986032  0.0545505537
    ## Major_crop_type       -0.017889547  0.2410476386  0.275792344  0.0171972932
    ## Settlement_pattern     0.444683027 -0.0535661419  0.141294024  0.0061022653
    ## Political_complexity   0.053436309 -0.0015989893  0.007362013  0.0024039227
    ## Plow_cultivation      -0.010222598  0.0003997473  0.001982080  0.0040681769
    ## Animal_type            0.103619974  0.0115926725  0.284853283  0.3102909461
    ## Milking                0.435854420  0.0058629608  0.041906845  0.0674947529
    ## Population_size        0.033455776 -0.0149239002  0.104767112  0.0497427083
    ## Amphibian_richness     0.038799407 -0.0053299644  0.117792213  0.0225598900
    ## Bird_richness          0.017657215  0.0015989893  0.144125568  0.0301414924
    ## Mammal_richness        0.008363944 -0.0081281957  0.058046643  0.0242241442
    ## Plants_richness        0.016960220  0.0511676579  0.263050398  0.0325454151
    ## Annual_NPP_Var         0.048789674  0.0021319857  0.015290335 -0.0268129840
    ## Monthly_mean_NPP       0.042516716 -0.0010659929  0.056064563  0.0073966852
    ## NPP_Constancy          0.052971646 -0.0086611921  0.131666776 -0.0194162988
    ## NPP_Contingency       -0.026950487  0.1548354649  0.084096844 -0.0123894478
    ## NPP_Predictability     0.020909860 -0.0074619501  0.171025232 -0.0231146414
    ## Biome                  0.014404570  0.0517006544  0.068523354  0.0201559673
    ## Annual_Mean_Temp       0.003020313  0.0271828183  0.212082613  0.0419761888
    ## Annual_Precip_Var      0.037405417 -0.0149239002  0.043039462  0.0327303322
    ## Annual_Temp_Var        0.007899281 -0.0030647295  0.008777785  0.0129441992
    ## Monthly_Mean_Precip    0.055294964 -0.0111929252  0.022369194  0.0338398350
    ## Precip_Constancy      -0.002787981 -0.0067957046  0.144408722  0.0090609394
    ## Precip_Contingency     0.009990267 -0.0145241529  0.099387179  0.0223749729
    ## Precip_Predictability  0.026253491 -0.0161231422  0.107315501  0.0159028733
    ## Temp_Constancy         0.005808295  0.0119924198  0.008494631  0.0007396685
    ## Temp_Contingency       0.027879814  0.0053299644 -0.013025100  0.0125743649
    ## Temp_Predictability    0.011616589  0.0454379463  0.144975031  0.0077665195

###### The first col. (all) is the decrease in predictive accuracy averaged over all classes, the other 15 cols. are the decrease in predictive accuracy in each class. We will keep col. 1.

##### Group variable importance on example dataset: groups considered

``` r
# Group 1: Agriculture-related variables:
# - EA028: Agriculture intensity
# - EA029: Major crop type
# - EA039: Plow cultivation
group_1_varImp_ex = vimp.rfsrc(rf_example, c("Agriculture_intensity", "Major_crop_type", 
                                          "Plow_cultivation"), joint = TRUE)$importance[1]

# Group 2: Husbandry-related variables:
# - EA040: Domestic animals type
# - EA041: Milking
group_2_varImp_ex = vimp.rfsrc(rf_example, c("Animal_type", "Milking"), joint = TRUE)$importance[1]

# Group 3: Demographic/degree of complexity variables
# - EA030: Prevailing type settlement pattern
# - EA033: Political complexity
# - EA202: Population size
group_3_varImp_ex = vimp.rfsrc(rf_example, c("Settlement_pattern", "Political_complexity",
                                          "Population_size"), joint = TRUE)$importance[1]

# Group 4: Ecological variables: 2+3+4+5+6
# 2. Variables from Jenkins et al. 2013:
# - Amphibian Richness
# - Bird Richness
# - Mammal Richness
#
# 3. Variables from Kreft and Jetz
# - Vascular plants Richness
#
# 4. Variables from Moderate Resolution Imaging Spectroradiometer
# - Annual NPP Variance
# - Monthly mean NPP
# - NPP Constancy
# - NPP Contingency
# - NPP Predictability
#
# 5. Variables from Terrestrial Ecoregions of the World
# - Biome
#
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
group_4_varImp_ex = vimp.rfsrc(rf_example, names(example_dataset[,9:28]),
                               joint = TRUE)$importance[1]
```

#### Variable importance analyses on the 100 imputed datasets with K = 15

##### Definition of the necessary variables for individual variable importance

``` r
#28 predictors, 100 imputed datasets
ind_varImpMatrix <- data.frame(matrix(nrow = 28, ncol = 100)) #each dataset is a col.
row.names(ind_varImpMatrix) <- rownames(var_imp_example)
```

##### Definition of the necessary variables for group variable importance

``` r
group_varImpMatrix <- data.frame(matrix(nrow = 4, ncol = 100))
row.names(group_varImpMatrix) <- c("Agriculture-related", "Husbandry-related", "Complexity-degree",
                                   "Ecological")
```

##### Loop definition

``` r
# Change working directory
# setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing/Multiple_Imputation_4_Weka_K15/")
# 
# # Loop
# for(k in 1:100){
#   imputedDataSet <- read.arff(paste("MICE_imputed_df_K15_",k,".arff", sep = ""))
#   # Random Forest with RandomForestSRC:
#   # - formula: symbolic description of the model to be fit.
#   # - data: imputedDataSet
#   # - ntree: 3000.
#   # - importance = "permute" (permutation VIMP from permuting OOB cases)
#   # - block.size = 1 (Difference between prediction error under the perturbed predictor and the original is
#   #   calculated for each tree and averaged over the forest. This yields Breiman 2001)
#   # - bootstrap = "by.root" (default)
#   # - samptype = "swr" sampling with replacement (Type of bootstrap when "by.root" is in effect)
#   # - forest = T (vimp.rfsrc requires forest = T in the original call)
#   rf = rfsrc(formula = cluster ~ ., data = imputedDataSet, ntree = 3000, importance = "permute", block.size = 1,
#            bootstrap = "by.root", samptype = "swr", forest = T)
#   
#   # Individual variable importance
#   ind_varImpMatrix[,k] = data.frame(vimp.rfsrc(rf)$importance[,1])
#   
#   # Group variable importance
#   # Group 1: Agriculture-related variables:
#   group_1_varImp = vimp.rfsrc(rf, c("Agriculture_intensity", "Major_crop_type", "Plow_cultivation"),
#                             joint = TRUE)$importance[1]
#   # Group 2: Husbandry-related variables:
#   group_2_varImp = vimp.rfsrc(rf, c("Animal_type", "Milking"), joint = TRUE)$importance[1]
#   # Group 3: Demographic variables:
#   group_3_varImp = vimp.rfsrc(rf, c("Settlement_pattern", "Political_complexity", "Population_size"), 
#                               joint =TRUE)$importance[1]
#   # Group 4: Ecological variables: 2+3+4+5+6
#   group_4_varImp = vimp.rfsrc(rf, names(imputedDataSet[,9:28]), joint = TRUE)$importance[1]
#   group_varImpMatrix[,k] = data.frame(c(group_1_varImp, group_2_varImp, group_3_varImp, group_4_varImp))
#   }
```

##### Save the results obtained

``` r
# save(ind_varImpMatrix, file = "Individual_variable_importance_matrix_k15.Rda")
# save(group_varImpMatrix, file = "Group_variable_importance_matrix_k15.Rda")
```
