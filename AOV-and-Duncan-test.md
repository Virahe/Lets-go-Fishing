AOV & Duncan tests
================
Virginia\_Ahedo
19/10/2020

#### Setup

#### Clean workspace

#### Load required libraries

``` r
require(xlsx)
```

    ## Loading required package: xlsx

``` r
require(agricolae)
```

    ## Loading required package: agricolae

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.5
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
require(varhandle)
```

    ## Loading required package: varhandle

#### Load results experimenter K = 15, datasets = 1:50

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/Weka_experimenter")
results_k15_1_50 = read.csv("results_experimenter_classification_1_50.csv", header = T,
                            sep = ",", fileEncoding = "UTF-8")
```

#### Load results experimenter K = 15, datasets = 51:100

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/Weka_experimenter")
results_k15_51_100 = read.csv("results_experimenter_classification_test_Josema.csv", header = T,
                            sep = ",", fileEncoding = "UTF-8")
```

##### Check the structure of results\_k15\_1\_50

``` r
str(results_k15_1_50)
```

    ## 'data.frame':    3000 obs. of  68 variables:
    ##  $ Key_Dataset                      : Factor w/ 1 level "R_data_frame": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Run                          : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Fold                         : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Key_Scheme                       : Factor w/ 5 levels "weka.classifiers.meta.CVParameterSelection",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ Key_Scheme_options               : Factor w/ 6 levels "''","'-B 6'",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Scheme_version_ID            : num  4.81e+16 4.81e+16 4.81e+16 4.81e+16 4.81e+16 ...
    ##  $ Date_time                        : num  20201011 20201011 20201011 20201011 20201011 ...
    ##  $ Number_of_training_instances     : num  1161 1161 1161 1161 1161 ...
    ##  $ Number_of_testing_instances      : num  129 129 129 129 129 129 129 129 129 129 ...
    ##  $ Number_correct                   : num  25 24 24 24 25 25 25 25 25 25 ...
    ##  $ Number_incorrect                 : num  104 105 105 105 104 104 104 104 104 104 ...
    ##  $ Number_unclassified              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Percent_correct                  : num  19.4 18.6 18.6 18.6 19.4 ...
    ##  $ Percent_incorrect                : num  80.6 81.4 81.4 81.4 80.6 ...
    ##  $ Percent_unclassified             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Kappa_statistic                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Mean_absolute_error              : num  0.119 0.119 0.119 0.119 0.119 ...
    ##  $ Root_mean_squared_error          : num  0.244 0.244 0.244 0.244 0.243 ...
    ##  $ Relative_absolute_error          : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ Root_relative_squared_error      : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ SF_prior_entropy                 : num  452 455 453 453 449 ...
    ##  $ SF_scheme_entropy                : num  452 455 453 453 449 ...
    ##  $ SF_entropy_gain                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SF_mean_prior_entropy            : num  3.51 3.52 3.51 3.51 3.48 ...
    ##  $ SF_mean_scheme_entropy           : num  3.51 3.52 3.51 3.51 3.48 ...
    ##  $ SF_mean_entropy_gain             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_information                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_mean_information              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_relative_information          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ True_positive_rate               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Num_true_positives               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ False_positive_rate              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Num_false_positives              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ True_negative_rate               : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Num_true_negatives               : num  124 124 124 124 124 124 124 124 124 124 ...
    ##  $ False_negative_rate              : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Num_false_negatives              : num  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ IR_precision                     : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ IR_recall                        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ F_measure                        : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Matthews_correlation             : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Area_under_ROC                   : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ Area_under_PRC                   : num  0.0388 0.0388 0.0388 0.0388 0.0388 ...
    ##  $ Weighted_avg_true_positive_rate  : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_false_positive_rate : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_true_negative_rate  : num  0.806 0.814 0.814 0.814 0.806 ...
    ##  $ Weighted_avg_false_negative_rate : num  0.806 0.814 0.814 0.814 0.806 ...
    ##  $ Weighted_avg_IR_precision        : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Weighted_avg_IR_recall           : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_F_measure           : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Weighted_avg_matthews_correlation: num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Weighted_avg_area_under_ROC      : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ Weighted_avg_area_under_PRC      : num  0.111 0.108 0.109 0.109 0.112 ...
    ##  $ Unweighted_macro_avg_F_measure   : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
    ##  $ Unweighted_micro_avg_F_measure   : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Elapsed_Time_training            : num  0 0 0 0 0 0 0 0 0.016 0 ...
    ##  $ Elapsed_Time_testing             : num  0 0 0 0.007 0 0 0 0 0 0 ...
    ##  $ UserCPU_Time_training            : num  0 0 0 0 0 ...
    ##  $ UserCPU_Time_testing             : num  0 0 0 0.0156 0 ...
    ##  $ UserCPU_Time_millis_training     : num  0 0 0 0 0 ...
    ##  $ UserCPU_Time_millis_testing      : num  0 0 0 15.6 0 ...
    ##  $ Serialized_Model_Size            : num  1239 1239 1239 1239 1239 ...
    ##  $ Serialized_Train_Set_Size        : num  307112 307112 307112 307112 307112 ...
    ##  $ Serialized_Test_Set_Size         : num  37760 37760 37760 37760 37760 ...
    ##  $ Coverage_of_Test_Cases_By_Regions: num  96.9 96.9 96.9 96.9 97.7 ...
    ##  $ Size_of_Predicted_Regions        : num  86.7 86.7 86.7 86.7 86.7 ...
    ##  $ Summary                          : Factor w/ 11 levels "'Selected values: -C 10 -L 0.050050000000000004 -P 1.0E-12 -N 0 -V -1 -W 1 -K \\weka.classifiers.functions.supp"| __truncated__,..: 11 11 11 11 11 11 11 11 11 11 ...
    ##  $ measureOutOfBagError             : Factor w/ 2 levels "-1.0","?": 2 2 2 2 2 2 2 2 2 2 ...

##### Check dimensions of results\_k15\_1\_50

``` r
dim(results_k15_1_50)
```

    ## [1] 3000   68

##### The dimensions obtained make sense since we have performed 10-fold CV on 50 datasets and applied 6 algorithms (6*x*10\*50)

##### Check the structure of results\_k15\_51\_100

``` r
str(results_k15_51_100)
```

    ## 'data.frame':    3000 obs. of  68 variables:
    ##  $ Key_Dataset                      : Factor w/ 1 level "R_data_frame": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Run                          : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Fold                         : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Key_Scheme                       : Factor w/ 5 levels "weka.classifiers.meta.CVParameterSelection",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ Key_Scheme_options               : Factor w/ 6 levels "''","'-B 6'",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Key_Scheme_version_ID            : num  4.81e+16 4.81e+16 4.81e+16 4.81e+16 4.81e+16 ...
    ##  $ Date_time                        : num  20201011 20201011 20201011 20201011 20201011 ...
    ##  $ Number_of_training_instances     : num  1161 1161 1161 1161 1161 ...
    ##  $ Number_of_testing_instances      : num  129 129 129 129 129 129 129 129 129 129 ...
    ##  $ Number_correct                   : num  25 24 24 24 25 25 25 25 25 25 ...
    ##  $ Number_incorrect                 : num  104 105 105 105 104 104 104 104 104 104 ...
    ##  $ Number_unclassified              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Percent_correct                  : num  19.4 18.6 18.6 18.6 19.4 ...
    ##  $ Percent_incorrect                : num  80.6 81.4 81.4 81.4 80.6 ...
    ##  $ Percent_unclassified             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Kappa_statistic                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Mean_absolute_error              : num  0.119 0.119 0.119 0.119 0.119 ...
    ##  $ Root_mean_squared_error          : num  0.244 0.244 0.244 0.244 0.243 ...
    ##  $ Relative_absolute_error          : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ Root_relative_squared_error      : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ SF_prior_entropy                 : num  452 455 453 453 449 ...
    ##  $ SF_scheme_entropy                : num  452 455 453 453 449 ...
    ##  $ SF_entropy_gain                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SF_mean_prior_entropy            : num  3.51 3.52 3.51 3.51 3.48 ...
    ##  $ SF_mean_scheme_entropy           : num  3.51 3.52 3.51 3.51 3.48 ...
    ##  $ SF_mean_entropy_gain             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_information                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_mean_information              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ KB_relative_information          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ True_positive_rate               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Num_true_positives               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ False_positive_rate              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Num_false_positives              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ True_negative_rate               : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Num_true_negatives               : num  124 124 124 124 124 124 124 124 124 124 ...
    ##  $ False_negative_rate              : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Num_false_negatives              : num  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ IR_precision                     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ IR_recall                        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ F_measure                        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Matthews_correlation             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Area_under_ROC                   : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ Area_under_PRC                   : num  0.0388 0.0388 0.0388 0.0388 0.0388 ...
    ##  $ Weighted_avg_true_positive_rate  : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_false_positive_rate : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_true_negative_rate  : num  0.806 0.814 0.814 0.814 0.806 ...
    ##  $ Weighted_avg_false_negative_rate : num  0.806 0.814 0.814 0.814 0.806 ...
    ##  $ Weighted_avg_IR_precision        : num  0.0376 0.0346 0.0346 0.0346 0.0376 ...
    ##  $ Weighted_avg_IR_recall           : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Weighted_avg_F_measure           : num  0.0629 0.0584 0.0584 0.0584 0.0629 ...
    ##  $ Weighted_avg_matthews_correlation: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Weighted_avg_area_under_ROC      : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ Weighted_avg_area_under_PRC      : num  0.111 0.108 0.109 0.109 0.112 ...
    ##  $ Unweighted_macro_avg_F_measure   : num  0.0216 0.0209 0.0209 0.0209 0.0216 ...
    ##  $ Unweighted_micro_avg_F_measure   : num  0.194 0.186 0.186 0.186 0.194 ...
    ##  $ Elapsed_Time_training            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Elapsed_Time_testing             : num  0 0 0 0 0 0 0 0.001 0.001 0 ...
    ##  $ UserCPU_Time_training            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ UserCPU_Time_testing             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ UserCPU_Time_millis_training     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ UserCPU_Time_millis_testing      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Serialized_Model_Size            : num  1239 1239 1239 1239 1239 ...
    ##  $ Serialized_Train_Set_Size        : num  307112 307112 307112 307112 307112 ...
    ##  $ Serialized_Test_Set_Size         : num  37760 37760 37760 37760 37760 ...
    ##  $ Coverage_of_Test_Cases_By_Regions: num  96.9 96.9 96.9 96.9 97.7 ...
    ##  $ Size_of_Predicted_Regions        : num  86.7 86.7 86.7 86.7 86.7 ...
    ##  $ Summary                          : Factor w/ 11 levels "'Selected values: -C 10 -L 0.050050000000000004 -P 1.0E-12 -N 0 -V -1 -W 1 -K \\weka.classifiers.functions.supp"| __truncated__,..: 11 11 11 11 11 11 11 11 11 11 ...
    ##  $ measureOutOfBagError             : Factor w/ 2 levels "-1.0","?": 2 2 2 2 2 2 2 2 2 2 ...

##### Check dimensions of results\_k15\_51\_100

``` r
dim(results_k15_51_100)
```

    ## [1] 3000   68

#### Extract the results

##### We begin with results\_k15\_1\_50

##### 50 datasets \* 10 folds = 500; 500 rows/algorithm \* 6 algorithms = 3000

##### Beginning of each dataset in algorithm 1:

``` r
beg_1 = seq(1, 500, by = 10)
beg_1
```

    ##  [1]   1  11  21  31  41  51  61  71  81  91 101 111 121 131 141 151 161 171 181
    ## [20] 191 201 211 221 231 241 251 261 271 281 291 301 311 321 331 341 351 361 371
    ## [39] 381 391 401 411 421 431 441 451 461 471 481 491

##### Length of beg\_1

``` r
length(beg_1)
```

    ## [1] 50

##### Number of folds

``` r
n_folds = 10
```

##### Definition of necessary variables for k\_1\_50

``` r
Accuracy_df_1_50 = data.frame(matrix(NA, nrow = 60, ncol = 50))
# nrow = 60: 6 algorithms * 10 folds/algorithm
# ncols = 50: 50 datasets
Algorithm_names = as.factor(c(rep("ZeroR", n_folds), rep("OneR", n_folds), rep("AdaBoost", n_folds),
                    rep("SVM_poly", n_folds), rep("RandomForest", n_folds), 
                    rep("RotationForest", n_folds)))
aov_pvalues_1_50 = as.numeric(rep(NA,50))
Duncan_df_1_50 = data.frame(matrix(NA, nrow = 6, ncol = 150))
RF_group_1_50 = as.character(rep(NA,50))
# nrow = 6 because we have 6 algorithms
# ncol = 150 because we keep algorithm name, accuracies and group (3 variables per dataset)
duncan_seq_ini = seq(1, 150, by = 3)
duncan_seq_end = seq(3, 150, by = 3)
```

##### Information retrieval

``` r
for (i in 1:50){ #i selects the dataset
  alg_counter = 0
  for (j in seq(1, 60, by=10)){
    #j selects the index to access Accuracy_df_1_50 (10 rows for each algorithm)
    k_ini = beg_1[i] + 500*alg_counter #beginning of the folds for each algorithm
    k_end = k_ini + 9
    Accuracy_df_1_50[j:(j+9),i] = results_k15_1_50[k_ini:k_end,]$"Percent_correct"
    alg_counter = alg_counter + 1
  }
  # Test Anova
  anova_data_i = data.frame(as.numeric(Accuracy_df_1_50[,i]), Algorithm_names)
  colnames(anova_data_i) <- c("Percent_correct", "Algorithm")
  anova_test_i <- aov(anova_data_i$Percent_correct~anova_data_i$Algorithm, data = anova_data_i)
  aov_pvalues_1_50[i] <- summary(anova_test_i)[[1]][["Pr(>F)"]][1]
  
  # Multiple comparison test
  mult_comp_i <- duncan.test(anova_test_i, "anova_data_i$Algorithm", alpha = 0.05)
  Duncan_df_1_50[,duncan_seq_ini[i]:duncan_seq_end[i]] <- cbind(row.names(mult_comp_i$groups),
                                                       mult_comp_i$groups)
  RF_group_1_50[i] = as.character(mult_comp_i[[6]]["RandomForest","groups"])
}
```

#### Do the same now with results\_k15\_51\_100

##### Definition of necessary variables for k\_51\_100

``` r
Accuracy_df_51_100 = data.frame(matrix(NA, nrow = 60, ncol = 50))
# nrow = 60: 6 algorithms * 10 folds/algorithm
# ncols = 50: 50 datasets
Algorithm_names = as.factor(c(rep("ZeroR", n_folds), rep("OneR", n_folds), rep("AdaBoost", n_folds),
                    rep("SVM_poly", n_folds), rep("RandomForest", n_folds), 
                    rep("RotationForest", n_folds)))
aov_pvalues_51_100 = as.numeric(rep(NA,50))
Duncan_df_51_100 = data.frame(matrix(NA, nrow = 6, ncol = 150))
RF_group_51_100 = as.character(rep(NA,50))
# nrow = 6 because we have 6 algorithms
# ncol = 150 because we keep algorithm name, accuracies and group (3 variables per dataset)
duncan_seq_ini = seq(1, 150, by = 3)
duncan_seq_end = seq(3, 150, by = 3)
```

##### Information retrieval

``` r
for (i in 1:50){ #i selects the dataset
  alg_counter = 0
  for (j in seq(1, 60, by=10)){
    #j selects the index to access Accuracy_df_1_50 (10 rows for each algorithm)
    k_ini = beg_1[i] + 500*alg_counter #beginning of the folds for each algorithm
    k_end = k_ini + 9
    Accuracy_df_51_100[j:(j+9),i] = results_k15_51_100[k_ini:k_end,]$"Percent_correct"
    alg_counter = alg_counter + 1
  }
  # Test Anova
  anova_data_i = data.frame(as.numeric(Accuracy_df_51_100[,i]), Algorithm_names)
  colnames(anova_data_i) <- c("Percent_correct", "Algorithm")
  anova_test_i <- aov(anova_data_i$Percent_correct~anova_data_i$Algorithm, data = anova_data_i)
  aov_pvalues_51_100[i] <- summary(anova_test_i)[[1]][["Pr(>F)"]][1]
  
  # Multiple comparison test
  mult_comp_i <- duncan.test(anova_test_i, "anova_data_i$Algorithm", alpha = 0.05)
  Duncan_df_51_100[,duncan_seq_ini[i]:duncan_seq_end[i]] <- cbind(row.names(mult_comp_i$groups),
                                                       mult_comp_i$groups)
  RF_group_51_100[i] = as.character(mult_comp_i[[6]]["RandomForest","groups"])
}
```

#### Join results 1\_50 and 51\_100

#### p\_value

``` r
aov_pvalues_1_100 = c(aov_pvalues_1_50, aov_pvalues_51_100)
```

##### Check that the AOV null hypothesis that the means of the different groups are the same can be rejected in all cases (p-value \< 0.05)

``` r
aov_pvalues_1_100 < 0.05
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
sum(aov_pvalues_1_100 < 0.05)
```

    ## [1] 100

#### Group to which RF belongs according to the Duncan test

``` r
RF_group_1_100 = c(RF_group_1_50, RF_group_51_100)
RF_group_1_100
```

    ##   [1] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"
    ##  [19] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"
    ##  [37] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"
    ##  [55] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"
    ##  [73] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"
    ##  [91] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"

##### Check that in all cases RF group is “a”

##### If length(unique(x)) == 1, all values are equal

``` r
length(unique(RF_group_1_100))
```

    ## [1] 1

#### Detail of the Duncan test (joined)

``` r
Duncan_df_1_100 = cbind(Duncan_df_1_50, Duncan_df_51_100)
dim(Duncan_df_1_100)
```

    ## [1]   6 300

##### Save the detailed table of the 100 Duncan tests (one test per dataset)

``` r
save(Duncan_df_1_100, file = "Detailed_Duncan_df_1_100.Rda")
write.csv(Duncan_df_1_100, "Detailed_Duncan_df_1_100.csv")
```

##### Explore the details of Duncan\_df\_1\_100

``` r
seq_alg_names = seq(1, 298, by = 3)
hundred_alg_names = unfactor(Duncan_df_1_100[,seq_alg_names])
```

##### Check if group “a” is always composed by the same algorithms

``` r
equal_vector = as.character(rep(NA,99))
for(i in 1:99){
  target = data.frame(hundred_alg_names[1:3,i+1])
  colnames(target) <- c("alg")
  current = data.frame(hundred_alg_names[1:3,i])
  colnames(current) <- c("alg")
  equal_vector[i] <- all_equal(target, current, ignore_col_order = TRUE)
}

equal_vector
```

    ##  [1] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [11] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [21] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [31] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [41] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [51] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [61] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [71] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [81] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"
    ## [91] "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE" "TRUE"

##### Check groups

``` r
seq_groups = seq(3, 300, by = 3)
groups = unfactor(Duncan_df_1_100[,seq_groups])
```

##### Check if group “a” is always composed by the same algorithms

``` r
equal_groups = as.character(rep(NA,99))
for(i in 1:99){
  target = data.frame(groups[,i+1])
  colnames(target) <- c("groups")
  current = data.frame(groups[,i])
  colnames(current) <- c("groups")
  equal_groups[i] <- all_equal(target, current)
}

equal_groups
```

    ##  [1] "TRUE"                                       
    ##  [2] "TRUE"                                       
    ##  [3] "TRUE"                                       
    ##  [4] "TRUE"                                       
    ##  [5] "TRUE"                                       
    ##  [6] "TRUE"                                       
    ##  [7] "TRUE"                                       
    ##  [8] "TRUE"                                       
    ##  [9] "TRUE"                                       
    ## [10] "TRUE"                                       
    ## [11] "TRUE"                                       
    ## [12] "TRUE"                                       
    ## [13] "TRUE"                                       
    ## [14] "TRUE"                                       
    ## [15] "TRUE"                                       
    ## [16] "TRUE"                                       
    ## [17] "TRUE"                                       
    ## [18] "TRUE"                                       
    ## [19] "TRUE"                                       
    ## [20] "TRUE"                                       
    ## [21] "TRUE"                                       
    ## [22] "TRUE"                                       
    ## [23] "TRUE"                                       
    ## [24] "TRUE"                                       
    ## [25] "TRUE"                                       
    ## [26] "TRUE"                                       
    ## [27] "Factor levels not equal for column `groups`"
    ## [28] "Factor levels not equal for column `groups`"
    ## [29] "TRUE"                                       
    ## [30] "TRUE"                                       
    ## [31] "TRUE"                                       
    ## [32] "TRUE"                                       
    ## [33] "TRUE"                                       
    ## [34] "TRUE"                                       
    ## [35] "TRUE"                                       
    ## [36] "TRUE"                                       
    ## [37] "TRUE"                                       
    ## [38] "TRUE"                                       
    ## [39] "TRUE"                                       
    ## [40] "TRUE"                                       
    ## [41] "TRUE"                                       
    ## [42] "TRUE"                                       
    ## [43] "TRUE"                                       
    ## [44] "TRUE"                                       
    ## [45] "TRUE"                                       
    ## [46] "TRUE"                                       
    ## [47] "TRUE"                                       
    ## [48] "TRUE"                                       
    ## [49] "TRUE"                                       
    ## [50] "TRUE"                                       
    ## [51] "TRUE"                                       
    ## [52] "TRUE"                                       
    ## [53] "TRUE"                                       
    ## [54] "TRUE"                                       
    ## [55] "TRUE"                                       
    ## [56] "TRUE"                                       
    ## [57] "TRUE"                                       
    ## [58] "TRUE"                                       
    ## [59] "TRUE"                                       
    ## [60] "TRUE"                                       
    ## [61] "TRUE"                                       
    ## [62] "TRUE"                                       
    ## [63] "TRUE"                                       
    ## [64] "TRUE"                                       
    ## [65] "TRUE"                                       
    ## [66] "TRUE"                                       
    ## [67] "TRUE"                                       
    ## [68] "TRUE"                                       
    ## [69] "TRUE"                                       
    ## [70] "TRUE"                                       
    ## [71] "TRUE"                                       
    ## [72] "TRUE"                                       
    ## [73] "TRUE"                                       
    ## [74] "TRUE"                                       
    ## [75] "TRUE"                                       
    ## [76] "TRUE"                                       
    ## [77] "TRUE"                                       
    ## [78] "TRUE"                                       
    ## [79] "TRUE"                                       
    ## [80] "TRUE"                                       
    ## [81] "TRUE"                                       
    ## [82] "TRUE"                                       
    ## [83] "TRUE"                                       
    ## [84] "TRUE"                                       
    ## [85] "TRUE"                                       
    ## [86] "TRUE"                                       
    ## [87] "TRUE"                                       
    ## [88] "TRUE"                                       
    ## [89] "TRUE"                                       
    ## [90] "TRUE"                                       
    ## [91] "TRUE"                                       
    ## [92] "TRUE"                                       
    ## [93] "TRUE"                                       
    ## [94] "TRUE"                                       
    ## [95] "TRUE"                                       
    ## [96] "TRUE"                                       
    ## [97] "TRUE"                                       
    ## [98] "TRUE"                                       
    ## [99] "TRUE"

##### Explore the different one

``` r
data.frame(groups[,27])
```

    ##   groups...27.
    ## 1            a
    ## 2            a
    ## 3            a
    ## 4            b
    ## 5            c
    ## 6            d

``` r
data.frame(groups[,28])
```

    ##   groups...28.
    ## 1            a
    ## 2           ab
    ## 3            b
    ## 4            c
    ## 5            d
    ## 6            e

``` r
Duncan_df_1_100[,82:84]
```

    ##              X82      X83 X84
    ## 1   RandomForest 53.25581   a
    ## 2 RotationForest 52.55814  ab
    ## 3       SVM_poly 49.68992   b
    ## 4           OneR 31.55039   c
    ## 5       AdaBoost 25.27132   d
    ## 6          ZeroR 19.14729   e
