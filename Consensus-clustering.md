Consensus clustering
================
Virginia\_Ahedo
8/10/2020

### Setup

### Clean workspace

### Set working paths

``` r
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing")
dataPath <- "D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing"
```

### Load required libraries

``` r
require(doParallel)
```

    ## Loading required package: doParallel

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
require(ConsensusClusterPlus)
```

    ## Loading required package: ConsensusClusterPlus

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

### Configure parallel execution: 6 cores

``` r
cl <- makeCluster(6)
registerDoParallel(cl)
```

### Loading input data. We are going to work with “Id\_Clustering\_db.Rda” since in ConsensusClusterPlus both rows and cols of the input matrix need to be named. Therefore, we will name our rows with the contents of “society\_id”

``` r
load("Id_Clustering_db.Rda")
```

#### Check the structure of “Id\_Clustering\_db.Rda”

``` r
str(id_clust_db)
```

    ## 'data.frame':    1290 obs. of  7 variables:
    ##  $ society_id           : chr  "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##  $ Gathering            : num  74.2 37.3 10 38.8 28.1 ...
    ##  $ Hunting              : num  18.9 55.8 29.2 29.2 65 ...
    ##  $ Fishing              : num  2.3 2.3 10 10 2.3 ...
    ##  $ Husbandry            : num  2.3 2.3 48.3 19.6 2.3 ...
    ##  $ Agriculture          : num  2.3 2.3 2.39 2.39 2.3 ...
    ##  $ Agriculture_intensity: Factor w/ 6 levels "Casual","Extensive/shifting",..: 6 6 6 NA 6 2 6 6 6 6 ...

#### Clustering Analysis will be conducted on variables EA001 to EA005, thus, we drop column 7: Agriculture\_intensity

#### In addition, we are going to drop “society\_id” col, having previously saved its contents in a variable

``` r
society_ids = id_clust_db$society_id
id_clust_db = select(id_clust_db, -c("society_id", "Agriculture_intensity"))
str(id_clust_db)
```

    ## 'data.frame':    1290 obs. of  5 variables:
    ##  $ Gathering  : num  74.2 37.3 10 38.8 28.1 ...
    ##  $ Hunting    : num  18.9 55.8 29.2 29.2 65 ...
    ##  $ Fishing    : num  2.3 2.3 10 10 2.3 ...
    ##  $ Husbandry  : num  2.3 2.3 48.3 19.6 2.3 ...
    ##  $ Agriculture: num  2.3 2.3 2.39 2.39 2.3 ...

``` r
dim(id_clust_db)
```

    ## [1] 1290    5

### ConsensusClusterPlus input requirements:

#### \- d: data matrix where columns=items/samples and rows are features

#### \- After exploring the input matrix in the Tutorial, it appears that both rows and cols need to be named. We name here our rows with the contents of society\_ids

``` r
## Transform the id_clust_db dataframe into a matrix
final_clust_matrix = as.matrix(id_clust_db)

## Set society_ids as column names
rownames(final_clust_matrix) <- society_ids
str(final_clust_matrix)
```

    ##  num [1:1290, 1:5] 74.2 37.3 10 38.8 28.1 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:1290] "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##   ..$ : chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...

#### Standardize elements in final\_clust\_matrix (substract the mean and divide by sd)

#### Such data transformation technique is widely used in clustering analysis.

##### The scale function, by default, centers and/or scales the COLUMNS of a numeric matrix. We want to scale EA001 to EA005, thus we scale prior to creating the transpose matrix.

``` r
final_clust_matrix_norm = scale(final_clust_matrix)
str(final_clust_matrix_norm)
```

    ##  num [1:1290, 1:5] 4.4609 1.8554 -0.0725 1.9564 1.2041 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:1290] "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##   ..$ : chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...
    ##  - attr(*, "scaled:center")= Named num [1:5] 11.1 14.6 15.7 15.8 42.9
    ##   ..- attr(*, "names")= chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...
    ##  - attr(*, "scaled:scale")= Named num [1:5] 14.1 14.1 15.5 16.2 25.1
    ##   ..- attr(*, "names")= chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...

##### Drop attributes “scaled:center”, “scaled:scale”

``` r
attr(final_clust_matrix_norm,"scaled:center")<-NULL
attr(final_clust_matrix_norm,"scaled:scale")<-NULL
str(final_clust_matrix_norm)
```

    ##  num [1:1290, 1:5] 4.4609 1.8554 -0.0725 1.9564 1.2041 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:1290] "Aa1" "Aa2" "Aa3" "Aa4" ...
    ##   ..$ : chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...

#### To meet the input format requirements from ConsensusClusterPlus, we need to transpose our final\_clust\_matrix\_norm

``` r
## So as to have the items as columns, we need to transpose final_clust_matrix
t_final_clust_matrix_norm = t(final_clust_matrix_norm)
str(t_final_clust_matrix_norm)
```

    ##  num [1:5, 1:1290] 4.461 0.304 -0.859 -0.83 -1.618 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:5] "Gathering" "Hunting" "Fishing" "Husbandry" ...
    ##   ..$ : chr [1:1290] "Aa1" "Aa2" "Aa3" "Aa4" ...

``` r
dim(t_final_clust_matrix_norm)
```

    ## [1]    5 1290

### ConsensusClusterPlus function

``` r
# Arguments:
# - d = data to be clustered. Data matrix where columns=items/samples and rows are features
# - maxK = integer value. Maximum cluster number to evaluate. In our case: 20.
#   The ConsensusClusterPlus Tutorial states: recommended cluster count: 20
# - reps = integer value. Number of subsamples. We set = 1000 as it is suggested in the Tutorial.
#   In his paper: Monti set reps to 500.
# - pItem = numerical value. Proportion of items to sample. We set pItem = 0.80 as Monti did.
# - pFeature = numerical value. Proportion of features to sample. We set pFeature = 1. (All features)
# - clusterAlg = character value. cluster algorithm. 'hc' hierarchical clustering (hclust).
# - title = character value for the output directory. 
# - innerLinkage = This option specifies the linkage method to use in iterative agglomerative hierarchical clustering. Not applicable to other clustering algorithms. We chose "ward.D2"
# - finalLinkage = This option specifies the linkage method to use in the final agglomerative hierarchical clustering. We chose "ward.D2".
# - distance = character value. 'pearson' (1 - Pearson correlation), 'spearman', 'euclidean', 'binary',...
#   In coherence with our previous analysis, we set distance = 'euclidean'
# - seed = optional numerical value. Sets random seed for reproducible results.
# - plot = NULL (print to screen), 'pdf', 'png'
# - writeTable = logical value. TRUE - write output and log to csv.
# - verbose = boolean. If TRUE, print messages to the screen to indicate progress.

title = "HC Euclidean Consensus Clustering"

rcc_Euclidean = ConsensusClusterPlus(d = t_final_clust_matrix_norm, maxK = 20, reps = 1000,
                                     pItem = 0.8, pFeature = 1, clusterAlg = 'hc',
                                     innerLinkage = "ward.D2", finalLinkage = "ward.D2",
                                     title = title, distance = 'euclidean', seed = 1, 
                                     plot = 'pdf', writeTable = T, verbose = F)
```

    ## end fraction

    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered
    ## clustered

### Consensus Cluster and Consensus Item

``` r
icl = calcICL(rcc_Euclidean, title = title, plot = "pdf", writeTable = T)
```

#### Check icl structure

``` r
str(icl)
```

    ## List of 2
    ##  $ clusterConsensus: num [1:209, 1:3] 2 2 3 3 3 4 4 4 4 5 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:3] "k" "cluster" "clusterConsensus"
    ##  $ itemConsensus   :'data.frame':    269610 obs. of  4 variables:
    ##   ..$ k            : num [1:269610] 2 2 2 2 2 2 2 2 2 2 ...
    ##   ..$ cluster      : num [1:269610] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ item         : Factor w/ 1290 levels "Aa1","Aa2","Aa3",..: 538 254 304 113 346 587 114 61 163 196 ...
    ##   ..$ itemConsensus: num [1:269610] 0.0286 0.0291 0.025 0.0126 0.0427 ...

``` r
str(icl$clusterConsensus)
```

    ##  num [1:209, 1:3] 2 2 3 3 3 4 4 4 4 5 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:3] "k" "cluster" "clusterConsensus"

#### Obtention of the mean value of cluster consensus for each K in Kmin:Kmax

##### Transform icl$clusterConsensus to dataframe

``` r
clusterCons_data = as.data.frame(icl$clusterConsensus)
```

##### Attach so as to be able to call cols by name

``` r
attach(clusterCons_data)
```

##### Variable definition

``` r
Kmax = 20
consensus_mean <- data.frame(matrix(ncol = 2, nrow = (Kmax -1)))
names(consensus_mean) <- c("K", "Mean_cluster_consensus")

for (i in 1:(Kmax-1)){
  nb_clusters = i + 1
  consensus_mean[i,] <- c(nb_clusters, mean(clusterCons_data[clusterCons_data$k == nb_clusters,
                                                             "clusterConsensus"]))
}
```

##### Order consensus\_mean

``` r
consensus_mean[order(consensus_mean$Mean_cluster_consensus, decreasing = T),]
```

    ##     K Mean_cluster_consensus
    ## 1   2              0.9737245
    ## 2   3              0.8768178
    ## 3   4              0.8563939
    ## 4   5              0.8343188
    ## 5   6              0.8306433
    ## 6   7              0.8250335
    ## 7   8              0.8138448
    ## 8   9              0.8107653
    ## 9  10              0.7956892
    ## 12 13              0.7892982
    ## 10 11              0.7827956
    ## 19 20              0.7811889
    ## 18 19              0.7783024
    ## 11 12              0.7760174
    ## 13 14              0.7723156
    ## 14 15              0.7713356
    ## 16 17              0.7684229
    ## 17 18              0.7677388
    ## 15 16              0.7646121

#### Save the results obtained

``` r
save(rcc_Euclidean, file = "rcc_Euclidean_FINAL.Rda")
save(icl, file = "cluster_item_consensus.Rda")
save(consensus_mean, file = "consensusMean.Rda")
```
