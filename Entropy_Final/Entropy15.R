rm(list=ls(all=TRUE))

library(dplyr)
library(entropy)
library(readxl)
library(writexl)
library(purrr)
library(pdqr)


##Read data
setwd("D:/OneDrive - Universidad de Burgos/Goonies/CULM/GitHub_repository/Lets-go-Fishing/Entropy_Final/")

n<-15

entropyVectors<-rep(0.0,n)
sdVectors<-rep(0.0,n)
Limit15<-rep(0.0,n)
Limit10<-rep(0.0,n)

for(i in 1:n)
{
myDF<-read_excel("hc_k_15_interpretation.xlsx",sheet = i)

myEntropy<-myDF %>%
  select(Gathering, Hunting, Fishing, Husbandry, Agriculture) %>%
  summarise_all(mean) %>%
  as.numeric() %>%
  entropy(, method = "ML")

mysd<-myDF %>%
  select(Gathering, Hunting, Fishing, Husbandry, Agriculture) %>%
  summarise_all(mean) %>%
  as.numeric() %>%
  sd()

myLimit<-myDF %>%
  select(Gathering, Hunting, Fishing, Husbandry, Agriculture) %>%
  summarise_all(mean)
 
myLimit15<-sum(myLimit[1,]>15)
myLimit10<-sum(myLimit[1,]>10)

entropyVectors[i]<-myEntropy
sdVectors[i]<-mysd
Limit15[i]<-myLimit15
Limit10[i]<-myLimit10
}

cluster<-seq(1:n)

output<-data.frame(cluster,entropyVectors,sdVectors,Limit15,Limit10)

write_xlsx(output,"entropy_15.xlsx")
