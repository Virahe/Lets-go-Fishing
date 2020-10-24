rm(list=ls(all=TRUE))

library(dplyr)
library(entropy)
library(readxl)
library(writexl)
library(purrr)
library(pdqr)

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(extrafont)


my_tibble <- tibble(
  ClusterNum = numeric(),
  Entropy = numeric(),
)

n<-15

for(i in 1:n)
{
  myDF<-read_excel("hc_k_15_interpretation.xlsx",sheet = i)
  
  for(j in 1:nrow(myDF))
  {
    myEntropy<-myDF[j,] %>%
    select(Gathering, Hunting, Fishing, Husbandry, Agriculture) %>%
    as.numeric() %>%
    entropy(, method = "ML")

    myDatum<-data.frame(ClusterNum=i,Entropy=myEntropy )
    my_tibble<-bind_rows(my_tibble,myDatum)
    
  }
  
  
}

data<-my_tibble

data$ClusterNum <-as.factor(data$ClusterNum)

library(ggridges)

#EntropyVectors
data3<-data
data3$ClusterNum<-factor(data3$ClusterNum,levels =c( 
  "12",
  "4",
  "11",
  "1",
  "5",
  "14",
  "8",
  "6",
  "9",
  "13",
  "7",
  "2",
  "10",
  "3",
  "15"

  ))


data3 %>%
  ggplot( aes(y=ClusterNum, x=Entropy,  fill=ClusterNum)) +
  geom_density_ridges(alpha=0.6, bandwidth=0.06) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Entropy") +
  ylab("Cluster")



