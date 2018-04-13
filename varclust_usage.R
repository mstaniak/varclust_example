# Libraries & data ----
library(varclust)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggmap)
load("data/march_less.rda")
load("data/march_daily.rda")
load("data/stations.rda")
source("functions.R")
# Random initialization, all arguments set to default ----
march_varclust <- mlcc.bic(march_less,
                           greedy = FALSE)
# Number of clusters
max(march_varclust$segmentation)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
print.clusters(march_less, march_varclust)
# Daily data ----
daily_varclust <- mlcc.bic(march_daily, greedy = FALSE)
print.clusters(march_daily, daily_varclust)
daily_varclust$BIC
## Bigger max.dim
daily_varclust2 <- mlcc.bic(march_daily, max.dim = 8, greedy = FALSE)
print.cluster(march_daily, daily_varclust2)
daily_varclust2$BIC
# Maps. ----
segmentation <- march_varclust$segmentation
segmentation2 <- march_varclust_ssc$segmentation
segmentation3 <- march_varclust_ssc2$segmentation
## Map for random-initialized clustering.
print.clusters(march_less, march_varclust)
draw.map(segmentation, c(4, 6, 9))
draw.map(segmentation, c(2, 5, 7))
## Map for SSC-initialized clustering.
print.clusters(march_less, march_varclust_ssc)
print.clusters(march_less, march_varclust_ssc2)
draw.map(segmentation2, 1)
draw.map(segmentation3, c(3, 6))
