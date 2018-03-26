# Libraries & data ----
library(varclust)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggmap)
load("march_less.rda")
load("march_daily.rda")
load("stations.rda")
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
# Initialization based on SSC ----
four_clusters_ssc <- list(c(4,3,3,1,1,1,3,3,3,3,3,3,4,3,3,1,1,1,4,3,3,1,1,
                       1,4,3,3,2,2,2,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,
                       1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,
                       1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,
                       3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,
                       3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,1,1,1,4,
                       3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,
                       4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,
                       1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,
                       1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,
                       1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,3,1,1,1,4,3,
                       3,1,1,1,4,3,3,1,1,1))
six_clusters_ssc <- list(c(4,5,2,3,3,3,5,5,2,5,5,5,4,5,2,3,3,3,4,5,2,3,3,3,
                      4,5,2,1,1,1,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,
                      4,5,2,3,3,3,4,5,2,6,6,6,4,5,2,3,3,3,4,5,2,6,6,6,
                      4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,
                      4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,
                      4,5,2,3,3,3,4,5,2,6,6,6,4,5,3,3,3,4,5,2,3,3,3,4,
                      5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,
                      5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,
                      5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,6,6,6,4,
                      5,2,3,3,3,4,5,2,6,6,6,4,5,2,6,6,6,4,5,2,6,6,6,4,
                      5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3,4,5,2,3,3,3))
# Clustering
march_varclust_ssc <- mlcc.reps(march_less, numb.clusters = 4,
                                numb.runs = 30, max.iter = 30,
                                initial.segmentations = four_clusters_ssc)
march_varclust_ssc2 <- mlcc.reps(march_less, numb.clusters = 6,
                                 numb.runs = 30, max.iter = 30,
                                 initial.segmentations = six_clusters_ssc)
# Display generated clusters
for(i in 1:max(march_varclust_ssc$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
for(i in 1:max(march_varclust_ssc2$segmentation)) {
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
# Quick stability experiment. ----
# proba_stab <- lapply(1:20, function(x) {
#   tmp <- mlcc.bic(march_less, greedy = FALSE)
#   max(tmp$segmentation)
# })
# hist(unlist(proba_stab))
# table(unlist(proba_stab))
## Co z większą liczbą iteracji?
# march_vcl <- mlcc.bic(march_less, max.iter = 30, greedy = FALSE)
# max(march_vcl$segmentation)
# Stability ----
# stab <- lapply(1:10, function(x) {
#   tmp <- mlcc.bic(march_less, greedy = FALSE, numb.runs = 30)
#   max(tmp$segmentation)
# })
# hist(unlist(stab))
# library(microbenchmark)
# microbenchmark(mlcc.bic(march_less, greedy = FALSE, numb.runs = 30), times = 1)
