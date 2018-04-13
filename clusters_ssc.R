library(varclust)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggmap)
load("data/march_less.rda")
load("data/march_daily.rda")
source("functions.R")
source("ssc_clusters_vecs.R")

every_cl <- list(rep(1, 263), c(1, cl2[2:263]), cl3, cl4, cl5, cl6, cl7, cl8, cl9,
                 cl10, cl11, cl12, cl13, cl14, cl15, cl16, cl17, cl18, cl19, cl20)
every_vcl <- lapply(1:20, function(x)
               mlcc.reps(march_less, numb.clusters = x, numb.runs = 30, max.iter = 50,
                         initial.segmentations = every_cl[[x]]))
lapply(every_vcl, function(x) x$BIC)
# The biggest BIC: 19 clusters
clusters_ssc19 <- print.clusters.vec(march_less, cl19)
clusters_vcl9 <- print.clusters(march_less, vclust19)
sum(clusters_ssc19 == clusters_vcl9) # żadnych zmian
# A jak dla innych?
lapply(1:20, function(x) {
  cl_ssc <- print.clusters.vec(march_less, (every_cl[[x]])[1:263])
  cl_vcl <- print.clusters(march_less, every_vcl[[x]])
  if(all(dim(cl_ssc) == dim(cl_vcl))) {
    sum(cl_ssc == cl_vcl)/(nrow(cl_ssc)*ncol(cl_ssc))
  } else {
    "Diff dim"
  }
})
# Tylko tutaj zaszły zmiany
cl_ssc2 <- print.clusters.vec(march_less, (every_cl[[2]])[1:263])
cl_vcl2 <- print.clusters(march_less, every_vcl[[2]])
# draw.map(every_cl[[2]]][1:263])
cl_ssc3 <- print.clusters.vec(march_less, (every_cl[[3]])[1:263])
cl_vcl3 <- print.clusters(march_less, every_vcl[[3]])

draw.map(vclust8$segmentation, clusters = c(2, 4, 6, 7, 8))
print.clusters(march_less, vclust8)

print.clusters(march_less, vclust10)
draw.map(vclust10$segmentation, clusters = c(1:2, 5:7, 9:10))
