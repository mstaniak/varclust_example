# Data analysis using varclust initialized with SSC clusterings.
## Load libraries, functions, SSC clusterings and datasets ----
library(varclust)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggmap)
library(readr)
source("functions.R")
source("ssc_clusters_vecs.R")
load("./data/march_less.rda")
load("./data/march_daily.rda")
load("./data/stations.rda")
sensor_locations <- read_csv("./data/sensor_locations.csv")
## Run varclust on each of the SSC clusterings ----
every_cl <- list(rep(1, 263), c(1, cl2[2:263]), cl3, cl4, cl5, cl6, cl7, cl8, cl9,
                 cl10, cl11, cl12, cl13, cl14, cl15, cl16, cl17, cl18, cl19, cl20)
every_vcl <- lapply(1:20, function(x)
               mlcc.reps(march_less, numb.clusters = x, numb.runs = 30, max.iter = 50,
                         initial.segmentations = list(every_cl[[x]][1:263])))
lapply(every_vcl, function(x) x$BIC)
### The largest BIC: 19 clusters
clusters_ssc19 <- print.clusters.vec(march_less, cl19)
clusters_vcl9 <- print.clusters(march_less, every_vcl[[19]])
### What about other partitions?
lapply(1:20, function(x) {
  cl_ssc <- print.clusters.vec(march_less, (every_cl[[x]])[1:263])
  cl_vcl <- print.clusters(march_less, every_vcl[[x]])
  if(all(dim(cl_ssc) == dim(cl_vcl))) {
    sum(cl_ssc == cl_vcl)/(nrow(cl_ssc)*ncol(cl_ssc))
  } else {
    "Diff dim"
  }
})
## Draw maps showing the clusterings ----
print.clusters(march_less, every_vcl[[19]])
### First for pressure
draw.map(every_vcl[[19]]$segmentation, clusters = c(5, 10, 17, 18))
### Then for particular matter
draw.map(every_vcl[[19]]$segmentation, clusters = c(1:3, 6:8, 12:16, 19))
### Compare to SSC initial clustering
print.clusters.vec(march_less, cl19)
draw.map(cl19, clusters = c(5, 10, 17, 18))
draw.map(cl19, clusters = c(1:3, 6:8, 12:16, 19))
