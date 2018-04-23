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
BICs <- unlist(lapply(every_vcl, function(x) x$BIC))
ggplot(tibble(x = 1:20, BIC = BICs), aes(x = x, y = BICs)) +
  geom_point() +
  theme_bw() +
  xlab("number of clusters") +
  ylab("BIC") +
  ylim(c(0, 40000))
### Chosen number of clusters: 10.
clusters_ssc10 <- print.clusters.vec(march_less, cl10)
clusters_vcl0 <- print.clusters(march_less, every_vcl[[10]])
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
print.clusters(march_less, every_vcl[[10]])
map_pm_vcl <- draw.map(every_vcl[[19]]$segmentation, clusters = c(1:3, 6:8, 12:16, 19)) +
  ggtitle("Particle matter measurements clustered using varclust")
ggsave(map_pm_vcl, filename = "map_pm_vcl.png")
### Compare to SSC initial clustering
print.clusters.vec(march_less, cl19)
map_pm_ssc <- draw.map(cl19, clusters = c(1:3, 6:8, 12:16, 19)) +
  ggtitle("Particle matter measurements clustered using varclust")
ggsave(map_pm_ssc, filename = "map_pm_ssc.png")
### Stability ----
stability_mbic <- lapply(seq(20, 100, 10), function(x) {
  cat("\n")
  tibble(numb_runs = x,
         mbic_vals = sapply(1:10, function(y) {
           cat(".")
           mlcc.bic(march_less, numb.clusters = 10,
                    numb.runs = x)$BIC
  }))
})
save(stability_mbic, file = "stability_mbic.rda")
mbic_plot <- bind_rows(stability_mbic) %>%
  ggplot(aes(x = reorder(as.character(numb_runs), numb_runs), y = mbic_vals)) +
    geom_boxplot() +
    theme_bw() +
    ylab("mBIC") +
    xlab("number of runs of mlcc.kmeans algorithm")
ggsave("mbic_plot.png", mbic_plot)
