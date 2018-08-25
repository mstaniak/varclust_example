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
set.seed(42)
load("./data/march_less.rda")
load("./data/march_daily.rda")
load("./data/stations.rda")
sensor_locations <- read_csv("./data/sensor_locations.csv")
## Run varclust on each of the SSC clusterings ----
every_cl <- list(rep(1, 263), c(1, cl2[2:263]), cl3, cl4, cl5, cl6, cl7, cl8, cl9,
                 cl10, cl11, cl12, cl13, cl14, cl15, cl16, cl17, cl18, cl19, cl20)
every_vcl <- lapply(1:20, function(x)
               mlcc.reps(march_less, numb.clusters = x, max.iter = 50,
                         initial.segmentations = list(every_cl[[x]][1:263])))
BICs <- unlist(lapply(every_vcl, function(x) x$BIC))
nclust_vs_BIC <- ggplot(tibble(x = 1:20, BIC = BICs), aes(x = x, y = BICs)) +
  geom_point() +
  theme_bw() +
  xlab("number of clusters") +
  ylab("mBIC")
ggsave(nclust_vs_BIC, file = "nclust_vs_BIC.png")
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
map_pm_vcl <- draw.map(every_vcl[[10]]$segmentation, clusters = c(1:2, 5:7, 9:10)) +
  ggtitle("Particle matter measurements clustered using varclust")
ggsave(map_pm_vcl, filename = "map_pm_vcl.png")
### Compare to SSC initial clustering
print.clusters.vec(march_less, every_cl[[10]])
map_pm_ssc <- draw.map(every_cl[[10]][1:263], clusters = c(1:2, 5:7, 9:10)) +
  ggtitle("Particle matter measurements clustered using varclust")
# ggsave(map_pm_ssc, filename = "map_pm_ssc.png")
vars_in_cluster<- print.clusters.vec(march_less, every_cl[[10]])$cluster_7
vars_in_cluster <- vars_in_cluster[!is.na(vars_in_cluster)]
vars_in_cluster <- vars_in_cluster[vars_in_cluster != "-"]
cluster <- mutate_all(march_less[, vars_in_cluster],
                         function(x) ifelse(is.na(x), mean(x, na.rm = T), x))
pca_clust <- princomp(cluster, cor = TRUE)
plot(pca_clust$loadings)
plot(pca_clust$scores)
biplot(pca_clust)
###
# krakow <- get_map(location = c(50.05970, 19.94139))
# ggmap(krakow)
### Stability ----
# stability_mbic <- lapply(seq(20, 100, 10), function(x) {
#   cat("\n")
#   tibble(numb_runs = x,
#          mbic_vals = sapply(1:10, function(y) {
#            cat(".")
#            mlcc.reps(march_less, numb.clusters = 10,
#                     numb.runs = x)$BIC
#   }))
# })
# save(stability_mbic, file = "stability_mbic.rda")
# mbic_plot <- bind_rows(stability_mbic) %>%
#   ggplot(aes(x = reorder(as.character(numb_runs), numb_runs), y = mbic_vals)) +
#     geom_boxplot() +
#     theme_bw() +
#     ylab("mBIC") +
#     xlab("number of runs of mlcc.kmeans algorithm")
# ggsave("mbic_plot.png", mbic_plot)
