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
# Random initialization, all arguments set to default ----
march_varclust <- mlcc.bic(march_less)
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
                                initial.segmentations = four_clusters_ssc)
march_varclust_ssc2 <- mlcc.reps(march_less, numb.clusters = 6,
                                 initial.segmentations = six_clusters_ssc)
# Display generated clusters
for(i in 1:max(march_varclust_ssc$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
for(i in 1:max(march_varclust_ssc2$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
# Propositions for the future
local.biplot <- function(data, mlcc_object, cluster) {
  if(cluster > max(mlcc_object$segmentation))
    stop("Cluster label is out of range")
  data_new <- mlcc.preprocess(data)
  one_cluster <- data_new[, mlcc_object$segmentation == cluster]
  pca <- princomp(one_cluster)
  biplot(pca)
}
local.biplot(march_less, march_varclust, 6)
print.clusters <- function(data, mlcc_object) {
  # Assume data is preprocessed (only numeric variables)
  max_cluster_size <- max(as.data.frame(table(mlcc_object$segmentation))$Freq)
  tmp <- lapply(1:max(mlcc_object$segmentation), function(x) {
    colnames_in_cluster <- colnames(data)[mlcc_object$segmentation == x]
    current_cluster_size <- length(colnames_in_cluster)
    c(colnames_in_cluster,
      rep("-", times = max_cluster_size - current_cluster_size))
  })
  tmp <- as.data.frame(tmp)
  colnames(tmp) <- paste("cluster", 1:max(mlcc_object$segmentation), sep = "_")
  tmp
}
print.clusters(march_less, march_varclust)
# Daily data ----
daily_varclust <- mlcc.bic(march_daily)
print.clusters(march_daily, daily_varclust)
daily_varclust$BIC
# Bigger max.dim
daily_varclust2 <- mlcc.bic(march_daily, max.dim = 8)
print.cluster(march_daily, daily_varclust2)
daily_varclust2$BIC
# Factors within cluster 2
# march_vc <- mlcc.bic(scale(march_less))
# pca <- princomp(scale(march_less)[, march_varclust$segmentation == 2])
# head(pca$loadings[, 1:4])
# head(march_vc$factors[[2]])
# PCA for all data.
march_pca <- princomp(march_less)
march_pca$sdev
plot(march_pca$sdev)
march_pca$sdev/sum(march_pca$sdev)
round(cumsum(march_pca$sdev)/sum(march_pca$sdev), 2)
# Quickly check stability
march_vcl <- mlcc.reps(march_less, max.iter = 30)
march_vcl2 <- mlcc.reps(march_less, max.iter = 30)
max(march_vcl$segmentation)
max(march_vcl2$segmentation)
# Sensor locations
sensor_locations <- read_csv("sensor_locations.csv")
stations_meas <- colnames(march_less)
stations_meas <- str_replace(stations_meas, "X", "")
stations <- str_split(stations_meas, "_", simplify = T)[, 1]
meas <- str_split(stations_meas, "_", simplify = T)[, 2]
stations <- tibble(id = as.integer(stations),
                   meas = meas) %>%
  left_join(sensor_locations, by = "id") %>%
  mutate(segmentation1 = march_varclust$segmentation)
print.clusters(march_less, march_varclust)
cluster4 <- stations %>%
  filter(segmentation1 == 4) %>%
  distinct(id) %>%
  left_join(sensor_locations, by = "id")
cluster6 <- stations %>%
  filter(segmentation1 == 6) %>%
  distinct(id) %>%
  left_join(sensor_locations, by = "id")
krakow_map <- get_map("https://www.google.pl/maps/place/Krak%C3%B3w/@50.0467446,19.9348338,12z/data=!3m1!4b1!4m5!3m4!1s0x471644c0354e18d1:0xb46bb6b576478abf!8m2!3d50.0646501!4d19.9449799")
krakow_map <- get_map("Kraków")
ggmap(krakow_map) +
  geom_point(data = cluster4, aes(latitude, longitude),
             size = 4, color = "red")
ggplot() +
  geom_point(data = cluster4, aes(latitude, longitude),
             size = 4, color = "red") +
  geom_point(data = cluster6, aes(latitude, longitude),
             size = 6, color = "blue") +
  geom_text(data = sensor_locations, aes(x = latitude, y = longitude,
                                         label = id),
            nudge_x = 0.003)
# TODO: same map for clustering starting from ssc
