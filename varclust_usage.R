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
                           deterministic = TRUE)
save(march_varclust, file = "march_varclust.rda")
# Number of clusters
max(march_varclust$segmentation)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
print.clusters(march_less, march_varclust)
march_varclust$BIC
# Daily data ----
daily_varclust <- mlcc.bic(march_daily, deterministic = TRUE)
print.clusters(march_daily, daily_varclust)
max(daily_varclust$segmentation)
daily_varclust$BIC
# Maps. ----
segmentation <- march_varclust$segmentation
## Map for random-initialized clustering.
print.clusters(march_less, march_varclust)
draw.map(segmentation, c(4, 6, 9))
