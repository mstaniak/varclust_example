# Libraries & data ----
library(varclust)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggmap)
sensor_locations <- read_csv("data/sensor_locations.csv")
load("data/march_less.rda")
load("data/march_daily.rda")
load("data/stations.rda")
source("R/functions.R")
set.seed(42)
# Random initialization, all arguments set to default ----
march_varclust <- mlcc.bic(march_less, greedy = F)
save(march_varclust, file = "data/march_varclust.rda")
# Number of clusters
max(march_varclust$segmentation)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
print.clusters(march_less, march_varclust)
march_varclust$BIC
# Daily data ----
daily_varclust <- mlcc.bic(march_daily, greedy = F)
print.clusters(march_daily, daily_varclust)
max(daily_varclust$segmentation)
daily_varclust$BIC
# Maps. ----
segmentation <- march_varclust$segmentation
## Map for random-initialized clustering.
print.clusters(march_less, march_varclust)
map_pm_pure_vcl <- draw.map(segmentation, c(1,4,6,7,8,9)) +
ggtitle("Particle matter measurements clustered using varclust") + 
  theme(plot.title = element_text(hjust=0.5))
ggsave(filename = "pictures/map_pm_pure_vcl.png", map_pm_pure_vcl, scale = 1.35)
