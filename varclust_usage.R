# Libraries & data ----
library(varclust)
load("march_less.rda")
# Random initialization, all arguments set to default ----
march_varclust <- mlcc.bic(march_less)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
# Initialization based on SSC ----
march_varclust_ssc <- mlcc.reps(march.less,
                                ...)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
# Vis (?)
