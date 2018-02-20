# Libraries & data ----
library(varclust)
load("march_less.rda")
# Random initialization, all arguments set to default ----
march_varclust <- mlcc.bic(march_less)
# Number of clusters
max(march_varclust$segmentation)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
# Initialization based on SSC ----
march_varclust_ssc <- mlcc.reps(march.less,                               ...)
# Display generated clusters
for(i in 1:max(march_varclust$segmentation)) {
  cat(colnames(march_less)[march_varclust$segmentation == i], "\n")
}
str(march_varclust)
# Vis (?)
# round(cov(factors), 4)
# factors[, 1:2]%*%solve(factors[, 1:2]%*%t(factors[, 1:2]))%*%t(factors[, 1:2])
# det(factors[, 1:2]%*%t(factors[, 1:2]))
# det(factors%*%t(factors))
# dim(factors%*%t(factors))
# local_biplot <- function(data, mlcc_object, cluster) {
#   if(cluster > max(mlcc_object$segmentation))
#     stop("Cluster label is out of range")
#   data_new <- preprocess_data(data)
#   one_cluster <- data_new[, mlcc_object$segmentation == 1]
#   factors <- mlcc_object$factors[[cluster]]
#   projection <- dim(as.matrix(data_new))%*%as.matrix(factors[, 1:2])
# }
# data <- march
# mlcc_object <- march_varclust
# cluster <- 3
# factors <- march_varclust$factors
# class(factors)
# str(factors)
# dim(factors)
