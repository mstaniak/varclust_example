# Propositions for the future ----
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
  tmp %>%
    mutate_all(as.character)
}
print.clusters.vec <- function(data, vec) {
  # Assume data is preprocessed (only numeric variables)
  max_cluster_size <- max(as.data.frame(table(vec))$Freq)
  tmp <- lapply(1:max(vec), function(x) {
    colnames_in_cluster <- colnames(data)[vec == x]
    current_cluster_size <- length(colnames_in_cluster)
    c(colnames_in_cluster,
      rep("-", times = max_cluster_size - current_cluster_size))
  })
  tmp <- as.data.frame(tmp)
  colnames(tmp) <- paste("cluster", 1:max(vec), sep = "_")
  tmp %>%
    mutate_all(as.character)
}
draw.map <- function(segmentation, clusters) {
  stations[["segmentation"]] <- segmentation
  clusters_list <- vector("list", length(clusters))
  for(i in 1:length(clusters)) {
    clusters_list[[i]] <- stations %>%
      filter(segmentation == clusters[i]) %>%
      distinct(id) %>%
      left_join(sensor_locations, by = "id") %>%
      mutate(cluster = as.character(clusters[i]))
  }
  all <- bind_rows(clusters_list)

  ggplot(all, aes(x = latitude, y = longitude, color = cluster)) +
    geom_point() +
    theme_bw()
}
