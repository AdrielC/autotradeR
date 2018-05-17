
# EDA_functions -----------------------------------------------------------

elbow_plot <- function(data, K = 7) {
  # Produce K k-means solutions.
  seg_sols <- vector("double", length = K)
  for (k in seq_along(seg_sols)) {
    out <- data %>% 
      kmeans(centers = k, nstart = 20, iter.max = 50)
    seg_sols[k] <- out$tot.withinss
  }
  # Produce the elbow plot.
  plot(seg_sols, type = "b", xlab = "k")
}
