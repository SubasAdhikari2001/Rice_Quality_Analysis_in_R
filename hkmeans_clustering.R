## Hierarchical k-mean clustering ##################################################
str(Analysis_working)
landrace_names <- as.character(Analysis_working[[1]])
length(landrace_names)
rownames(df) <- landrace_names
numeric_data <- Analysis_working[sapply(Analysis_working, is.numeric)]
df <- scale(numeric_data)
df <- as.data.frame(df)
rownames(df) <- landrace_names

# Computing hierarchical k-means clustering
library(factoextra)
res.hk <- hkmeans(df, 6, hc.method = "ward.D2", hc.metric = "euclidean")
res.hk <-hkmeans(df, 6)
names(res.hk)
res.hk
# hierarchial tree
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
#k-mean
# Custom color palette (6 colors for 6 clusters)
my_colors <- c("#0273C2", "#EFC000", "#868686", "#7AA6DC", "#003C67", "#cd534c","red")

fviz_cluster(res.hk, 
             palette = my_colors,     
             repel = TRUE, 
             ggtheme = theme_classic()) + 
  labs(title = "PCA with K-means Clustering") + 
  theme_minimal() + 
  scale_y_reverse() +           # Reverse Y-axis
  scale_x_reverse()             # Reverse X-axis

# means and parallel plot 
# Extract cluster centers from hkmeans
cluster_means_hk <- data.frame(Cluster = 1:nrow(res.hk$centers), res.hk$centers)
print(cluster_means_hk)
# Add cluster assignments to the original (numeric) data
Analysis_working_numeric$HKCluster <- res.hk$cluster

# means of original data for each HK cluster
hk_cluster_means_original <- aggregate(. ~ HKCluster, data = Analysis_working_numeric, FUN = mean)

# Converting cluster column to factor
hk_cluster_means_original$HKCluster <- as.factor(hk_cluster_means_original$HKCluster)


print(hk_cluster_means_original)
View(hk_cluster_means_original)
library(GGally)
library(ggplot2)

ggparcoord(data = hk_cluster_means_original,
           columns = 2:ncol(hk_cluster_means_original),  
           groupColumn = "HKCluster",
           alphaLines = 0.7) +
  geom_line(linewidth = 0.67)+
  theme_minimal() +
  labs(title = "Parallel Coordinate Plot of HKMeans Cluster Means",
       x = "Rice Quality & Grain Traits",
       y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("#0273C2", "#EFC000", "#868686", "#7AA6DC", "#003C67", "#cd534c"))


######################finished#####################################################
####intra and inter cluster distance########
# intra-cluster distances
compute_intra_cluster_d2 <- function(data, clusters, centers) {
  intra_d2 <- numeric(length = length(unique(clusters)))
  
  for (k in unique(clusters)) {
    cluster_points <- data[clusters == k, , drop = FALSE]
    center <- centers[k, , drop = FALSE]
    dists <- rowSums((cluster_points - center)^2)
    intra_d2[k] <- mean(dists)
  }
  
  return(data.frame(Cluster = 1:length(intra_d2), Intra_D2 = intra_d2))
}

intra_d2_results <- compute_intra_cluster_d2(df, res.hk$cluster, res.hk$centers)
print(intra_d2_results)
#inter-cluster distances between all pairs
compute_inter_cluster_d2 <- function(centers) {
  k <- nrow(centers)
  dist_matrix <- matrix(0, nrow = k, ncol = k)
  
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      d2 <- sum((centers[i, ] - centers[j, ])^2)
      dist_matrix[i, j] <- d2
      dist_matrix[j, i] <- d2  
    }
  }
  colnames(dist_matrix) <- rownames(dist_matrix) <- paste0("Cluster_", 1:k)
  return(as.data.frame(dist_matrix))
}

inter_d2_matrix <- compute_inter_cluster_d2(res.hk$centers)
print(inter_d2_matrix)
library(reshape2)
library(ggplot2)

# Convert to long format for heatmap
dist_long <- melt(as.matrix(inter_d2_matrix))
ggplot(dist_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Inter-cluster Squared Euclidean Distance", x = "", y = "")

#####

























