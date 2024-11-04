## CCM Machine Learning in R Workshop: K-Means Clustering


# df = prepped_heart_df_cluster (only contains numeric/continuous variables)

# look at correlations within the clustering dataset
corrplot(cor(prepped_heart_df_cluster))

set.seed(66)

# perform k-means clustering
heart_kmeans <- kmeans(prepped_heart_df_cluster, centers = 3, nstart = 25)

# The kmeans() function returns an object of class “kmeans” with information about the partition:
#   1. cluster. A vector of integers indicating the cluster to which each point is allocated.
#   2. centers. A matrix of cluster centers.
#   3. size. The number of points in each cluster.

heart_kmeans$cluster
heart_kmeans$centers
heart_kmeans$size

# visualize the clusters
fviz_cluster(heart_kmeans, data = prepped_heart_df_cluster, ellipse.type = "convex")

## what is the optimal number of clusters?
fviz_nbclust(
  prepped_heart_df_cluster, 
  kmeans)

## run k-means clustering with 2 clusters
heart_kmeans_optimal = kmeans(prepped_heart_df_cluster, centers = 2, nstart = 25)
fviz_cluster(heart_kmeans_optimal, data = prepped_heart_df_cluster, ellipse.type = "convex")

# look at cluster characteristics in the updated clustering model - what can you tell about the sub-groups? 
heart_kmeans_optimal$cluster
heart_kmeans_optimal$centers
heart_kmeans_optimal$size


