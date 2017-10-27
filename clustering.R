library(dbscan)
library(cluster)
library(factoextra)

dim(iris)

iris_df <- iris[,1:4]
head(iris_df)

iris_mat <- as.matrix(iris_df)
iris_mat_scaled <- scale(iris_mat)
head(iris_mat_scaled)
iris_scaled_df <- as.data.frame(iris_mat_scaled)
str(iris_scaled_df)

dbscan::kNNdistplot(iris_scaled_df)
# inflection around 550
abline(h = 0.7, v = 550)

cl <- dbscan(iris_scaled_df, eps = 0.7, minPts = 5)
cl
# 2 clusters and 34 noise points
pairs(iris_scaled_df, col = cl$cluster + 1L)

factoextra::fviz_cluster(cl, iris_scaled_df, 
                         ellipse = FALSE,
                         palette = "Set1",
                         labelsize = 10,
                         ggtheme = theme_minimal())

str(cl)
clusters <- cl$cluster
iris_scaled_df$cluster <- clusters
head(iris_scaled_df)

