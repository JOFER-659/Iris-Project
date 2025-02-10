#####
# K-means Clustering of iris data
#####
# BY: Jacob Hofer
#####

#Library packages
library(datasets)
library(tidyverse)

#First lets load and scale our data 
IrisData <- as.data.frame(scale(iris[1:4]))
IrisData2 <- bind_cols(IrisData, Species = iris[,5])

#Lets find the optimal number of clusters using elbow method
#Keep in mind we know the "True" number of clusters is 3

maxclusters <- 10
wss <- numeric()

for(i in 1:maxclusters){
  output <- kmeans(IrisData, centers = i, nstart = 30)
  wss[i] <- output$tot.withinss
}

scree_df <- data.frame(ncluster = 1:maxclusters, wss = wss)

ggplot(scree_df, aes(x = ncluster, y = wss)) + geom_path() + theme_minimal() +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) + 
  labs(title = 'Scree Plot (Number of Clusters vs. Total Within Cluster SS',
       x = 'Number of Clusters', y = 'Total Within Cluster SS')

#Lets use 3 clusters, which is identified as a good elbow, as well as we know that 
#there are 3 distrinct types of flowers anyway

clustering.output <- kmeans(IrisData, centers = 3, nstart = 50)


#Now lets visualize our clusters using PCA
PCAresult <- prcomp(IrisData)
PCAresultWlabels <- bind_cols(PCAresult$x, clustering.output$cluster)
colnames(PCAresultWlabels)[colnames(PCAresultWlabels) == "...5"] <- "Cluster"
PCAresultWlabels$Cluster <- factor(PCAresultWlabels$Cluster)

ggplot(PCAresultWlabels, aes(x = PC1, y = PC2, col = Cluster)) + geom_point() +
  theme_minimal() +
  labs(title = 'K-means Clustering in PCA Space', x = 'PC1', y = 'PC2')
