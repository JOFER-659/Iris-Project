######
# DBScan of iris data
######
# BY: Jacob Hofer
######

#Loading necessary packages
library(tidyverse)
library(datasets)
library(fpc)
library(dbscan)

#Load and Scale Iris Dataset (IrisData2 includes true species)
IrisData <- as.data.frame(scale(iris[1:4]))
IrisData2 <- bind_cols(IrisData, Species = iris[,5])

#Before fitting our dbscan, we want to find the optimal values for hyper parameter eps
#We will do this using the knearest neighbor distances plot and look for an elbow
kNNdistplot(IrisData, k = 4)
abline(h = 0.79, col = "red")

#Minimum Points is another hyperparameter, but a general rule I've seen is use
#two times the number of features, in this case 4*2 = 8.

#Now we can fit the dbscan
db.output <- fpc::dbscan(IrisData, eps = 0.79, MinPts = 8)

#Use PCA so we can visualize in 2 dimensions
PCAresult <- prcomp(IrisData)
IrisData.output <- bind_cols(IrisData, PCAresult$x ,db.output$cluster)
colnames(IrisData.output)[colnames(IrisData.output) == "...9"] <- "Cluster"
IrisData.output$Cluster <- factor(IrisData.output$Cluster)

ggplot(IrisData.output, aes(x = PC1, y = PC2, col = Cluster)) + geom_point() +
  theme_minimal() + 
  labs(title = 'DBScan Clustering in PC Space', x = 'PC1', y = 'PC2') +
  scale_color_manual(values = c("1" = "purple", "2" = 'lightblue', "0" = 'black'),
                     labels = c("Outliers", "Cluster 1", "Cluster 2"))

#We see that the algorithm has found 2 distinct clusters, from my knowledge of the
#data set it has combined versicolor and virginica into one cluster and left setosa
#on its own. There were 'outliers' found outside of both groups, this means they
#are not associated with a group, their distance was too great to include them.
