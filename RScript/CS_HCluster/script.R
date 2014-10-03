CS_HCluster <- function(data,x,num)
{
library(clValid)
Dist <- dist(data[,x],method="euclidean")
clusterObj <- hclust(Dist,method="average")
dendro <- as.dendrogram(clusterObj)
cluster <- cutree(clusterObj,num)

#Visualization of upper dendrogram and Dunn index
plot(cut(dendro,h=0.3)$upper,main=paste('The Dunn Index:',dunn(Dist,cluster),collapse=""))
summary <- data.frame(paste('The Dunn Index:',dunn(Dist,cluster),collapse=""))
names(summary) <- "Dunn Index"
data$results <- cluster

#Dummy summary variable just to print string
summary <- data.frame(paste('The Dunn Index:',dunn(Dist,cluster),collapse=""))
return(list(out=data,model=summary))
}