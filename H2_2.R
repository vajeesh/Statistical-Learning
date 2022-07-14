library(ISLR)
library(MASS)
library(e1071)
library(vctrs)
library(tree)
library(tidyverse)
library(readxl)
library(tibble)
library(dplyr)
library(VIM)
library(caret)
rm(list = ls())
data <-read_excel("C:/Users/Maha Vajeeshwaran/Desktop/STATISTICAL LEARNING/ASSIGNMENT 2/Data_Cortex_Nuclear.xls")
view(data)

## Replacing the null values
data <- data %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
sum(is.na(data)) == 0
summary(data)

# Removing the index
data <- data[,c(2:82)]

attach(data)
data$Genotype <- as.factor(Genotype)
data$Treatment <- as.factor(Treatment)
data$Behavior <- as.factor(Behavior)
data$class <- as.factor(class)
detach(data)
set.seed(1000)

# Determine number of clusters
#WSS means the sum of distances between the points and 
#the corresponding centroids for each cluster and BSS means the sum of distances 
#between the centroids and the total sample mean multiplied by the number of points within each cluster. 

wss <- (nrow(data[,-c(78:81)])-1)*sum(apply(data[,-c(78:81)],2,var))
wss
for (i in 1:15) wss[i] <- sum(kmeans(data[,-c(78:81)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


data_Cluster <- kmeans(data[,-c(78:81)], 2, nstart = 20)
#data_Cluster <- kmeans(data[,-c(78:81)], 8, nstart = 20)
data_Cluster

cluster_data<-data
cluster_data<-cbind(cluster_data,data_Cluster$cluster)

table(data_Cluster$cluster, cluster_data$Treatment)
table(data_Cluster$cluster, cluster_data$Behavior)
table(data_Cluster$cluster, cluster_data$Genotype)
table(data_Cluster$cluster, cluster_data$class)

#-------------------------------------------
# Hieracial

#Hierarchical Clustering
s <- dist(data[,-c(77:80)], method = "euclidean") # distance matrix
fit <- hclust(s, method="single")
plot(fit) 

A <- dist(data[,-c(77:80)], method = "euclidean") # distance matrix
fit <- hclust(A, method="average")
plot(fit)                                        

c <- dist(data[,-c(77:80)], method = "euclidean") # distance matrix
fit <- hclust(A, method="complete")
plot(fit)                                        

# draw dendogram with red borders around the 8 clusters
rect.hclust(fit, k=8, border="green")

groups <- cutree(fit, k=8) # cut tree into 8 clusters

cluster_data<-df
cluster_data<-cbind(cluster_data,groups)

table(cluster_data$groups, cluster_data$Genotype)

#--------------------------------------------------------------------------------  
     
attach(data)
data$Genotype <- as.factor(Genotype)
data$Treatment <- as.factor(Treatment)
data$Behavior <- as.factor(Behavior)
data$class <- as.factor(class)
detach(data)
data <- data[-1] # Drop MouseID
names(data)
Y <- data[,c(77:80)]
X <- data[,-c(77:80)]
pca.out <- prcomp(X, scale = TRUE)
head(pca.out[0])

#View(pca.out)
var_explained <- pca.out$sdev / sum(pca.out$sdev)
cum_var_explained <- cumsum(var_explained)

plot(var_explained)
plot(cum_var_explained)
abline(h = 0.9, col = "red")
pca.out$x

result <- c()
nr_clusters <- c(1:30)
for (i in nr_clusters){
  km.out <- kmeans(pca.out$x, i, nstart = 15)
  result <- c(result,km.out$tot.withinss)
}
plot(nr_clusters, result)

km.out <- kmeans(pca.out$x, 4, nstart = 15)
km.out$cluster

pc1 <- pca.out$x[,1]
pc2 <- pca.out$x[,2]

kmeans_2 <- data.frame(pc1, pc2, km.out$cluster)
plot(pc1, pc2, col = km.out$cluster, pch = 20)
plot(pc1,pc2, col = data$Genotype, pch =20)
plot(pc1,pc2, col = data$Treatment, pch =20)
plot(pc1,pc2, col = data$Behavior, pch =20)
plot(pc1,pc2, col = data$class, pch =20)



result <- c()
nr_clusters <- c(1:30)
for (i in nr_clusters){
  km.out <- kmeans(pca.out$x, i, nstart = 15)
  result <- c(result,km.out$tot.withinss)
}
plot(nr_clusters, result)
km.out <- kmeans(pca.out$x, 4, nstart = 15)
plot(pc1, pc2, col = km.out$cluster, pch = 19)
km.result <- data.frame(X, km.out$cluster)
names(km.result)

c1 <- km.result[km.result$km.out.cluster == 1,]
c2 <- km.result[km.result$km.out.cluster == 2,]
c3 <- km.result[km.result$km.out.cluster == 3,]
c4 <- km.result[km.result$km.out.cluster == 4,]
rbind(lapply(c1, mean), lapply(c2, mean), lapply(c3,mean), lapply(c4,mean))

methods <- c("single", "average", "complete")
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "black")

hc <- hclust(dist(pca.out$x), method = 'single')
plot(hc, ylab = "Height", nodePar = nodePar, leaflab = "none")
hc <- hclust(dist(pca.out$x), method = 'average')
plot(hc, ylab = "Height", nodePar = nodePar, leaflab = "none")
hc <- hclust(dist(pca.out$x), method = 'complete')
plot(hc, ylab = "Height", nodePar = nodePar, leaflab = "none")

# complete linkage feels better when compared to others