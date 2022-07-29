# 9. Consider the USArrests data. We will now perform hierarchical clustering
# on the states.
# (a) Using hierarchical clustering with complete linkage and
# Euclidean distance, cluster the states.
data <- USArrests
hc.complete <- hclust(dist(data), method = "complete")
plot(hc.complete ,main = "Complete Linkage", xlab = "", sub = "",
     cex =.9)
abline(h=125, col="red")
abline(h=60, col="red")
# (b) Cut the dendrogram at a height that results in three distinct
# clusters. Which states belong to which clusters?
data$clusters <- cutree(hc.complete, 3)
cutree(hc.complete, 3)
data[which(data$clusters==3),]
# (c) Hierarchically cluster the states using complete linkage and Euclidean
# distance, after scaling the variables to have standard deviation
# one.
x <- as.data.frame(scale(data[,1:4]))
hc.complete <- hclust(dist(x), method = "complete")
plot(hc.complete ,main = "Complete Linkage", xlab = "", sub = "",
     cex =.9)
abline(h=2.8, col="red")
x$clusters <- cutree(hc.complete, 3)
# (d) What effect does scaling the variables have on the hierarchical
# clustering obtained? In your opinion, should the variables be
# scaled before the inter-observation dissimilarities are computed?
# Provide a justification for your answer.
table(x$clusters, data$clusters)
#####################################################################
# 11. On the book website, www.StatLearning.com, there is a gene expression
# data set (Ch10Ex11.csv) that consists of 40 tissue samples with
# measurements on 1,000 genes. The first 20 samples are from healthy
# patients, while the second 20 are from a diseased group.
# (a) Load in the data using read.csv(). You will need to select
# header=F.
df <- read.csv("C:/Users/its/Desktop/Ilias/Courses/Statistical learning/Ch10Ex11.csv", header = FALSE)
# (b) Apply hierarchical clustering to the samples using correlation based
# distance, and plot the dendrogram. Do the genes separate
# the samples into the two groups? Do your results depend on the
# type of linkage used?
dd <- as.dist(1 - cor(df))
plot(hclust(dd, method ="complete"), main = "Complete Linkage
       with Correlation -Based Distance", xlab="", sub ="")
cutree(hclust(dd, method ="complete"), 2)
plot(hclust(dd, method ="single"), main = "Single Linkage
       with Correlation -Based Distance", xlab="", sub ="")
cutree(hclust(dd, method ="single"), 2)
plot(hclust(dd, method ="average"), main = "Average Linkage
       with Correlation -Based Distance", xlab="", sub ="")
cutree(hclust(dd, method ="average"), 2)
###################################################################
dd <- dist(t(df))
plot(hclust(dd, method ="complete"), main = "Complete Linkage
       with eucledian Distance", xlab="", sub ="")
abline(h=12, col="red")
cutree(hclust(dd, method ="complete"), 2)
plot(hclust(dd, method ="single"), main = "Single Linkage
       with eucledian Distance", xlab="", sub ="")
cutree(hclust(dd, method ="single"), 2)
plot(hclust(dd, method ="average"), main = "Average Linkage
       with eucledian Distance", xlab="", sub ="")
################################################################

# (c) Your collaborator wants to know which genes differ the most
# across the two groups. Suggest a way to answer this question,
# and apply it here.
library(pheatmap)
pheatmap(df,fontsize =0.1,
         show_colnames = FALSE, cluster_cols = F,cluster_rows = F,
         cellheight=0.25,cellwidth=6)
#################################################################
###################################################################
data_frame_new <- as.data.frame(t(df))
data_frame_new$state <- c(rep(0,20),rep(1,20))
#################################################################

#################################################################
km.out <- kmeans(data_frame_new[,1:1000], 2, nstart =20)
a <- km.out$centers
plot(abs(a[1,]-a[2,]))
abline(h=1.3, col="red")
which(abs(a[1,]-a[2,])> 1.3)

#################################################################     
data_frame_new_new <- data_frame_new[,which(abs(a[1,]-a[2,])>2.5)]
km.out <- kmeans(data_frame_new_new, 2, nstart =20)
km.out$cluster
