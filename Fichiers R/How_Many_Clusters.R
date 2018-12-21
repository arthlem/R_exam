#Inspired by:
# http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning#elbow-method


data("USArrests")
data<-USArrests

# The Elbow method
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", main="Elbow method")
abline(v = 3, lty =2)
text(6,590, "Optimal number of Clusters")


# Silouhette method
library(cluster)

k.max <- 15

sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k", main="Optimal number of clusters")
abline(v = which.max(sil), lty = 2)

# Gap Statistic
gap_stat <- clusGap(data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k", main="Optimal number of clusters")
#abline(v = 3, lty = 2)


# NbClust Package
library("NbClust")
set.seed(123)
# The following example determine the number of clusters using gap statistics:
res.nb <- NbClust(data, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete", index ="gap") 
res.nb # print the results

#Compute all the 30 indices
nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
# Print the result
nb
