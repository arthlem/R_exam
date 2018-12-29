################################################################################
#               ICHEC - Cours MQ1 - Ann?e Acad?mique 2016-2017                 #
################################################################################
#                   Les Algorithmes de Clustering                              #
#                                                                              #
################################################################################
# Remarque importante : les commandes R qui seront d?taill?es ci-dessous 
# sont (au choix):
# - ? recopier manuellement dans l'interface R choisie (option pas si 
#   inint?ressante en termes d'apprentissage)
# - ? copier puis ? coller dans l'interface R,
# - ? envoyer du fichier vers l'interface en utilisant la fonction ad hoc de 
#   l'outil choisi (RStudio, Tinn-R, ...)
################################################################################
# Le?on du 26/04/2017                                                          #
################################################################################ 
# Pr?paration des donn?es                                      
# Pour cette introduction au clustering nous allons continuer ? utiliser les
# Iris de Fisher. 
# Mais pour les besoins de la repr?sentation nous commencerons par une ACP
# en ajoutant les composantes au dataset existant.
# (Some parts of this file are in english, sorry for the vandammisation)
################################################################################

#data(iris)
data("USArrests")
mydata<-USArrests
# Downloading and loading package which permits to perform an more detailled PCA 
# than using the built-in function princomp:

#install.packages("ade4")# Uncomment if necessary
# Loading :)
library(ade4)
# If you wish to skip the interactive step just set 'scannf' to FALSE and give 
# directly the number of axes to keep via nf.
pca<-dudi.pca(mydata[,1:4], scannf=FALSE, nf=4,center = TRUE, scale = TRUE )
cumsum(pca$eig/sum(pca$eig)*100)
inertia.dudi(pca,col.inertia = T)$col.abs
s.corcircle(pca$co, fullcircle = TRUE)
s.corcircle(pca$co, fullcircle = FALSE)
scatter(pca, posieig="none")
scatter(pca, posieig="none", clab.row = 0)

# Adding the new components in the original data frame for using them to 
# draw the clustering results
mydata<-cbind(mydata,pca$li)


################################################################################
# KMeans
################################################################################

# Loading the Miscelaneous libray of Rcommander to use KMeans insted kmeans:
# - kmeans: default R command for kmeans. One execution of the kmeans algorithms
# - KMeans: wrapper function from library RcmdrMisc to perform several kmeans 
#           keeping the clustering with the best criterion

#install.packages("RcmdrMisc")# Uncomment if necessary
#library(RcmdrMisc)
# KMeans with k=3, 10 iterations for each kmeans, 10 kmeans tried..
km1 <- kmeans(mydata[,1:4], centers = 3, iter.max = 10, nstart = 10)
# Size of the clusters
table(km1$cluster)
# Clusters Centers
km1$centers
# Plot of the clusters
pairs(mydata[,1:4],col=km1$cluster)
# Chargement du package "car" pour utiliser sa fonction scatterplotMatrix
library(car)
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=km1$cluster, by.groups=TRUE)

# Representation of clusters in the 2 first principal components
plot(mydata[,c("Axis1","Axis2")], col=km1$cluster, main="K-means")


# Same algorithm, but on scaled data.
km2 <- kmeans(scale(mydata[,1:4],center = TRUE,scale=TRUE), centers = 3, iter.max = 10, nstart = 10)
# Size of the clusters
table(km2$cluster)
# Clusters Centers (with no direct meaning!)
km2$centers
# Cluster center on initial variables
aggregate(mydata[,1:4], list(km2$cluster), mean)

# Representation of clusters in the 2 first principal components
plot(mydata[,c("Axis1","Axis2")], col=km2$cluster)
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=km2$cluster, by.groups=TRUE)
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=km2$cluster, by.groups=FALSE)
cor(mydata[,1:4])

# Comparison of the two results
plot(mydata[,c("Axis1","Axis2")], col=km1$cluster, main="K-means")
plot(mydata[,c("Axis1","Axis2")], col=km2$cluster, main="K-means on scaled data")

# Plot with labels
plot(mydata[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(mydata[,c("Axis1","Axis2")], labels=rownames(mydata), col=km2$cluster, main="K-means on scaled data", cex=0.7)


################################################################################
# Hierarchical Clustering
################################################################################

# Computing the distance matrix
#mydata.dist<- dist(mydata[,1:4]) # not so good
mydata.dist<- dist(scale(mydata[,1:4],center = TRUE,scale=TRUE)) # Better

#Hclust with average link
HClust.1 <- hclust(mydata.dist, method="average")
plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab=
       "Observation Number in Data Set", sub="Method=average; Distance=euclidian")
# Cutting the tree to obtain 3 clusters
hc.1<-cutree(HClust.1, k=3)
# Size of the clusters
table(hc.1)

plot(mydata[,c("Axis1","Axis2")], col=hc.1, main="Clusters with average link." )

# Plot with labels
plot(mydata[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(mydata[,c("Axis1","Axis2")], labels=rownames(mydata), col=hc.1, main="K-means on scaled data", cex=0.7)

scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=hc.1, by.groups=TRUE)

# HClsut single link
HClust.2 <- hclust(mydata.dist , method= "single")
plot(HClust.2, main= "Cluster Dendrogram for Solution HClust.2", xlab=
       "Observation Number in Data Set", sub="Method=single; Distance=euclidian")
hc.2<-cutree(HClust.2, k=3)
# Size of the clusters
table(hc.2)

plot(mydata[,c("Axis1","Axis2")], col=hc.2, main="Clusters with single link." )
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=hc.2, by.groups=TRUE)

# HClust complete link
HClust.3 <- hclust(mydata.dist, method="complete")
plot(HClust.3, main= "Cluster Dendrogram for Solution HClust.3", xlab=
       "Observation Number in Data Set", sub="Method=complete; Distance=euclidian")
hc.3<-cutree(HClust.3, k=3)
# Size of the clusters
table(hc.3)

plot(mydata[,c("Axis1","Axis2")], col=hc.3, main="Clusters with complete link." )
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=hc.3, by.groups=TRUE)

# Plot with labels
plot(mydata[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(mydata[,c("Axis1","Axis2")], labels=rownames(mydata), col=hc.3, main="K-means on scaled data", cex=0.7)


# HClust complete link
HClust.4 <- hclust(mydata.dist, method="ward.D")
plot(HClust.4, main= "Cluster Dendrogram for Solution HClust.4", xlab=
       "Observation Number in Data Set Iris", sub="Method=Ward; Distance=euclidian")
hc.4<-cutree(HClust.4, k=3)
# Size of the clusters
table(hc.4)

plot(mydata[,c("Axis1","Axis2")], col=hc.4, main="Clusters with complete link." )
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=hc.4, by.groups=TRUE)

# Plot with labels
plot(mydata[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(mydata[,c("Axis1","Axis2")], labels=rownames(mydata), col=hc.4, main="K-means on scaled data", cex=0.7)


################################################################################
# Mixture Decomposition using Model Based Clustering Package
################################################################################

#install.packages("mclust")# Uncomment if necessary
#load MCLUST library
library(mclust)
# Mclust on the 4th firts columns of the data
mclust.1 <- Mclust(scale(mydata[,1:4],center = TRUE,scale=TRUE), G=3)

# A summary of the chosen model 
summary(mclust.1)

Mc.1<-mclust.1$classification

plot(mydata[,c("Axis1","Axis2")], col=Mc.1, main="Clusters calcul?s pas MCLUST." )
scatterplotMatrix(mydata[,1:4],smooth=FALSE,groups=Mc.1, by.groups=TRUE)

# Plot with labels
plot(mydata[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(mydata[,c("Axis1","Axis2")], labels=rownames(mydata), col=Mc.1, main="K-means on scaled data", cex=0.7)



################################################################################
# Clustering Spectral
################################################################################

# Compute the distance matrix
distances = as.matrix(mydata.dist)
# Compute Weght matrix using normal distance
W = exp(-distances^2)
# Compute sum of rows and form a diagonal matrix with them.
G = diag(rowSums(W))
# Compute the basic laplacian
L = G - W
# solve(G) gives the inverse of G
# %*% is the matrix product
L=solve(G)%*%L
# eigen(L) compute the spectral decomposition of L
eig = eigen(L)
N<-dim(mydata)[1]
k<-3
# kmeans!
#     joint  <-the 3 last eigen vectors->
km2 = kmeans(cbind(eig$vectors[,(N-k+1):N]),centers=3,iter.max=20,nstart=5)
Sp<-km2$cluster
plot(mydata[,c("Axis1","Axis2")], col=Sp, main="Clusters spectraux." )
pairs(mydata[,1:4],col=Sp)
library(rgl)
plot3d(mydata[,1:3], type = "p", col = Sp)
plot3d(mydata[,c(1,2,4)], type = "p", col = Sp)

################################################################################
# Exercices: Wholesale Data 
################################################################################
# Le dataset Wholesale_customers_data contient les donn?es d'un grossiste 
# portugais. La description compl?te de ce fichier peut ?tre trouv?e ? l'adresse
# suivante : http://archive.ics.uci.edu/ml/datasets/Wholesale+customers

Wholesale <- read.csv("C:/Users/EC/Dropbox/Teaching/ICHEC/MQ/Data/Wholesale/Wholesale customers data.csv")
View(Wholesale)
table(Wholesale[,"Channel"])
table(Wholesale[,"Region"])
Fresh <-Wholesale[,"Fresh"]
Milk <-Wholesale[,"Milk"]
Grocery <-Wholesale[,"Grocery"]
Frozen <-Wholesale[,"Frozen"]
Detpaper <-Wholesale[,"Detergents_Paper"]
Delicassen <-Wholesale[,"Delicassen"]
Channel <-Wholesale[,"Channel"]
Region <-Wholesale[,"Region"]
# Fresh
plot(density(Fresh),main="D?penses annuelles en Fresh", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Fresh",xlab="D?penses annuelles",add=TRUE, freq=FALSE)
# Milk
plot(density(Milk),main="D?penses annuelles en Milk", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Milk",xlab="D?penses annuelles",add=TRUE, freq=FALSE)
# Grocery
plot(density(Grocery),main="D?penses annuelles en Grocery", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Grocery",xlab="D?penses annuelles",add=TRUE, freq=FALSE)
# Frozen
plot(density(Frozen),main="D?penses annuelles en Frozen", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Frozen",xlab="D?penses annuelles",add=TRUE, freq=FALSE)
# Detpaper
plot(density(Detpaper),main="D?penses annuelles en Detergents & Paper", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Detergents & Paper",xlab="D?penses annuelles",add=TRUE, freq=FALSE)
# Delicassen
plot(density(Delicassen),main="D?penses annuelles en Delicassen", xlab="D?penses annuelles")
hist(Fresh,main="D?penses annuelles en Delicassen",xlab="D?penses annuelles",add=TRUE, freq=FALSE)

library(ade4)
pairs(Wholesale[,3:8]) 
library(car)
scatterplotMatrix(Wholesale[,3:8],smooth=FALSE)
cor(Wholesale[,3:8]) 
acp1<-dudi.pca(Wholesale[,3:8], scannf=FALSE, nf=6,center = TRUE, scale = TRUE )
acp1$eig/sum(acp1$eig)*100
cumsum(acp1$eig/sum(acp1$eig)*100)
barplot(acp1$eig/sum(acp1$eig)*100)
inertia.dudi(acp1,col.inertia = T)$col.abs
score(acp1, xax=1)
score(acp1, xax=2)
s.corcircle(acp1$co)
#library(RcmdrMisc)
Wholesale<-cbind(Wholesale,acp1$li)
s.corcircle(acp1$co)
#hist(Fresh,main="D?penses annuelles en Delicassen",xlab="D?penses annuelles", freq=FALSE)
km1<-kmeans(Wholesale[,3:8],3,iter.max = 10, nstart = 10)
table(km1$cluster)
km1$centers
plot(Wholesale[,c("Axis1","Axis2")], col=km1$cluster, main="K-means - 3 clusters")
layout(1)
barplot(acp1$eig/sum(acp1$eig)*100)
#library(cluster)
#clusplot(Wholesale[,c("Axis1","Axis2")], km1$cluster, color=TRUE,col.p ="black", shade=TRUE, labels=1, lines=0,main="K-means - 3 clusters")


library(rgl)
acp2<-dudi.pca(Wholesale[,3:8], scannf=FALSE, nf=6,center = TRUE, scale = TRUE )
WholeSale<-cbind(Wholesale[,1:8],acp2$li)
km2 <- kmeans(scale(WholeSale[,9:14],center = F, scale=F), centers = 3, iter.max = 10, nstart = 10)

# Graphique 3D des 3 premi?res variables des donn?es: 
plot3d(WholeSale[,c("Axis1","Axis2", "Axis3")], type = "p",size=10, col = km2$cluster)
Qinv<-solve(acp2$co)
lines3d(rbind(c(0,0,0),Qinv[1:3,1]*5),lwd=4, col="green")
lines3d(rbind(c(0,0,0),Qinv[1:3,2]*5),lwd=4, col="grey")
lines3d(rbind(c(0,0,0),Qinv[1:3,3]*5),lwd=4, col="brown")
lines3d(rbind(c(0,0,0),Qinv[1:3,4]*5),lwd=4, col="blue")
lines3d(rbind(c(0,0,0),Qinv[1:3,5]*5),lwd=4, col="yellow")
lines3d(rbind(c(0,0,0),Qinv[1:3,6]*5),lwd=4, col="orange")
text3d(rbind(Qinv[1:3,1]*4.5),texts = colnames(Wholesale[,3:8])[1], col="black")
text3d(rbind(Qinv[1:3,2]*4.5),texts = colnames(Wholesale[,3:8])[2], col="black")
text3d(rbind(Qinv[1:3,3]*4.5),texts = colnames(Wholesale[,3:8])[3], col="black")
text3d(rbind(Qinv[1:3,4]*4.5),texts = colnames(Wholesale[,3:8])[4], col="black")
text3d(rbind(Qinv[1:3,5]*4.5),texts = colnames(Wholesale[,3:8])[5], col="black")
text3d(rbind(Qinv[1:3,6]*4.5),texts = colnames(Wholesale[,3:8])[6], col="black")


################################################################################
# Exercices:
################################################################################


# 1) Effectuez un clustering l'analyse de ses r?sultats pour les donn?es USArrest des TP pr?c?dents 
#    (pour les 4 premi?re variables seulement).

# 2) idem pour les donn?es swiss:
data(swiss, package="datasets")

