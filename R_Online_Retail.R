#-------------------------------------------ONLINE RETAIL CSV-------------------------------------------------
#Libraries to install
pkgs <- c("ggplot2","dplyr","lubridate","ade4","tm","SnowballC","wordcloud","cluster","factoextra","NbClust","RcmdrMisc")
install.packages(pkgs)

#Load the libraries we just downloaded, necessary for the script to work properly
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ade4)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(factoextra)
library(NbClust)
library(car)
library(RcmdrMisc)

#General constants
turnoverByMonthScale <- 1/1000

#----A. IMPORT THE DATA----
onlineRetail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=",", row.names = NULL, fileEncoding = "UTF-8-BOM") #Load CSV File

#Check if the data has been imported correctly
#View(onlineRetail)

#Explore the varibales
#How many variables do we have?
length(onlineRetail)

#What are the names of the different variables? And which type of data do they have?
str(onlineRetail)

#Number of unique InvoiceNo (orders)
length(unique(onlineRetail$InvoiceNo))

#Number of unique StockCode (different products)
length(unique(onlineRetail$StockCode))

#Number of unique Description (different products): To compare with the length of StockCode
length(unique(onlineRetail$Description))

#Analysis of Quantity
summary(onlineRetail$Quantity)

#Check when the records started and when it ended (Start date and End date)
head(select(onlineRetail, InvoiceDate), 10)
tail(select(onlineRetail, InvoiceDate), 10)

#CustomerID, show th enumber of unique Customers
length(unique(onlineRetail$CustomerID))

#Country, show the number of unique countries
length(unique(onlineRetail$Country))

#----B. DATA CLEANING----
#Removing the missing variables (CustomerID that are empty)
onlineRetailClean <- subset(onlineRetail, CustomerID != "")

#Counting the number variables that have been removed (missing CustomerID)
dim(onlineRetail)-dim(onlineRetailClean)
#Percentage of data removed:
paste(round((135080/541909)*100,digit=2), "%", sep="")

#Remove canceled invoices: these invoices are beggining with C
#Count number of records (canceled included)
beforeCancelations <- nrow(onlineRetailClean)
onlineRetailClean <- subset(onlineRetailClean, grepl("^(?!C).*$", onlineRetailClean$InvoiceNo, perl = TRUE))
#Count number of records after having removed the canceled invoices
afterCancelations <- nrow(onlineRetailClean)
#Number of records removed
beforeCancelations - afterCancelations

#Remove other records that aren't orders
#Remove POSTAGE
onlineRetailClean <- subset(onlineRetailClean, StockCode != "POST")

#Remove Discount
onlineRetailClean <- subset(onlineRetailClean, StockCode != "D")

#Remove C2
onlineRetailClean <- subset(onlineRetailClean, StockCode != "C2")

#Remove Manual
onlineRetailClean <- subset(onlineRetailClean, StockCode != "M")

#Remove Bank Charges
onlineRetailClean <- subset(onlineRetailClean, StockCode != "BANK CHARGES")

#Remove PADS 
onlineRetailClean <- subset(onlineRetailClean, StockCode != "PADS")

#Remove DOT
onlineRetailClean <- subset(onlineRetailClean, StockCode != "DOT")

#Remove Unit Price <= 0
onlineRetailClean <- subset(onlineRetailClean, UnitPrice > 0)

#Remove Quantity < 0
onlineRetailClean <- subset(onlineRetailClean, Quantity > 0)

#Remove Duplicates
onlineRetailUnique <- unique(onlineRetailClean)
#Number of duplicate lines removed
dim(onlineRetailClean)-dim(onlineRetailUnique)

#We add a new column to compute the total price
onlineRetailUnique <- onlineRetailUnique %>% 
  mutate(TotalPrice = Quantity*UnitPrice)

#----ANALYSIS OF THE DATA - DESCRIPTIVE STATISTICS----

#1. Number of invoices (orders)
listOfInvoices <- onlineRetailUnique["InvoiceNo"]
length(listOfInvoices[!duplicated(listOfInvoices), ])
#2. Number of products
listOfProducts <- onlineRetailUnique["StockCode"]
length(listOfProducts[!duplicated(listOfProducts), ])
#3. Number of Countries
listOfCountry <- onlineRetailUnique["Country"]
length(listOfCountry[!duplicated(listOfCountry), ])
#4. Number of Customers
listOfCustomers <- onlineRetailUnique["CustomerID"]
length(listOfCustomers[!duplicated(listOfCustomers), ])

#5. Group the amount of purchases for each country
purchasesPerCountry <- aggregate(onlineRetailUnique$Quantity*onlineRetailUnique$UnitPrice, by=list(Category=onlineRetailUnique$Country), FUN=sum)
names(purchasesPerCountry) <- c("Country","SalesCountry")
summary(purchasesPerCountry[2])

#6. Create another variable to analyse the data out of the UK
purchasesNotUk <- purchasesPerCountry[- grep("United Kingdom", purchasesPerCountry$Country),]

#7. Group the quanty and the purchases (and quantity) for each invoices
purchasesPerInvoice <- aggregate(onlineRetailUnique$Quantity*onlineRetailUnique$UnitPrice, by=list(Category=onlineRetailUnique$InvoiceNo), FUN=sum)
quantityPerInvoice <- aggregate(onlineRetailUnique$Quantity, by=list(Category=onlineRetailUnique$InvoiceNo), FUN=sum)
invoiceData <- merge(purchasesPerInvoice, quantityPerInvoice, by="Category")
names(invoiceData) <- c("InvoiceNO","Sales Invoice","Quantity")

#PIECHARTS
#PieChart with all countries
purchaseUk <- purchasesPerCountry[grep("United Kingdom", purchasesPerCountry$Country),]
slices2 <- c(purchaseUk[[2]],sum(purchasesNotUk[[2]]))
lbls2 <- c("United Kingdom", "Others")
pie(slices2, labels = lbls2, main="Pie Chart of Sales")

#Piechart with sales out of the United Kingdom
slicesNotUK <- purchasesNotUk[[2]]
lblsNotUK <- purchasesNotUk[[1]]
pie(slicesNotUK, labels = lblsNotUK, main="Pie Chart of Sales - Out of the UK")

#Piechart with the countries with the most returns Out of the UK
countriesTopSelling <- subset(purchasesNotUk, SalesCountry > 80000)
names(countriesTopSelling) = c("Country", "SalesCountry")
countriesLeastSelling <- subset(purchasesNotUk, SalesCountry <= 80000)
otherCountries <- data.frame(Country="Others",SalesCountry=sum(countriesLeastSelling[[2]]))

countriesTopSelling <- rbind(countriesTopSelling,otherCountries)
slicesTopSelling <- countriesTopSelling[[2]]
lblsTopSelling <- countriesTopSelling[[1]]

pctTopSelling <- round(slicesTopSelling/sum(slicesNotUK)*100)
lblsTopSelling <- paste(lblsTopSelling, pctTopSelling)
lblsTopSelling <- paste(lblsTopSelling,"%",sep="")
pie(slicesTopSelling, labels = lblsTopSelling, main="5 best selling countries out of UK")

#BOXPLOTS
#Sales per product
salesPerProduct <- aggregate(onlineRetailUnique$Quantity*onlineRetailUnique$UnitPrice, by=list(StockCode=onlineRetailUnique$StockCode), FUN=sum)
names(salesPerProduct) <- c("StockCode","Sales Product")
#View(salesPerProduct)

#Boxplot of the sales per product
boxplot(salesPerProduct[2], main= "Sales per product", horizontal = TRUE, outline = FALSE,las=2)
summary(salesPerProduct[2])

#Boxplot of the Quantity per Invoice
boxplot(invoiceData[3], main= "Quantity per invoice", horizontal = TRUE, outline = FALSE,las=2)

#Sales sumary per invoice
summary(invoiceData[2])

#GRAPHS
#Invoices per month in 2011

#Check format of dates
onlineRetailUnique$InvoiceDate <- parse_date_time(onlineRetailUnique$InvoiceDate, c( "%m/%d/%y %H:%M","%d/%m/%y %H:%M"), exact = T)
#Creating object for date's year
onlineRetailUnique$InvoiceYear <- year(onlineRetailUnique$InvoiceDate)
#Creating object for date's month
onlineRetailUnique$InvoiceMonth <- month(onlineRetailUnique$InvoiceDate,label=T)
#Creating object for date's day
onlineRetailUnique$InvoiceWeekday <- wday(onlineRetailUnique$InvoiceDate, label=T)
#Creating object for date's hour
onlineRetailUnique$InvoiceHour <- hour(onlineRetailUnique$InvoiceDate)

#Number of invoices per month in 2011
#Filter to select year 2011 and count invoices for each month
monthData <- onlineRetailUnique %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceMonth)

ggplot(monthData, aes(InvoiceMonth, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="Month", y="Number of invoices")

#Number of invoices per day in 2011
#Filter to select year 2011 and count invoices for each day of the week
dayData <- onlineRetailUnique %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceWeekday)

ggplot(dayData, aes(InvoiceWeekday, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="Week", y="Number of invoices")

#Number of invoices per hour in 2011
#Filter to select year 2011 and count invoices for each hour of the day
hourData <- onlineRetailUnique %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceHour)

ggplot(hourData, aes(InvoiceHour, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="hour", y="Number of invoices")

#Turnover per month in 2011
SalesData <- onlineRetailUnique %>%
  dplyr::filter(InvoiceYear == 2011) %>%
  group_by(InvoiceMonth) %>%
  summarise(CA = sum(TotalPrice))

ggplot(SalesData, aes(InvoiceMonth, CA*turnoverByMonthScale)) +              
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=format(round(CA*turnoverByMonthScale, 2), nsmall = 2)), vjust=1.6, color="white", size=3.5)

#-------------------------------------------PCA-------------------------------------------
#----PART 1: PREPARE DATASET FOR PCA----

#Create a dataset per PRODUCT (StockCode // (a)sum of Quantity / (b)Turnover[Quantity*UnitPrice] / (c)Count of Customers / (d)AvgUnitPrice / (e)NbOfCountry )

#(a)Create a dataset with aggregate() by combining the StockCode with the Quantity and then change the variables names with names()
stockPerQuantity <- aggregate(onlineRetailUnique$Quantity, by=list(Category=onlineRetailUnique$StockCode), FUN=sum)
names(stockPerQuantity) <- c("StockCode","Quantity")

#(b)Create a dataset with aggregate() by combining the StockCode with the Quantity*UnitPrice and then change the variables names with names()
stockPerPurchases <- aggregate(onlineRetailUnique$Quantity*onlineRetailUnique$UnitPrice, by=list(Category=onlineRetailUnique$StockCode), FUN=sum)
names(stockPerPurchases) <- c("StockCode","Purchases")

#(c)Create a dataset with aggregate() by combining the StockCode with the NbOfCustomers and then change the variables names with names()
stockPerCustomers <- aggregate(onlineRetailUnique$CustomerID, by=list(Category=onlineRetailUnique$StockCode), FUN=function(x) length(unique(x)))
names(stockPerCustomers) <- c("StockCode","NbOfCustomers")

#(d)Create a dataset with aggregate() by combining the StockCode with the UnitPrice and then change the variables names with names()
stockPerUnitPrice <- aggregate(onlineRetailUnique$UnitPrice, by=list(Category=onlineRetailUnique$StockCode), FUN=mean)
names(stockPerUnitPrice) <- c("StockCode","Avg UnitPrice")

#(e)Create a dataset with aggregate() by combining the StockCode with the Country and then change the variables names with names()
stockPerCountry <- aggregate(onlineRetailUnique$Country, by=list(Category=onlineRetailUnique$StockCode), FUN=function(x) length(unique(x)))
names(stockPerCountry) <- c("StockCode","NbOfCountry")

#Merge the dataset to make productData
productData <- merge(stockPerQuantity, stockPerPurchases, by="StockCode")
productData <- merge(productData, stockPerCustomers, by="StockCode")
productData <- merge(productData, stockPerUnitPrice, by="StockCode")
productData <- merge(productData, stockPerCountry, by="StockCode")
row.names(productData) <- productData$StockCode
productData <- productData[2:6]

#Merged dataset for our PCA of the Products
View(productData)

#Create a dataset per COUNTRY (Country // (a)NbOfProduct/ (b)Purchases/ (c)NbOfCustomers)

#(a) Create a dataset with aggregate() by combining the Country with the nbOfStockCode and then change the variables names with names()
countryPerStockCode <- aggregate(onlineRetailUnique$StockCode, by=list(Category=onlineRetailUnique$Country), FUN=function(x) length(unique(x)))
names(countryPerStockCode) <- c("Country","NbOfProduct")

#(b) Create a dataset with aggregate() by combining the Country with the Quantity*UnitPrice and then change the variables names with names()
countryPerPurchases <- aggregate(onlineRetailUnique$Quantity*onlineRetailUnique$UnitPrice, by=list(Category=onlineRetailUnique$Country), FUN=sum)
names(countryPerPurchases) <- c("Country","Turnover")

#(c) Create a dataset with aggregate() by combining the Country with the nbOfCustomers and then change the variables names with names()
countryPerCustomers <- aggregate(onlineRetailUnique$CustomerID, by=list(Category=onlineRetailUnique$Country), FUN=function(x) length(unique(x)))
names(countryPerCustomers) <- c("Country","NbOfCustomers")

#(d) Create a dataset with aggregate() by combining the Country with the nbOfInvoice and then change the variables names with names()
countryPerInvoice <- aggregate(onlineRetailUnique$InvoiceNo, by=list(Category=onlineRetailUnique$Country), FUN=function(x) length(unique(x)))
names(countryPerInvoice) <- c("Country","NbOfInvoice")

#Merge the dataset to make countryData
countryData <- merge(countryPerStockCode, countryPerPurchases, by="Country")
countryData <- merge(countryData, countryPerCustomers, by="Country")
countryData <- merge(countryData, countryPerInvoice, by="Country")
row.names(countryData) <- countryData$Country
countryData <- countryData[2:5]

#Merged dataset for our PCA of the Countries
View(countryData)

#----PART 2: GET INTO PCA----

#----PCA: PRODUCT----

#Plot of a matrix of data
pairs(productData)

#Returns the matrix of correlations (purement informatif)
cor(productData)

#Center and scale data
productData.CR<-scale(productData,center=TRUE,scale=TRUE)
#Perform the pca and returns the results as an object
pcaProduct <- princomp(productData.CR)

#Variance-covariance of scaled and centered variables
covarProduct<-cov(productData.CR)
#summary of the componnent
summary(pcaProduct)

#visual of the summary
plot(pcaProduct)

#weight of the original variables in the component
loadings(pcaProduct)

#Final projection
biplot(pcaProduct)

#Use the ade4 librarie for the acp
pcaProductAde4<-dudi.pca(productData, scannf=FALSE,center = TRUE, scale = TRUE)

#Print the proper values
pcaProductAde4$eig
#Cumulative variances
cumsum(pcaProductAde4$eig)
#The varaince in percentage
pcaProductAde4$eig/sum(pcaProductAde4$eig)*100
#The screeplot
barplot(pcaProductAde4$eig/sum(pcaProductAde4$eig)*100)
#The cumulative percentages
cumsum(pcaProductAde4$eig/sum(pcaProductAde4$eig)*100)

#decomposition of inertia (the share of total variance explained) between variables and components (in 10000 ths)
inertia.dudi(pcaProductAde4,col.inertia = T)$col.abs

#link betwen component and variables in graph
score(pcaProductAde4, xax=1)
score(pcaProductAde4, xax=2)

#Corelation circle. the lenght of an arrow shows the part of information on two axis: 
#The angle between two arrows represents the correlation between them:
# acute angle = positive;
# right angle = zero;
# obtuse angle = negative.
s.corcircle(pcaProductAde4$co)

#Data projection with the new axis
scatter(pcaProductAde4, posieig="none")
#Data projection with the new axis (but with dot)
scatter(pcaProductAde4, posieig="none", clab.row=0)

#----PCA:COUNTRY-----

#Plot of a matrix of data
pairs(countryData)

#Returns the matrix of correlations (purement informatif)
cor(countryData)

#Center and scale data
countryData.CR<-scale(countryData,center=TRUE,scale=TRUE)
#Perform the pca and returns the results as an object
pcaCountry <- princomp(countryData.CR)

#Variance-covariance of scaled and centered variables
covarCountry<-cov(countryData.CR)
#summary of the componnent
summary(pcaCountry)

#visual of the summary
plot(pcaCountry)

#weight of the original variables in the component
loadings(pcaCountry)

#Final projection
biplot(pcaCountry)

#Use the ade4 librarie for the acp
pcaCountryAde4<-dudi.pca(countryData, scannf=FALSE,center = TRUE, scale = TRUE)

#Print the proper values
pcaCountryAde4$eig
#Cumulative variances
cumsum(pcaCountryAde4$eig)
#The varaince in percentage
pcaCountryAde4$eig/sum(pcaCountryAde4$eig)*100
#The screeplot
barplot(pcaCountryAde4$eig/sum(pcaCountryAde4$eig)*100)
#The cumulative percentages
cumsum(pcaCountryAde4$eig/sum(pcaCountryAde4$eig)*100)

#decomposition of inertia (the share of total variance explained) between variables and components (in 10000 ths)
inertia.dudi(pcaCountryAde4,col.inertia = T)$col.abs

#link betwen component and variables in graph
score(pcaCountryAde4, xax=1)
score(pcaCountryAde4, xax=2)

#Corelation circle. the lenght of an arrow shows the part of information on two axis: 
#The angle between two arrows represents the correlation between them:
# acute angle = positive;
# right angle = zero;
# obtuse angle = negative.
s.corcircle(pcaCountryAde4$co)

#Data projection with the new axis
scatter(pcaCountryAde4, posieig="none")
#Data projection with the new axis (but with dot)
scatter(pcaCountryAde4, posieig="none", clab.row=0)

#----PCA: COUNTRIES WITHOUT UK----
#Same without UK
#Remove UK from the dataset 
countryDataWithoutUK <- subset(countryData, !(rownames(countryData) %in% "United Kingdom"))
#Plot of a matrix of data
pairs(countryDataWithoutUK)

#Returns the matrix of correlations (purement informatif)
cor(countryDataWithoutUK)

#Center and scale data
countryDataWithoutUK.CR<-scale(countryDataWithoutUK,center=TRUE,scale=TRUE)
#Perform the pca and returns the results as an object
pcaCountryWithoutUK <- princomp(countryDataWithoutUK.CR)

#Variance-covariance of scaled and centered variables
covarCountryWithoutUK<-cov(countryDataWithoutUK.CR)
#summary of the componnent
summary(pcaCountryWithoutUK)

#Visual of the summary
plot(pcaCountryWithoutUK)

#weight of the original variables in the component
loadings(pcaCountryWithoutUK)

#Final projection
biplot(pcaCountryWithoutUK)

#Use the ade4 librarie for the acp
pcaCountryWithoutUKAde4<-dudi.pca(countryDataWithoutUK, scannf=FALSE,center = TRUE, scale = TRUE)

#Print the proper values
pcaCountryWithoutUKAde4$eig
#Cumulative variances
cumsum(pcaCountryWithoutUKAde4$eig)
#The varaince in percentage
pcaCountryWithoutUKAde4$eig/sum(pcaCountryWithoutUKAde4$eig)*100
#The screeplot
barplot(pcaCountryWithoutUKAde4$eig/sum(pcaCountryWithoutUKAde4$eig)*100)
#The cumulative percentages
cumsum(pcaCountryWithoutUKAde4$eig/sum(pcaCountryWithoutUKAde4$eig)*100)

#decomposition of inertia (the share of total variance explained) between variables and components (in 10000 ths)
inertia.dudi(pcaCountryWithoutUKAde4,col.inertia = T)$col.abs

#link betwen component and variables in graph
score(pcaCountryWithoutUKAde4, xax=1)
score(pcaCountryWithoutUKAde4, xax=2)

#Corelation circle. the lenght of an arrow shows the part of information on two axis: 
#The angle between two arrows represents the correlation between them:
# acute angle = positive;
# right angle = zero;
# obtuse angle = negative.
s.corcircle(pcaCountryWithoutUKAde4$co)

#Data projection with the new axis
scatter(pcaCountryWithoutUKAde4, posieig="none")
#Data projection with the new axis (but with dot)
scatter(pcaCountryWithoutUKAde4, posieig="none", clab.row=0)

#-------------------------PART 3: CLUSTERING-------------------------
#----CLUSTERING: CountryData----

#-----COMPUTE THE NUMBER OF CLUSTERS-----
#Prepare the data
clusteringCountries <- countryDataWithoutUK

scaled_data = as.matrix(scale(clusteringCountries))

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 5
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

#-----COMPUTE THE CLUSTERS-----
#J'introduis mon dataset dans mydata
clusteringCountries2 <- countryDataWithoutUK

#Je calcule mon ACP pour l'introduire dans mon dataset qui est centré
clusteringCountries2.PCA <- dudi.pca(clusteringCountries2, scannf = FALSE, nf = 3, center = TRUE, scale = TRUE)

#J'introduis mon ACP dans le DataSet
clusteringCountries2 <- cbind(clusteringCountries2, clusteringCountries2.PCA$li)

#Compute the kmeans
clusteringCountries2.km <- kmeans(clusteringCountries2[,1:3], centers = 3, iter.max = 10, nstart = 10)

#See how many data each cluster contains
table(clusteringCountries2.km$cluster)

#Show the centers
clusteringCountries2.km$centers

#Show the different clusters
pairs(clusteringCountries2[,1:3],col=clusteringCountries2.km$cluster)

#Create a scatterplotmatrix
scatterplotMatrix(clusteringCountries2[,1:3],smooth=FALSE,groups=clusteringCountries2.km$cluster, by.groups=TRUE)

aggregate(clusteringCountries2[,1:3], list(clusteringCountries2.km$cluster), mean)

#Final Result
plot(clusteringCountries2[,c("Axis1","Axis2")], col="white", main="K-means")
text(clusteringCountries2[,c("Axis1","Axis2")], labels=rownames(clusteringCountries2), col=clusteringCountries2.km$cluster, main="K-means on scaled data", cex=0.50)

#----A EFFACER (SAUF WORDCLOUD!!)----

#----CLUSTERING: ProductData----

#Va nous permettre de changer rapidement de dataset, sans devoir changer toutes les lignes de code
productClustering <- productData

# Elbow method
scaled_data = as.matrix(scale(productClustering))
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
productClustering.R <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(productClustering.R, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#We find the optimal number of clusters at 5, k = 5
abline(v = 5, lty =2)

#---------TEST-------

# KMeans with k=5, 10 iterations for each kmeans, 10 kmeans tried..
km1 <- kmeans(mydata[,1:4], centers = 5, iter.max = 10, nstart = 10)

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




#--------------TEST--------

pca4<-dudi.pca(productClustering[,1:4], scannf=FALSE, nf=4,center = TRUE, scale = TRUE )
productClustering<-cbind(productClustering,pca4$li)

km1 <- kmeans(productClustering[,1:4], centers = 3, iter.max = 10, nstart = 10)
table(km1$cluster)
km1$centers

#Plot the clusters
pairs(productClustering[,1:4],col=km1$cluster)

scatterplotMatrix(productClustering[,1:4],smooth=FALSE,groups=km1$cluster, by.groups=TRUE)

# Representation of clusters in the 2 first principal components
plot(productClustering[,c("Axis1","Axis2")], col=km1$cluster, main="K-means")

# Same algorithm, but on scaled data.
km2 <- kmeans(scale(productClustering[,1:4],center = TRUE,scale=TRUE), centers = 3, iter.max = 10, nstart = 10)
# Size of the clusters
table(km2$cluster)
# Clusters Centers (with no direct meaning!)
km2$centers
# Cluster center on initial variables
aggregate(productClustering[,1:4], list(km2$cluster), mean)

# Representation of clusters in the 2 first principal components
plot(productClustering[,c("Axis1","Axis2")], col=km2$cluster)
scatterplotMatrix(productClustering[,1:4],smooth=FALSE,groups=km2$cluster, by.groups=TRUE)
scatterplotMatrix(productClustering[,1:4],smooth=FALSE,groups=km2$cluster, by.groups=FALSE)
cor(productClustering[,1:4])

# Comparison of the two results
plot(productClustering[,c("Axis1","Axis2")], col=km1$cluster, main="K-means")
plot(productClustering[,c("Axis1","Axis2")], col=km2$cluster, main="K-means on scaled data")

# Plot with labels
plot(productClustering[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(productClustering[,c("Axis1","Axis2")], labels=rownames(productClustering), col=km2$cluster, main="K-means on scaled data", cex=0.7)

#----------CLUSTERING CountryData------------

#Va nous permettre de changer rapidement de dataset, sans devoir changer toutes les lignes de code
clusteringCountries <- countryDataWithoutUK

#Scale clusterCountries
clusteringCountries.R <- scale(clusteringCountries)

# Elbow method
fviz_nbclust(clusteringCountries.R, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(clusteringCountries.R, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

pca5<-dudi.pca(clusteringCountries[,1:3], scannf=FALSE, nf=4,center = TRUE, scale = TRUE )
clusteringCountries<-cbind(clusteringCountries,pca5$li)

km3 <- kmeans(clusteringCountries[,1:3], centers = 3, iter.max = 10, nstart = 10)
table(km3$cluster)
km3$centers

#Plot the clusters
pairs(clusteringCountries[,1:3],col=km3$cluster)

scatterplotMatrix(clusteringCountries[,1:3],smooth=FALSE,groups=km3$cluster, by.groups=TRUE)

# Representation of clusters in the 2 first principal components
plot(clusteringCountries[,c("Axis1","Axis2")], col=km3$cluster, main="K-means")

# Same algorithm, but on scaled data.
km4 <- kmeans(scale(clusteringCountries[,1:3],center = TRUE,scale=TRUE), centers = 3, iter.max = 10, nstart = 10)
# Size of the clusters
table(km4$cluster)
# Clusters Centers (with no direct meaning!)
km4$centers
# Cluster center on initial variables
aggregate(clusteringCountries[,1:3], list(km4$cluster), mean)

# Representation of clusters in the 2 first principal components
plot(clusteringCountries[,c("Axis1","Axis2")], col=km4$cluster)
scatterplotMatrix(clusteringCountries[,1:3],smooth=FALSE,groups=km4$cluster, by.groups=TRUE)
scatterplotMatrix(clusteringCountries[,1:3],smooth=FALSE,groups=km4$cluster, by.groups=FALSE)
cor(clusteringCountries[,1:3])

# Comparison of the two results
plot(clusteringCountries[,c("Axis1","Axis2")], col=km3$cluster, main="K-means")
plot(clusteringCountries[,c("Axis1","Axis2")], col=km4$cluster, main="K-means on scaled data")

# Plot with labels
plot(clusteringCountries[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(clusteringCountries[,c("Axis1","Axis2")], labels=rownames(productData), col=km4$cluster, main="K-means on scaled data", cex=0.50)

#HIERARCHICAL CLUSTERING -> Mieux pour les countries

# Computing the distance matrix
#mydata.dist<- dist(mydata[,1:4]) # not so good
clusteringCountries.dist<- dist(scale(clusteringCountries[,1:3],center = TRUE,scale=TRUE)) # Better

#Hclust with average link
HClust.1 <- hclust(clusteringCountries.dist, method="average")
plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab=
       "Observation Number in Data Set", sub="Method=average; Distance=euclidian")
# Cutting the tree to obtain 2 clusters
hc.1<-cutree(HClust.1, k=2)
# Size of the clusters
table(hc.1)

plot(clusteringCountries[,c("Axis1","Axis2")], col=hc.1, main="Clusters with average link." )

# Plot with labels
plot(clusteringCountries[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(clusteringCountries[,c("Axis1","Axis2")], labels=rownames(clusteringCountries), col=hc.1, main="K-means on scaled data", cex=0.7)

scatterplotMatrix(clusteringCountries[,1:3],smooth=FALSE,groups=hc.1, by.groups=TRUE)

# HClsut single link
HClust.2 <- hclust(clusteringCountries.dist , method= "single")
plot(HClust.2, main= "Cluster Dendrogram for Solution HClust.2", xlab=
       "Observation Number in Data Set", sub="Method=single; Distance=euclidian")
hc.2<-cutree(HClust.2, k=3)
# Size of the clusters
table(hc.2)

plot(clusteringCountries[,c("Axis1","Axis2")], col=hc.2, main="Clusters with single link." )
scatterplotMatrix(clusteringCountries[,1:4],smooth=FALSE,groups=hc.2, by.groups=TRUE)

# HClust complete link
HClust.3 <- hclust(clusteringCountries.dist, method="complete")
plot(HClust.3, main= "Cluster Dendrogram for Solution HClust.3", xlab=
       "Observation Number in Data Set", sub="Method=complete; Distance=euclidian")
hc.3<-cutree(HClust.3, k=3)
# Size of the clusters
table(hc.3)

plot(clusteringCountries[,c("Axis1","Axis2")], col=hc.3, main="Clusters with complete link." )
scatterplotMatrix(clusteringCountries[,1:4],smooth=FALSE,groups=hc.3, by.groups=TRUE)

# Plot with labels
plot(clusteringCountries[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(clusteringCountries[,c("Axis1","Axis2")], labels=rownames(clusteringCountries), col=hc.3, main="K-means on scaled data", cex=0.7)


# HClust Ward.D link
HClust.4 <- hclust(clusteringCountries.dist, method="ward.D")
plot(HClust.4, main= "Cluster Dendrogram for Solution HClust.4", xlab=
       "Observation Number in Data Set Iris", sub="Method=Ward; Distance=euclidian")
hc.4<-cutree(HClust.4, k=3)
# Size of the clusters
table(hc.4)

plot(clusteringCountries[,c("Axis1","Axis2")], col=hc.4, main="Clusters with complete link." )
scatterplotMatrix(clusteringCountries[,1:3],smooth=FALSE,groups=hc.4, by.groups=TRUE)

# Plot with labels
plot(clusteringCountries[,c("Axis1","Axis2")], col="white", main="K-means on scaled data")
text(clusteringCountries[,c("Axis1","Axis2")], labels=rownames(clusteringCountries), col=hc.4, main="K-means on scaled data", cex=0.7)

# 
#
# CLUSTER ON WORDS USAGE
#
#

#Cluster on words usage

uniqueDescriptionList <- onlineRetailUnique[!duplicated(onlineRetailUnique[,c('Description')]),]
uniqueDescriptionList <- uniqueDescriptionList[c("StockCode", "Description")]
#Putting a name for col description
names(uniqueDescriptionList) <- c("StockCode","Description")
#Converting to corpus

dd<-data.frame(doc_id=uniqueDescriptionList[["StockCode"]],text=uniqueDescriptionList[["Description"]])
head(dd)

docs <- VCorpus(DataframeSource(dd))

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs <- tm_map(docs,content_transformer(tolower))

#docs<-tm_map(docs, PlainTextDocument)

summary(docs)
#Let's remove some useless words like colors..
docs<-tm_map(docs, removeWords, c('pink', 'blue', 'tag', 'green', 'orange','red','black','purple','white','set'))
#Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#Remove whitespaces
docs <- tm_map(docs, stripWhitespace)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#No need for this command because language is english
#docs <- tm_map(docs,stemDocument)

#Create a term document matrix from the corpus
minTermFreq<- 25
maxTermFreq<-Inf
dtm <- DocumentTermMatrix(docs,control=list(wordLengths=c(3,Inf), bounds = list(global = c(minTermFreq, maxTermFreq))))

#Clean dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ]


##Clustering
tdm.tfidf <- tm::weightTfIdf(dtm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.99) 

m <- as.matrix(dtm_tfxidf)

norm_eucl <- function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_norm <- norm_eucl(m)
results <- kmeans(m_norm, 3)

clusters <- 1:10

for(i in clusters){
  cat("Cluster ", i, ":", findFreqTerms(dtm_tfxidf[results$cluster== i,], lowfreq=25), "\n\n")
}

fviz_nbclust(m_norm, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Frequencies of words
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
freq[ord]
#inspect least frequently occurring terms
freq[tail(ord)] 

#That done, let’s take get a list of terms that occur at least a  40 times in the entire corpus. This is easily done using the findFreqTerms() function as follows:
findFreqTerms(dtm,lowfreq=40)

#findAssocs(dtmr,"hair",0.1)

wf=data.frame(term=names(freq),occurrences=freq)
ggplot(subset(wf, freq>40), aes(term, occurrences)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#On construit un data.frame avec les fr?quences de mots:
mfw<-data.frame(word=names(freq), freq=freq)
# On s'assure que les mots sont dans l'ordre de fr?quences
mfw<-mfw[order(mfw[,2], decreasing=TRUE),]

# On fait un barplot des fr?quences
barplot(mfw[,2], names.arg = mfw[,1],las=2, horiz = TRUE)
# Heu il y a trop de mots, on n'y voit rien :D
# On cr?e un nouvel index pour ne retenir que les mots apparaissant au 
# moins 15 fois:
mfw2<-subset(mfw, freq[ord]>15)
# On refait le barplot en ajoutant une palette de couleur (avec la 
# fonction heatcolors())  contenant autant de couleur que le nombre 
# de lignes(de mots donc) retenues dan smfw2.
barplot(mfw2[,2], names.arg = mfw2[,1],las=2,
        horiz = TRUE, main="Most Frequent Words",
        col=heat.colors(dim(mfw2)[1]))

#setting the same seed each time ensures consistent look across clouds
#set.seed(52)
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
#limit words by specifying min frequency and add color
wordcloud(names(freq),freq,min.freq=25,scale=c(5, .1),colors=brewer.pal(6,"Dark2"), random.order=FALSE)
