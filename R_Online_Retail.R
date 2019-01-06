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
countriesClustering <- countryDataWithoutUK

countriesClustering.R <- as.matrix(scale(countriesClustering))

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
<<<<<<< HEAD
k.max <- 15
data <- countriesClustering.R
=======
k.max <- 10
data <- scaled_data
>>>>>>> 72b0e87735cdad7a82e521b954d1f9a5abfc2f5a
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 4, lty =2)

#-----COMPUTE THE CLUSTERS-----

#Je calcule mon ACP pour l'introduire dans mon dataset qui est centré
countriesClustering.PCA <- dudi.pca(countriesClustering, scannf = FALSE, nf = 4, center = TRUE, scale = TRUE)

#J'introduis mon ACP dans le DataSet
countriesClusteringWithPCA <- cbind(countriesClustering, countriesClustering.PCA$li)

#Compute the kmeans
countriesClustering.km <- kmeans(scale(countriesClustering[,1:4]), centers = 4, iter.max = 10, nstart = 10)

#See how many data each cluster contains
table(countriesClustering.km$cluster)

#Show the centers
countriesClustering.km$centers

#Show the different clusters
pairs(countriesClustering[,1:4],col=countriesClustering.km$cluster)

#Create a scatterplotmatrix
scatterplotMatrix(countriesClustering[,1:4],smooth=FALSE,groups=countriesClustering.km$cluster, by.groups=TRUE)

aggregate(countriesClustering[,1:4], list(countriesClustering.km$cluster), mean)

#Final Result
plot(countriesClusteringWithPCA[,c("Axis1","Axis2")], col="white", main="K-means")
text(countriesClusteringWithPCA[,c("Axis1","Axis2")], labels=rownames(countriesClustering), col=countriesClustering.km$cluster, main="K-means on scaled data", cex=0.50)

#----CLUSTERING: ProductData----

#Va nous permettre de changer rapidement de dataset, sans devoir changer toutes les lignes de code
productClustering <- productData

#ELBOW METHOD
#Centering the data
productClustering.R <- as.matrix(scale(productClustering))
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(productClustering.R, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Thanks to the elbow method, we find the optimal number of clusters at 5 which means k = 5
abline(v = 5, lty =2)

#COMPUTE THE CLUSTERS

#Compute the PCA for the productData cluster
productClustering.PCA <- dudi.pca(productClustering, scannf = FALSE, nf = 5, center = TRUE, scale = TRUE)


#Bind the PCA in the DataSet
productClusteringWithPCA <- cbind(productClustering.R, productClustering.PCA$li)

#Compute the kmeans (with scaled data)
productClustering.km <- kmeans(productClusteringWithPCA[,1:5], centers = 5, iter.max = 10, nstart = 10)

#See how many data each cluster contains
table(productClustering.km$cluster)

#Show the centers
productClustering.km$centers

#Show the different clusters
pairs(productClusteringWithPCA[,1:5],col=productClustering.km$cluster)

#Create a scatterplotmatrix
scatterplotMatrix(productClusteringWithPCA[,1:5],smooth=FALSE,groups=productClustering.km$cluster, by.groups=TRUE)

aggregate(productClusteringWithPCA[,1:5], list(productClustering.km$cluster), mean)

#Final Result
plot(productClusteringWithPCA[,c("Axis1","Axis2")], col="white", main="K-means")
text(productClusteringWithPCA[,c("Axis1","Axis2")], labels=rownames(productClustering), col=productClustering.km$cluster, main="K-means on scaled data", cex=0.50)

#----BONUS: CLUSTER ON WORDS USAGE----

#Cluster on words usage

uniqueDescriptionList <- onlineRetailUnique[!duplicated(onlineRetailUnique[,c('Description')]),]
uniqueDescriptionList <- uniqueDescriptionList[c("StockCode", "Description")]
#Putting a name for col description
names(uniqueDescriptionList) <- c("StockCode","Description")
#Converting to corpus

dd<-data.frame(doc_id=uniqueDescriptionList[["StockCode"]],text=uniqueDescriptionList[["Description"]])

docs <- VCorpus(DataframeSource(dd))

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs <- tm_map(docs,content_transformer(tolower))
#Let's remove some useless words like colors..
docs<-tm_map(docs, removeWords, c('pink', 'blue', 'tag', 'green', 'orange','red','black','purple','white','set'))
#Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#Remove whitespaces
docs <- tm_map(docs, stripWhitespace)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#Create a term document matrix from the corpus
minTermFreq<- 25
maxTermFreq<-Inf
dtm <- DocumentTermMatrix(docs,control=list(wordLengths=c(3,Inf), bounds = list(global = c(minTermFreq, maxTermFreq))))

#Clean dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ]

##Clustering
dtm_tfxidf <- weightTfIdf(dtm)
#dtm_tfxidf <- tm::removeSparseTerms(dtm_tfxidf, 0.99) 

m <- as.matrix(dtm_tfxidf)

norm_eucl <- function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_norm <- norm_eucl(m)
results <- kmeans(m_norm, 3)

clusters <- 1:10

# Get words repartition accross clusters.. 
for(i in clusters){
  cat("Cluster ", i, ":", findFreqTerms(dtm_tfxidf[results$cluster== i,], lowfreq=25), "\n\n")
}

# Number of clusters
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

#Graph with words usage with freq > 40
wf=data.frame(term=names(freq),occurrences=freq)
ggplot(subset(wf, freq>40), aes(term, occurrences)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
#limit words by specifying min frequency and add color
wordcloud(names(freq),freq,min.freq=25,scale=c(5, .1),colors=brewer.pal(6,"Dark2"), random.order=FALSE)

