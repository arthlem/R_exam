#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

#-------------------------------------------ONLINE RETAIL CSV-------------------------------------------------
#Libraries to install
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ade4")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("cluster")

#Libraries to load
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ade4)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
#General constants
turnoverByMonthScale <- 1/1000

#----A. IMPORT THE DATA----
Onlineretail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=",", row.names = NULL, fileEncoding = "UTF-8-BOM") #Load CSV File

#Check if the data has been imported correctly
#View(Onlineretail)

#Match canceled orders and  corresponding orders
ProductsPerInvoice <- aggregate(Onlineretail$Quantity, by=list(Category=Onlineretail$InvoiceNo), FUN=sum)

#Explore the varibales
#How many variables do we have?
length(Onlineretail)

#What are the different variables?
str(Onlineretail)

#Number of unique InvoiceNo (orders)
length(unique(Onlineretail$InvoiceNo))

#Number of unique StockCode (different products)
length(unique(Onlineretail$StockCode))

#Number of unique Description (different products): Compare with the length of StockCode
length(unique(Onlineretail$Description))

#Analysis of Quantity
summary(Onlineretail$Quantity)

#InvoiceData -> Select function has been taken from the library dplyr
#Check when the records started and when it ended
head(select(Onlineretail, InvoiceDate), 10)
tail(select(Onlineretail, InvoiceDate), 10)

#UnitPrice, not working?
#summary(Onlineretail$UnitPrice)

#CustomerID, show th enumber of unique Customers
length(unique(Onlineretail$CustomerID))

#Country, show the number of unique countries
length(unique(Onlineretail$Country))

#----B. DATA CLEANING----
#Removing the missing variables (CustomerID that are empty)
OnlineretailClean <- subset(Onlineretail, CustomerID != "")

#Counting the number of removed variables (missing CustomerID)
dim(Onlineretail)-dim(OnlineretailClean)
#Percentage of empty data
round((135080/541909)*100,digit=2)

#Remove Invoices beggining with C
beforeCancelations <- dim(OnlineretailClean[])
OnlineretailClean <- subset(OnlineretailClean, grepl("^(?!C).*$", OnlineretailClean$InvoiceNo, perl = TRUE))
afterCancelations <- dim(OnlineretailClean)
#Percentage of Cancelations (to finish)


#
# ATTENTION PAS TERMINé !!!!!
#
(beforeCancelations-withCancelations)

#Finish cleaning dataset in one line 
# DataToRemove <- c('POST', 'D', 'C2', 'M', 'BANK CHARGES', 'PADS', 'DOT')
# 
# OnlineretailClean <- subset(OnlineretailClean, !(StockCode %in% DataToRemove))

#Remove POSTAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "POST")

#Remove Discount
OnlineretailClean <- subset(OnlineretailClean, StockCode != "D")

#Remove CARRIAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "C2")

#Remove Manual
OnlineretailClean <- subset(OnlineretailClean, StockCode != "M")

#Remove Bank Charges
OnlineretailClean <- subset(OnlineretailClean, StockCode != "BANK CHARGES")

#Remove PADS TO MATCH ALL CUSHIONS 
OnlineretailClean <- subset(OnlineretailClean, StockCode != "PADS")

#Remove DOTCOM POSTAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "DOT")

#Remove Unit Price <= 0
OnlineretailClean <- subset(OnlineretailClean, UnitPrice > 0)

#Remove Quantity < 0
OnlineretailClean <- subset(OnlineretailClean, Quantity > 0)

#Remove Duplicates
OnlineretailUnique <- unique(OnlineretailClean)
dim(OnlineretailClean)-dim(OnlineretailUnique)

#Compute total revenue per row
#setDT(OnlineretailClean)[, TotalPrice := as.numeric(as.character(UnitPrice))*Quantity]

OnlineretailClean <- OnlineretailClean %>% 
  mutate(TotalPrice = Quantity*UnitPrice)

#----ANALYSIS OF THE DATA - DESCRIPTIVE STATISTICS----

#1. Number of invoices (orders)
ListOfInvoices <- Onlineretail["InvoiceNo"]
length(ListOfInvoices[!duplicated(ListOfInvoices), ])
#2. Number of products
ListOfProducts <- Onlineretail["StockCode"]
length(ListOfProducts[!duplicated(ListOfProducts), ])
#3. Number of Countries
ListOfCountry <- Onlineretail["Country"]
length(ListOfCountry[!duplicated(ListOfCountry), ])

#4. Number of Customers
ListOfCustomers <- Onlineretail["CustomerID"]
length(ListOfCustomers[!duplicated(ListOfCustomers), ])

#5. Amount of purchases for each country
PurchasesPerCountry <- aggregate(Onlineretail$Quantity, by=list(Category=Onlineretail$Country), FUN=sum)
#View(PurchasesPerCountry)
PurchasesNotUk <- PurchasesPerCountry[-36,]

#PieChart with all countries: TO DO -> Calc the % of sales from UK (!!)
slices <- PurchasesPerCountry[[2]]
lbls <- PurchasesPerCountry[[1]]
pie(slices, labels = lbls, main="Pie Chart of Countries without UK")

#Piechart without UK because it takes a too big part: TO DO -> only show the 10 biggest countries (!!)
slicesNotUK <- PurchasesNotUk[[2]]
lblsNotUK <- PurchasesNotUk[[1]]
pie(slicesNotUK, labels = lblsNotUK, main="Pie Chart of Countries without UK")


#Countries with the most returns
Returns <- subset(Onlineretail,Quantity<0)
CountriesWithReturns <- aggregate(Returns$Quantity, by=list(Category=Returns$Country), FUN=sum)
View(CountriesWithReturns)

#Sales per product
SalesPerProduct <- aggregate(Onlineretail$Quantity, by=list(StockCode=Onlineretail$StockCode), FUN=sum)
View(SalesPerProduct)
boxplot(SalesPerProduct[2], main= "Sales per product", horizontal = TRUE, outline = FALSE,las=2)
#TO DO: Show the most returned product (!!)

#Analysis of the % of quantity returned in comparison with the number ordered
((-CountriesWithReturns[29,2]) / PurchasesPerCountry[36,2])*100

boxplot(Onlineretail, main= "Purchases", horizontal = TRUE, outline = FALSE,las=2)

#Invoices per month in 2011

#Check format of dates
OnlineretailClean$InvoiceDate <- mdy_hm(OnlineretailClean$InvoiceDate)

#Creating object for date's year
OnlineretailClean$InvoiceYear <- year(OnlineretailClean$InvoiceDate)
#Creating object for date's month
OnlineretailClean$InvoiceMonth <- month(OnlineretailClean$InvoiceDate,label=T)
#Creating object for date's day
OnlineretailClean$InvoiceWeekday <- wday(OnlineretailClean$InvoiceDate, label=T)
#Creating object for date's hour
OnlineretailClean$InvoiceHour <- hour(OnlineretailClean$InvoiceDate)
View(OnlineretailClean)
#Number of invoices per month in 2011
#Filter to select year 2011 and count invoices for each month
monthData <- OnlineretailClean %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceMonth)

ggplot(monthData, aes(InvoiceMonth, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="Month", y="Number of invoices")

#Number of invoices per day in 2011
#Filter to select year 2011 and count invoices for each day of the week
dayData <- OnlineretailClean %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceWeekday)

ggplot(dayData, aes(InvoiceWeekday, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="Week", y="Number of invoices")

#Number of invoices per hour in 2011
#Filter to select year 2011 and count invoices for each hour of the day
hourData <- OnlineretailClean %>% 
  dplyr::filter(InvoiceYear==2011) %>% 
  count(InvoiceHour)

ggplot(hourData, aes(InvoiceHour, n)) +  #plot the number of invoices per day               
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(x="hour", y="Number of invoices")

#Turnover per month in 2011
SalesData <- OnlineretailClean %>%
  dplyr::filter(InvoiceYear == 2011) %>%
  group_by(InvoiceMonth) %>%
  summarise(CA = sum(TotalPrice))

ggplot(SalesData, aes(InvoiceMonth, CA*turnoverByMonthScale)) +              
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=format(round(CA*turnoverByMonthScale, 2), nsmall = 2)), vjust=1.6, color="white", size=3.5)+
  labs(x="Month", y="Turnover in thousand")

#-------------------------------------------PCA-------------------------------------------
#----A. ARANGE DATA SET----
#length(unique(OnlineretailUnique$StockCode))

#Create a dataset per product (StockCode // sum of Quantity / Turnover[Quantity*UnitPrice] / Count of Customers)
#Create a dataset with aggregate() by combining the StockCode with the Quantity and then change the variables names with names()
stockPerQuantity <- aggregate(OnlineretailUnique$Quantity, by=list(Category=OnlineretailUnique$StockCode), FUN=sum)
names(stockPerQuantity) <- c("StockCode","Quantity")

#Create a dataset with aggregate() by combining the StockCode with the Quantity*UnitPrice and then change the variables names with names()
stockPerPurchases <- aggregate(OnlineretailUnique$Quantity*OnlineretailUnique$UnitPrice, by=list(Category=OnlineretailUnique$StockCode), FUN=sum)
names(stockPerPurchases) <- c("StockCode","Purchases")

#Create a dataset with aggregate() by combining the StockCode with the Quantity*UnitPrice and then change the variables names with names()
stockPerCustomers <- aggregate(OnlineretailUnique$CustomerID, by=list(Category=OnlineretailUnique$StockCode), FUN=length)
names(stockPerCustomers) <- c("StockCode","NbOfCustomers")

#Merge the dataset to make productData
productData <- merge(stockPerQuantity, stockPerPurchases, by="StockCode")
productData <- merge(productData, stockPerCustomers, by="StockCode")
row.names(productData) <- productData$StockCode
productData <- productData[2:4]

View(productData)

#Create a dataset per Country (Country // NbOfProduct/Purchases/NbOfCustomers)
#Create a dataset with aggregate() by combining the Country with the nbOfStockCode and then change the variables names with names()
countryPerStockCode <- aggregate(OnlineretailUnique$StockCode, by=list(Category=OnlineretailUnique$Country), FUN=length)
names(countryPerStockCode) <- c("Country","NbOfProduct")

#Create a dataset with aggregate() by combining the Country with the Quantity*UnitPrice and then change the variables names with names()
countryPerPurchases <- aggregate(OnlineretailUnique$Quantity*OnlineretailUnique$UnitPrice, by=list(Category=OnlineretailUnique$Country), FUN=sum)
names(countryPerPurchases) <- c("Country","Turnover")

#Create a dataset with aggregate() by combining the Country with the nbOfCustomers and then change the variables names with names()
countryPerCustomers <- aggregate(OnlineretailUnique$CustomerID, by=list(Category=OnlineretailUnique$Country), FUN=length)
names(countryPerCustomers) <- c("Country","NbOfCustomers")

#Merge the dataset to make countryData
countryData <- merge(countryPerStockCode, countryPerPurchases, by="Country")
countryData <- merge(countryData, countryPerCustomers, by="Country")
row.names(countryData) <- countryData$Country
countryData <- countryData[2:4]

View(countryData)

#----B. GET INTO PCA----
productData.CR<-scale(productData,center=TRUE,scale=TRUE)
pca <- princomp(productData.CR)
summary(pca)
plot(pca)
loadings(pca)
pairs(productData)

countryData.CR<-scale(countryData,center=TRUE,scale=TRUE)
pca2 <- princomp(countryData.CR)
summary(pca2)
plot(pca2)
loadings(pca2)
pairs(countryData)
View(countryData)

#Same without UK
countryDataWithoutUK <- subset(countryData, !(rownames(countryData) %in% "United Kingdom"))
countryDataWithoutUK.CR<-scale(countryDataWithoutUK,center=TRUE,scale=TRUE)
pca3 <- princomp(countryDataWithoutUK.CR)
summary(pca3)
plot(pca3)
loadings(pca3)
pairs(countryDataWithoutUK)
View(countryDataWithoutUK)


#Cluster on words usage
uniqueDescriptionList <- unique(OnlineretailClean["Description"]) 
#Putting a name for col description
names(uniqueDescriptionList) <- c("Description")
#Put descriptions in a vector
descVector <- uniqueDescriptionList[["Description"]]
#Converting to corpus
docs <- VCorpus(VectorSource(descVector))
docnames<-names(docs)

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs <- tm_map(docs,content_transformer(tolower))

docs<-tm_map(docs, PlainTextDocument)

# La m?me manip nous a fait perdre les noms des textes, 
# on r?injecte les noms qui ont ?t? perdus :
names(docs)<-docnames

#Let's remove some useless words like colors..
docs<-tm_map(docs, removeWords, c('pink', 'blue', 'tag', 'green', 'orange','red'))
#Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#Remove whitespaces
docs <- tm_map(docs, stripWhitespace)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#No need for this command because language is english
#docs <- tm_map(docs,stemDocument)

#Create a term document matrix from the corpus
dtm <- DocumentTermMatrix(docs,control=list(wordLengths=c(3,Inf)))
# Frequencies of words
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#Removing word with less than 15 repetitions
freq <- freq[freq>15]
#new length
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
set.seed(42)

#limit words by specifying min frequency and add color
wordcloud(names(freq),freq,min.freq=10,colors=brewer.pal(6,"Dark2"), random.order=FALSE)

set.seed(152)

dtmss <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is only 25% empty space, maximum.   
inspect(dtmss) 

d <- dist(t(dtmss), method="euclidian")   
# Clustering:
fit <- hclust(d=d, method="ward.D")   
# Dendrogram:
plot(fit)
plot(fit, hang=-1)# Pour avoir tous les mots ? la m?me hauteur
# Groupons les mots en 10 groupes:
groups <- cutree(fit, k=5) 

install.packages("NbClust")
library("NbClust")

set.seed(123)
# The following example determine the number of clusters using gap statistics:
res.nb <- NbClust(dtm, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete", index ="gap") 
res.nb # print the results

#Compute all the 30 indices
nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
# Print the result
nb