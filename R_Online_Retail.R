#---------------------------------------------GITHUB----------------------------------------------------
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

#Libraries to load
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
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
  geom_text(aes(label=format(round(CA*turnoverByMonthScale, 2), nsmall = 2)), vjust=1.6, color="white", size=3.5)
#<<<<<<< HEAD
# labs(x="Month", y="Turnover in million")
#
#=======
#  labs(x="Month", y="Turnover in thousand")
#>>>>>>> a5fb17dde751436c450e59514bc906937edd915f

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

#Create a dataset with aggregate() by combining the StockCode with the NbOfCustomers and then change the variables names with names()
stockPerCustomers <- aggregate(OnlineretailUnique$CustomerID, by=list(Category=OnlineretailUnique$StockCode), FUN=function(x) length(unique(x)))
names(stockPerCustomers) <- c("StockCode","NbOfCustomers")

#Create a dataset with aggregate() by combining the StockCode with the UnitPrice and then change the variables names with names()
stockPerUnitPrice <- aggregate(OnlineretailUnique$UnitPrice, by=list(Category=OnlineretailUnique$StockCode), FUN=mean)
names(stockPerUnitPrice) <- c("StockCode","Avg UnitPrice")

#Create a dataset with aggregate() by combining the StockCode with the Country and then change the variables names with names()
stockPerCountry <- aggregate(OnlineretailUnique$Country, by=list(Category=OnlineretailUnique$StockCode), FUN=function(x) length(unique(x)))
names(stockPerCountry) <- c("StockCode","NbOfCountry")

#Merge the dataset to make productData
productData <- merge(stockPerQuantity, stockPerPurchases, by="StockCode")
productData <- merge(productData, stockPerCustomers, by="StockCode")
productData <- merge(productData, stockPerUnitPrice, by="StockCode")
productData <- merge(productData, stockPerCountry, by="StockCode")
row.names(productData) <- productData$StockCode
productData <- productData[2:6]

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
#Plot of a matrix of data
pairs(productData)

#Center and scale data
productData.CR<-scale(productData,center=TRUE,scale=TRUE)
#Perform the pca and returns the results as an object
pca <- princomp(productData.CR)
summary(pca)
plot(pca)
loadings(pca)


#--------------------------------UPDATE 29/12/2018------------------------------------

#---------ALL COUNTRIES-------------

countryData.CR<-scale(countryData,center=TRUE,scale=TRUE)
pca2 <- princomp(countryData.CR)
summary(pca2)
plot(pca2)
loadings(pca2)
pairs(countryData)
View(countryData)


#---------WITHOUT UK----------------

#Same without UK
countryDataWithoutUK <- subset(countryData, !(rownames(countryData) %in% "United Kingdom"))

countryDataWithoutUK.CR<-scale(countryDataWithoutUK,center=TRUE,scale=TRUE)
pca3 <- princomp(countryDataWithoutUK.CR)
summary(pca3)
plot(pca3)
loadings(pca3)
pairs(countryDataWithoutUK, pch=19)
View(countryDataWithoutUK)


#---------WITHOUT UK BUT ONLY BEST SELLING COUNTRIES-----------

#Order by highest turnover, BS stands for Best Selling
countryDataWithoutUKBS <- countryDataWithoutUK[order(-countryDataWithoutUK$Turnover),]
#Take the best 17 selling countries
countryDataWithoutUKBS <- countryDataWithoutUKBS[1:17,]

countryDataWithoutUKBS.CR<-scale(countryDataWithoutUKBS,center=TRUE,scale=TRUE)
pca3 <- princomp(countryDataWithoutUKBS.CR)
summary(pca3)
plot(pca3)
loadings(pca3)
pairs(countryDataWithoutUKBS, pch=19)
View(countryDataWithoutUKBS)

#Let's compare

pairs(countryData, main="All Countries", pch=19)
pairs(countryDataWithoutUK,main="All Countries Without UK", pch=19)
pairs(countryDataWithoutUKBS,main="TOP 17 Without UK", pch=19)

#------------END OF UPDATE 29/12/2018------------

#Using ade4

acp.ade4<-dudi.pca(productData, scannf=FALSE, nf=3,center = TRUE, scale = TRUE )

# Dans les deux cas il est conseill? de centrer(center=TRUE)-r?duire(scale=TURE)
# les donn?es :
# - si on ne centre pas, la premi?re composante sera la direction qui va 
#   de l'origine (point o? toutes les variables originales sont nulles)au 
#   centre du nuage.
# - la r?duction permet d'?viter les effets des diff?rences importantes d'?chelles 
#   entre les variables.

# Impression des valeurs propres
acp.ade4$eig
# Les variances cumul?es
cumsum(acp.ade4$eig)
# Les variances en pourcentages:
acp.ade4$eig/sum(acp.ade4$eig)*100
# Le screeplot:
barplot(acp.ade4$eig/sum(acp.ade4$eig)*100)
# Les pourcentages cumul?s :
cumsum(acp.ade4$eig/sum(acp.ade4$eig)*100)


# Le probl?me de l'ACP ?tant l'interpr?tation des nouveaux axes (les 
# composantes) puisqu'ils sont form?s par combinaisons lin?aires
# des anciennes variables. C?d que les composantes sont un mix
# des variables initiales.
# Une premi?re fa?on de "comprendre" ces composantes est de regarder les 
# vecteurs propres qui contiennent les coefficients des combinaisons 
# lin?aires ?voqu?es ci-dessus:
acp.ade4$c1

# On peut aussi avoir une id?e de la d?compostion l'inertie (la part de la variance totale 
# expliqu?e) entre les variables et composantes (en 10000 ?mes):
inertia.dudi(acp.ade4,col.inertia = T)$col.abs


# Le package ade4 fournit aussi d'autres outils.
# Plot des "droites" de corr?lation des variables avec les deux premi?res 
# composantes: 
score(acp.ade4, xax=1)
score(acp.ade4, xax=2)
# Ces graphiques permettent de voir les liaisons entre les composantes et les variables.



# On peut tracer les cercles de correlation o? la longueur des fl?ches 
# indique la part de leur information repr?sent?e par les deux axes. 
# L'angle entre deux  fl?ches repr?sente la corr?lation qui les lie : 
# - angle aigu = positive;
# - angle droit = nulle;
# - angle obtus = n?gative.
s.corcircle(acp.ade4$co)

# Enfin on peut passer aux projections des donn?es dans les nouveaux axes.
# Repr?sentation ? la fois les individus et des variables dans le premier plan
# factoriel (deux premi?res composantes):
scatter(acp.ade4)
# Idem sans l'histogramme des valeurs propres
scatter(acp.ade4, posieig="none")
# Idem mais sans ?tiquettes, les individus ?tant repr?sent? par des points
scatter(acp.ade4, posieig="none", clab.row=0)
# Comparez cette visualisation avec la visualisation 3D du debut...




  