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
?read.csv2

Onlineretail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=",", row.names = NULL) #Load CSV File

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
OnlineretailClean <- subset(OnlineretailClean, grepl("^(?!C).*$", OnlineretailClean$InvoiceNo, perl = TRUE))

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
<<<<<<< HEAD
  labs(x="Month", y="Turnover in million")

=======
  labs(x="Month", y="Turnover in thousand")
>>>>>>> a5fb17dde751436c450e59514bc906937edd915f
