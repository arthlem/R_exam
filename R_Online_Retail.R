#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

#-------------------------------------------SUPERMARKETS-------------------------------------------------

#IMPORT THE DATA
Onlineretail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=".", row.names = NULL) #Load CSV File
View(Onlineretail)


#INSPECT THE DATA
str(Onlineretail)
dim(Onlineretail)
#Check the first part of the data
head(Onlineretail)
#Check the last part of the data
tail(Onlineretail)

#ANALYSIS OF THE DATA - DESCRIPTIVE STATISTICS

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
View(PurchasesPerCountry)
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


