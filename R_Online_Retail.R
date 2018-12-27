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
#View(Onlineretail)


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

#5. Amount of purchases for each customer


#List of countries


boxplot(Onlineretail, main= "Purchases", horizontal = TRUE, outline = FALSE,las=2)

