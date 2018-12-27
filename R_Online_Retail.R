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
OnlineretailClean <- Onlineretail[c(3:6,8)]
#View(OnlineretailClean)

#INSPECT THE DATA
str(OnlineretailClean)
dim(OnlineretailClean)
#Check the first part of the data
head(OnlineretailClean)
#Check the last part of the data
tail(OnlineretailClean)

#ANALYSIS OF THE DATA - DESCRIPTIVE STATISTICS
#1. Number of invoices (orders)
UniqueInvoices <-
#2. Number of products
UniqueProducts <-
#3. Number of Countries
UniqueCountries <-
#4. Number of Customers
UniqueCustomers <-
#5. Amount of purchases for each customer
AmountPurchases <-

#List of countries
ListOfCountry <- OnlineretailClean["Country"]
ListOfCountry[!duplicated(ListOfCountry), ]
