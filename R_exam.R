#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

#-------------------------------------------SUPERMARKETS-------------------------------------------------

#IMPORT THE DATA
supermarkets <- read.csv2(file.choose(), header=TRUE, sep=",", dec=".") #Load CSV File

#INSPECT THE DATA
#Check the first part of the data
head(supermarkets)
#Check the last part of the data
tail(supermarkets)

#View all the data
#View(supermarkets)
View(supermarkets[c(20,26,32,38,44)])
#Create Distances sub-DataSet

#11 is not included
distances <- supermarkets[c(8,9,10)]
#Create Prices sub-DataSet
prices <- supermarkets[c(5,6)]

purchases <- supermarkets[c(2,3,4)]

boxplot(distances, main= "Distances", horizontal = TRUE, outline = FALSE,las=2)
par(mar=c(5,10,4,2))
boxplot(prices, main= "Prices", horizontal = TRUE, outline = FALSE,las=2)
boxplot(purchases, main= "Purchases", horizontal = TRUE, outline = FALSE,las=2)
head(purchases)
tail(purchases)
summary(purchases)
View(purchases)
#View the different column names of the data and their column number
names(supermarkets)

##PARTIE 1: STATISTIQUE DESCRIPTIVE

average_all <- colMeans(supermarkets[,c(2:11,15:44)])
average_all
#Changer les marges pour les graphiques
par(mar=c(5,4,4,2))

#par(mar=c(5,10,4,2)) -> important quand il y a beaucoup de données

colnames(supermarkets)[20] <- c("avg price shop1")

#Boxplot par catégorie (distance_shop1-5)
boxplot(supermarkets[,c(15,21,27,33,39)], main= "Distance to shop", horizontal = TRUE, outline = FALSE,las=2)

#Shop 1-5 Products Purchased
boxplot(supermarkets[,c(16,22,28,34,40)], main= "Products Purchased", horizontal = TRUE, outline = FALSE,las=2)

#Shop 1-5 Unique products purchased
boxplot(supermarkets[,c(17,23,29,35,41)], main= "Unique Products Purchased", horizontal = TRUE, outline = FALSE,las=2)

#Shop 1-5 Amount purchases shops
boxplot(supermarkets[,c(18,24,30,36,42)], main= "Amount purchases per shop", horizontal = TRUE, outline = FALSE,las=2)

#Shop 1-5 Average purchased
boxplot(supermarkets[,c(19,25,31,37,43)], main= "Average purchased", horizontal = TRUE, outline = FALSE,las=2)

#Shop 1-5 Average price per Shop
boxplot(supermarkets[,c(20,26,32,38,44)], main= "Average price per shop", horizontal = TRUE, outline = FALSE,las=2)

#-----SUMMARY PER SHOP-----
#Shop 1
Shop_1<-supermarkets[,c(15:20)]
summary(Shop_1)

#Shop 2
Shop_2<-supermarkets[,c(21:26)]
summary(Shop_2)

#Shop 3
Shop_3<-supermarkets[,c(27:32)]
summary(Shop_3)

#Shop 4
Shop_4<-supermarkets[,c(33:38)]
summary(Shop_4)

#Shop 5
Shop_5<-supermarkets[,c(39:44)]
summary(Shop_5)

#Overview of the boxplots: déterminet lesquels nous allons utiliser
boxplot(supermarkets[,c(2)], main="Products Purchased", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(3)], main="Unique Products Purchased", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(4)], main="Amount Purchases", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(5)], main="Average Purchase", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(6)], main="Average Price", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(7)], main="Shops Used", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(8)], main="Average distance to shops", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(9)], main="Min distance to shops", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(10)], main="Max distance to shops", horizontal = TRUE, outline = FALSE, las=2)