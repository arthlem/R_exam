#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour r√©cup√©rer les changements
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

#Create Distances sub-DataSet
#11 is not included
distances <- supermarkets[c(8,9,10)]
#Create Prices sub-DataSet
prices <- supermarkets[c(5,6)]

#Create Purchases sub-DataSet, column 12, 13 and 14 not included because these are shop ID's
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

#par(mar=c(5,10,4,2)) -> important quand il y a beaucoup de donn√©es

colnames(supermarkets)[20] <- c("avg price shop1")

#Boxplot par cat√©gorie (distance_shop1-5)
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

#Overview of products bougth
boxplot(supermarkets[,c(2)], main="Products Purchased", horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(3)], main="Unique Products Purchased", horizontal = TRUE, outline = FALSE, las=2)

#-----Purchases-----
#MÈdiane sur le nombre total de produit achetÈ et sur le nombre de magasin frÈquentÈ
med_prod_purchase <- median(supermarkets[,c(2)])
med_prod_purchase

med_shop_used <- median(supermarkets[,c(7)])
med_shop_used

#Moyenne sur le nombre magasin frÈquentÈ et le montant des achats
avg_amount_purchase <- mean(supermarkets[,c(4)])
avg_amount_purchase

avg_shop_used <- mean(supermarkets[,c(7)])
avg_shop_used

#DÈrivation standard sur le montant des achats
sd_amount_purchase <- sd(supermarkets[,c(4)])
sd_amount_purchase

#Variation sur les produits uniques
var_unique_product <- var(supermarkets[,c(3)])
var_unique_product

#-----Distance-----
#Moyenne sur les distance jusqu'au shop
avg_dist_to_shop <- mean(supermarkets[,c(8)])
avg_dist_to_shop

avg_dist_max_to_shop <- mean(supermarkets[,c(10)])
avg_dist_max_to_shop

avg_dist_min_to_shop <- mean(supermarkets[,c(9)])
avg_dist_min_to_shop

#Moyenne des distances par shop
avg_dist_shop1 <- mean(supermarkets[,c(15)])
avg_dist_shop1

avg_dist_shop2 <- mean(supermarkets[,c(21)])
avg_dist_shop2

avg_dist_shop3 <- mean(supermarkets[,c(27)])
avg_dist_shop3

avg_dist_shop4 <- mean(supermarkets[,c(33)])
avg_dist_shop4

avg_dist_shop5 <- mean(supermarkets[,c(39)])
avg_dist_shop5

#Distance maximal par shop
max_dist_shop1 <- max(supermarkets[,c(15)])
max_dist_shop1

max_dist_shop2 <- max(supermarkets[,c(21)])
max_dist_shop2

max_dist_shop3 <- max(supermarkets[,c(27)])
max_dist_shop3

max_dist_shop4 <- max(supermarkets[,c(33)])
max_dist_shop4

max_dist_shop5 <- max(supermarkets[,c(39)])
max_dist_shop5

#-----Price-----
#Moyenne des prix des achats
avg_price_purchase <- mean(supermarkets[,c(5)])
avg_price_purchase

#Moyenne des prix
avg_price_product <- mean(supermarkets[,c(6)])
avg_price_product

#Moyenne des prix par shop
avg_price_shop1 <- mean(supermarkets[,c(20)])
avg_price_shop1

avg_price_shop2 <- mean(supermarkets[,c(26)])
avg_price_shop2

avg_price_shop3 <- mean(supermarkets[,c(32)])
avg_price_shop3

avg_price_shop4 <- mean(supermarkets[,c(38)])
avg_price_shop4

avg_price_shop5 <- mean(supermarkets[,c(44)])
avg_price_shop5