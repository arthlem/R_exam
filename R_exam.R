#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

#-------------------------------------------SUPERMARKETS-------------------------------------------------

#IMPORT THE DATA
onlineretail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=".", row.names = NULL) #Load CSV File
View(onlineretail)

#INSPECT THE DATA
str(supermarkets)
dim(supermarkets)
#Check the first part of the data
head(supermarkets)
#Check the last part of the data
tail(supermarkets)

#Arrange the data
price <- supermarkets[ , 5:6]
pairs(price, pch=19)

test <- supermarkets[,c(15,18,21,24)]
pairs(test, pch=19)

test2 <- supermarkets[,c(4,6,8)]
pairs(test2, pch=19)

#View all the data
View(supermarkets)
View(supermarkets[c(20,26,32,38,44)])
#Create Distances sub-DataSet

#11 is not included
distances <- supermarkets[c(8,9,10)]
#Create Prices sub-DataSet
prices <- supermarkets[c(5,6)]

purchases <- supermarkets[c(2,3,4)]
View(purchases)
clients_of_the_shop_3 <- supermarkets[ which(supermarkets$amount_purchases_shop_3 > 0), ]

par(mar=c(5,10,4,2))

boxplot(distances, main= "Distances", horizontal = TRUE, outline = FALSE,las=2)

boxplot(prices, main= "Prices", horizontal = TRUE, outline = FALSE,las=2)

par(mar=c(5,12,4,2))
boxplot(purchases, main= "Purchases", horizontal = TRUE, outline = FALSE,las=2)

#Inspect the data of purchases
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

#PCA
#Pour pouvoir utiliser le PCA il faut charger la libraire "ade4"
# install.packages("ade4")
library(ade4)
#Analyser le script ACP-PCA du prof, normalement à partir de la ligne 74
#Centrer et réduire les données
measures <- supermarkets[ , c(2,4,5,7,13)]
measures.CR<-scale(measures, center = TRUE, scale = TRUE)
View(measures.CR)

install.packages("rgl") 
# Chargement de package dans le systeme:
#library(rgl)

Covar<-cov(measures.CR)

# L'affichage est facultatif car la covariance est moins directement 
# interpr?table que la corr?lation.
# Les covariance donnent une mesure de la fa?on dont les variables
# varient les unes par rapport aux autres: comment elles covarient.
# Les cor?lations sont calcul?es ? partir des covariances et variances.

# Les variances sont sur la diagonale de la matrice de variance-covariance:
# La diagonale d'une matrice est directement accessible via la fonction diag():
diag(Covar)
# La variance totale est la somme des variances des diff?rentes variables
# originelles. Cette variance totale (aussi appel?e inertie) servira de
# mesure de r?f?rence pour la mesure de la perte d'information durant l'ACP.
# Cette variance totale se calcule simplement en faisant la somme (fonction 
# sum()) de la diagonale de la matrice de variance-covariance :
total.var<-sum(diag(Covar))

# Calcul des valeurs propres de la matrice de covariance
my.eig<-eigen(Covar)
# Les valeurs propres
my.eig$values
# Les vecteurs propres
my.eig$vectors

# Une caract?ristique des vecteurs propres et valeurs propres est que 
# la matrice diagonale constitu?e des valeur propres repr?sente la 
# m?me "information" (ici la variance-covariance des donn?es) que la
# matrice originale, MAIS dans le nouveau syst?me de  variables.
# De plus les traces des deux matrices sont ?gales. Cette information 
# reste donc inchang?e par le changement de coordonn?es.
# On peut donc mesurer l'information (variance) concentr?e sur chacune 
# de ces nouvelles variables ? partir de la variance totale calcul?e plus
# haut:
my.eig$values/total.var

# Comme les nouvelles variables (composantes, vect. pr.) sont class?es
# par ordre d?croissant de variances (valeur pr.) on peut calculer
# la quantit? d'information contenue de fa?on cumulative dans les 
# x premi?res composantes : 
cumsum(my.eig$values/total.var)
Q<-my.eig$vectors # matrice de transformation
Qinv<-solve(Q) # Inverse de Q
yinMypc<-t(Qinv%*% t(measures.CR)) # On passe via des transpos?es car en Alg?bre
# les coordonn?es sont des vecteurs colonnes
# alors que nos donn?es sont habituellement 
# stock?es de telle fa?on que les valeurs
# (coordonn?es dans l'espace originel) sont 
# stock?es sous forme de lignes.
cols=c("blue", "red", "green", "pink")
plot(yinMypc[,1:2],col=cols) # On plot les 2 premi?res composantes des tortues
abline(v=0,h=0) # on ajoutes les nouveaux axes
?plot

pca.r <- princomp(measures.CR)
# On stocke le r?sultat dans une variable car princomp calcule
# plusieurs choses (cf. ci-dessus).
# Pour avoir un r?sum? de l'ACP:
summary(pca.r)
?princomp
# Attention la premi?re ligne de la commande ci-dessus donne, non pas 
# les variances (!) mais bien les ?cart-types, c?d les racines carr?es
# des premi?res.
# La commande ci-dessus est l'?quivalent des trois commandes suivantes:
# sqrt(my.eig$values)
# my.eig$values/sum(my.eig$values)
# cumsum(my.eig$values/sum(my.eig$values))

# Comme un petit dessin vaut mieux qu'un long discours on donne souvent 
# un barplot des valeurs propres. Ce graphe est appel? screeplot ou encore 
# graphe des ?boulis:
plot(pca.r) # shows a screeplot.
loadings(pca.r)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary

# C'est l'information que nous avions en affichant les vecteurs propres : 
#my.eig$vectors

# Enfin la commande biplot() permet enfin de dessiner les projections des 
# donn?es sur les 2 premi?res composantes ainsi que les projections sur ces
# deux composantes des variables originales:
biplot(pca.r)

# En r?sum? une ACP avec les outils de base de R se r?sume, pour les donn?es
# tortues tient donc dans les commandes suivantes:

pairs(measures) # si opportun par rapport au nombre de variables
cor(measures) # informatif
pca.r <- princomp(measures) # ACP
summary(pca.r) # valeurs propres et % d'infos dans les composantes
plot(pc.cr)  # idem mais visuellement
loadings(pca.r) # poids des variables originelles dans les composantes
biplot(pca.r) # projection "finale"

