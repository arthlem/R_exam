# GIT
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull
#

##CHARGEMENT DES DONNEES

supermarkets <- read.csv2(file.choose(), header=TRUE, sep=",", dec=".") #charger le fichier CSV
#Commande pour afficher le CSV des supermarchés
View(supermarkets)
names(supermarkets)

##PARTIE 1: STATISTIQUE DESCRIPTIVE

average_all <- colMeans(supermarkets[,c(2:11,15:44)])
average_all
#Changer les marges pour les graphiques
old.mar<-par("mar")
par(mar=c(5,10,4,2))
#Boxplot de tout mais trop de variables donc illisibles
boxplot(supermarkets[,c(2:11,15:44)], horizontal = TRUE, outline = FALSE,las=2)
#Boxplot par catégorie (distance_shop1-5)
boxplot(supermarkets[,c(15,21,27,33,39)], horizontal = TRUE, outline = FALSE,las=2)
#Overview of products bougth
boxplot(supermarkets[,c(2)], main="Products Purchased" horizontal = TRUE, outline = FALSE, las=2)
boxplot(supermarkets[,c(3)], main="Unique Products Purchased", horizontal = TRUE, outline = FALSE, las=2)
