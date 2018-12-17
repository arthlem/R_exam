#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

supermarkets <- read.csv2(file.choose(), header=TRUE, sep=",", dec=".") #charger le fichier CSV
#Commande pour afficher le CSV des supermarchés
#View(supermarkets)
#France fout le caff