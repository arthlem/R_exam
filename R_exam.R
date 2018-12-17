#Lors de changements
# git add . && git commit -m "Le message a envoyer" && git push

#Pour récupérer les changements
# git pull

supermarkets <- read.csv2(file.choose(), header=TRUE, sep=",", dec=".") #charger le fichier CSV
#Commande pour afficher le CSV des supermarchés
#View(supermarkets)