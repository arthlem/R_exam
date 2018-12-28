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

#Check the duplicates
install.packages("tidyverse")
library(tidyverse)

#DuplicatedOnlineretail <- duplicated(OnlineretailClean, incomparables = FALSE, MARGIN = 1, fromLast = FALSE)
DuplicatesOR <- duplicated(OnlineretailClean)

#Removing the duplicates
UniqueOnlineretail <- unique(OnlineretailClean)
UniqueOnlineretail

dim(OnlineretailClean)-dim(UniqueOnlineretail)