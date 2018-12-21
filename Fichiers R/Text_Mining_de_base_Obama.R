################################################################################
# Text mining de base
# =========================================
# !!! ATTENTION !!!
# Il n'y a pas de slides pour cette partie.
################################################################################
# Sources d'inspiration:
browseURL("https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html")
browseURL("https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/")
browseURL("http://francoisguillem.fr/2012/04/la-campagne-presidentielle-analysee-statistiquement/")
# Just for fun ;)
browseURL("http://www.degeneratestate.org/posts/2016/Apr/20/heavy-metal-and-natural-language-processing-part-1/")
#=======================
# Préalables
#=======================
# On utlise le package "tm" (Text Mining Package)
install.packages("tm") # si non encore installé
library(tm) # Chargement du package

#=======================
# Les données
#=======================
# Les données viennent du blog ci-dessous
browseURL("http://francoisguillem.fr/2012/04/la-campagne-presidentielle-analysee-statistiquement/")
# Elles sont constituées des textes electoraux des différents 
# candidats aux élections présidentielle française de 2012/
# Vous trouverez ces données dans le fichier nommé "25_France2012.zip" sur
# ICHEC campus.
# Vous pouvez aussi directement le télécharger avec la commande ci aprés
# (pour autant que vous soyez déjà connecté à ICHEC campus ):
browseURL("http://icheccampus.ichec.be/claroline/backends/download.php?url=LzI1X0ZyYW5jZTIwMTIuemlw&cidReset=true&cidReq=21MQ010")

# Le fichier est à décompresser (dézipper). Je vous conseille de ne 
# rien laisser d'autre que les textes dans le dossier où vous décompresserez 
# ceux-ci. 
# Une fois que cela est fait, on  stocke dans une variable le chemin dudit 
# répertoire contenant les textesà analyser:
cname <-"/Users/EC/Dropbox/Teaching/ICHEC/MQ/Cours/MQ1-05/texts/Obama"

# Chargement du corpus
docs<-Corpus(DirSource(cname, encoding = "UTF-8"))

# La commande ci-aprés permet de stoker les noms des textes car l'expérience 
# montre qu'ils peuvent être perdus lors des manipulations...
docnames<-names(docs)

# Afficher le texte numéro1:
as.character(docs[[1]])

# Afficher le même texte mais sans les numéros de lignes:
writeLines(as.character(docs[[1]]))

#=======================
# Preprocessing
#=======================
# Un certain nombre de manipulations sont nécessaires avant de pratiquer 
# les analyse numériques. Les deux plus importantes sont les suivantes:
#  - la lemmatisation (stemming en anglais):
#     La lemmatisation d'une forme ou d'un mot consiste à en prendre sa forme
#     canonique qui est définie comme suit :
#     (a) pour un verbe : ce verbe à l'infinitif,
#     (b) pour les autres mots : le mot au masculin singulier.
#     On notera donc que toutes les entrées d'un dictionnaire sont lemmatisées 
#     et qu'il en est de même pour les titres des articles de Wikipédia 
#     (au moins ceux constitués d'un seul lemme). 
#     La lemmatisation permet en quelle que sorte de "remonter"  à la racine du
#     mot, quel que soit le contexte où il est utilisé.
#  - La suppresions des mots de liaison (stowrods en anglais). Il s'agit des
#    mots "non signifiants" mais nécessaires à la construction d'une phrase. 
#    Exemples : le, la, les, et, de, des, un, une, ou ,...

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs<-tm_map(docs, tolower)
# To plain text:
# La manipulation ci-dessus tranforme la nature des objets de 
# "PlainTextDocument" a "character" ce qui posera problème par la suite,
# on indique dons de nouveau que les objets sont des textes:
docs<-tm_map(docs, PlainTextDocument)

# La même manip nous a fait perdre les noms des textes, 
# on réinjecte les noms qui ont été perdus :
names(docs)<-docnames


# Remove stopwords
docs<-tm_map(docs, removeWords, stopwords("en"))
# Remove stranges characters notces in the texts
docs<-tm_map(docs, removeWords, c("applause", "%"))
# Remove Punctuation
docs<-tm_map(docs, removePunctuation)
# Stripping whitespaces
docs<-tm_map(docs, stripWhitespace)


# Lemmatisation/Stemming
# Le package appelé par "tm" pour effectuer la lemmatisation est
# le package "SnowballC"
install.packages("SnowballC")  # If necessary
library(SnowballC)

# Remarque: en classe nous avons effectué la lemmatisation avec la 
# commande ci-dessous, mais après examen approfondi, il manquait l'argument
# language="french", mais celui-ci doit être transféré à une autre méthode
# appelée par tm_map mais apparement la transmission de ce paramètre ne 
# se fait pas. Je conseille donc de passer la commande ci-dessous pour une
# autre langue que l'anglais a priori. 
#docs<-tm_map(docs, stemDocument)


#=======================
# Processing the Data
#=======================

# On créer une matrice de comptage qui aura en lignes les différents texte (
# une ligne correspond à un texte) et colonnes les mots rencontrés dans tous
# les textes, à l'exception de tous les "nettoyages déjà" faits. 
# L'élément [i,j] de  cette matrice contiendra le nombre de fois que le mot j
# se retrouve dans le texte i.
# Comme cette matrice contiendra beaucoup plus de zéros que de valeurs non
# nulles , on parle de "sparse matrix". 
# Une forme de stockage spécifique est prévue pour éviter
# d'occuper de l'espace mémoire pour les valeurs nulles...

dtm <- DocumentTermMatrix(docs,
        control = list(removePunctuation = TRUE,
                       stopwords = TRUE,
                       stripWhitespace = TRUE,
                       stemming = stemDocument))
# Notez bien que si cette commande propose en options la suppression 
# des mots de liaison, de la ponctuation, et des espaces, il semblerait
# que le commandes effectuées ci-dessus sont plus efficaces...

# Un petit coup d'oeil aux mots gardés sous leur forme lemmatisée:
colnames(dtm)

# Frequencies of words
freq<-colSums(as.matrix(dtm))
# On réalise un index (la liste des numéros de mots à suivre pour les afficher
# dans l'ordre décroissant de fréquences ):
ord<-order(freq, decreasing=TRUE)

# Words frequencies
# Most frequently occuring words
freq[head(ord)]
# Les mots les plus utilsés et par qui:
inspect(dtm[,head(ord, n=10)])

# On peut recalculer une autre matrice de comptage en ajoutant comme
# contrainte de n'accepter que les mots qui ont au moins 3 caractères
# et qui apparaissent au moins une fois dans chaque document (bounds/global)
# et au moins deux fois chaque texte (bounds/local):
dtmr<-DocumentTermMatrix(docs,
          control=list(wordLengths=c(3,Inf),
                       bounds=list(global=c(1,Inf), local=c(2,Inf)),
                       removePunctuation = TRUE,
                       stopwords = TRUE,
                       stripWhitespace = TRUE,
                       stemming = stemDocument))


#=======================
# Mots les plus féquents
#=======================
# On calcule de nouveau un vecteur de fréquences:
freqr<-colSums(as.matrix(dtmr))
# Un index
ordr<-order(freqr, decreasing=TRUE)
# Words frequencies
# Most frequently occuring words
freq[head(ord)]
# Les mots les plus utilsés et par qui:
inspect(dtmr[,head(ordr, n=10)])

# Une autre façon de trouver les mots les plus utilisés:
findFreqTerms(dtmr, lowfreq=40)

#On construit un data.frame avec les fréquences de mots:
mfw<-data.frame(word=names(freqr), freq=freqr)
# On s'assure que les mots sont dans l'ordre de fréquences
mfw<-mfw[order(mfw[,2], decreasing=TRUE),]

# On fait un barplot des fréquences
barplot(mfw[,2], names.arg = mfw[,1],las=2, horiz = TRUE)
# Heu il y a trop de mots, on n'y voit rien :D
# On crée un nouvel index pour ne retenir que les mots apparaissant au 
# moins 20 fois:
mfw2<-subset(mfw, freqr[ordr]>20)
# On refait le barplot en ajoutant une palette de couleur (avec la 
# fonction heatcolors())  contenant autant de couleur que le nombre 
# de lignes(de mots donc) retenues dan smfw2.
barplot(mfw2[,2], names.arg = mfw2[,1],las=2,
        horiz = TRUE, main="Most Frequent Words",
        col=heat.colors(dim(mfw2)[1]))


# Les nuages de mots sont une façon plus agréable de repésenter les mots 
# les plus féquents
install.packages("wordcloud") # si nécessaire
library(wordcloud)

# Wordcloud de tous les mots
#wordcloud(names(freq), freq, random.order = FALSE)

# Most frequent words
wordcloud(names(freq), freq, min.freq = 15, random.order = FALSE)

# Plot the 100 most frequent words
wordcloud(names(freq[ord]), freq[ord], max.words = 100, random.order = FALSE)
# Add some colors
wordcloud(names(freq[ord]), freq[ord], max.words = 100, 
          colors = brewer.pal(6, "Dark2"), random.order = FALSE)

# La même chose mais basé sur les mots les plus fréquents contenus dans
# le dataset mfw:
wordcloud(mfw[,1], mfw[,2],  max.words=100,
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)

# Idem mais en fixant la proportion de mots affichés verticalement (moins 
# lisibles) à 10%:
wordcloud(mfw[,1], mfw[,2], max.words = 100,
          colors = brewer.pal(8, "Dark2")
          , rot.per = 0.1, random.order = FALSE)


# Relationships between words 
# On cherche les mots les plus souvent utilisés avec la racine "franc":
findAssocs(dtmr,"america" , corlimit = 0.8)
# Le paramètre corlimit  donne la corélation  minimale nécessaire.
# La corélation est calculée classiquement ssur la matrice de données
# fournie, ici la matrice ce comptage dtmr.
# Si les mots apparaissent toujours ensembles la corélation sera égale à 1.


# Autre exemple:
findAssocs(dtmr,"job" , corlimit = 0.8)

#Remarque : il est plus interressant de chercher les associations
# sur une matrice de termes fréquents pour éviter 
# les corrélations hautes sur des termes peu fréquents (voire sur des mots 
# n'apparaissant qu'une seule fois)...

# Il est possible de tracer un graphe des mots en relation avec le package
# Rgraphviz, mais celui-ci nécessite d'ajouter le repository Bioconductor
# (un repository est un serveur conservant les packages R):
setRepositories(addURLs ="https//bioconductor.org")
install.packages("Rgraphviz")
library(Rgraphviz)
# Graphe des mots apparaissant au moins 35 fois avec une corrélation 
# minimale de 60% :
plot(dtmr, terms=findFreqTerms(dtmr, lowfreq=110)
     , corThreshold=0.65 )

#=======================
#Clustering by Term Similarity
#=======================
#To do this well, you should always first remove a lot of the uninteresting or 
#infrequent words. If you have not done so already, you can remove these with 
#the following code.
dtmss <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is only 25% empty space, maximum.   
inspect(dtmss)   
#Hierarchal Clustering
#First calculate distance between words & then cluster them according to similarity.
library(cluster)   
# Hclust on words
# Distance Matrix:
# Remarque: la transposée t() de la matrice permet de traiter les mots comme
# les "objets" ou "individus" avec comme variables les textes...
d <- dist(t(dtmss), method="euclidian")   
# Clustering:
fit <- hclust(d=d, method="ward.D")   
# Dendrogram:
plot(fit)
plot(fit, hang=-1)# Pour avoir tous les mots à la même hauteur
# Groupons les mots en 10 groupes:
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using
# On ajoute les groupes au dendrogramme:
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around 
                                     # the 10 clusters   

# Sizes of the clusters
table(groups)
# see the clusters
colnames(dtmss[,groups==4])
colnames(dtmss[,groups==5])
colnames(dtmss[,groups==3])
#...


# Hclust on Years
# Distance Matrix:
dc <- dist(dtmss, method="euclidian")   
dfit <- hclust(d=dc, method="ward.D")   
plot(dfit, hang=-1)
groups <- cutree(dfit, k=2)   # "k=" defines the number of clusters you are using   
rect.hclust(dfit, k=2, border="red") # draw dendogram with red borders around the 5 clusters   

# Kmeans on Years
library(fpc)   
kfit <- kmeans(dtmss, 2)   
clusplot(as.matrix(dc), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

# Kmeans on words
kfit <- kmeans(t(dtmss), 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

#PCA
install.packages("FactoMineR")
library(FactoMineR)
pca <- PCA((as.matrix(dtmss)), scale.unit = F)
