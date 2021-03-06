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
# Pr?alables
#=======================
# On utlise le package "tm" (Text Mining Package)
install.packages("tm") # si non encore install?
library(tm) # Chargement du package

#=======================
# Les donn?es
#=======================
# Les donn?es viennent du blog ci-dessous
browseURL("http://francoisguillem.fr/2012/04/la-campagne-presidentielle-analysee-statistiquement/")
# Elles sont constitu?es des textes electoraux des diff?rents 
# candidats aux ?lections pr?sidentielle fran?aise de 2012/
# Vous trouverez ces donn?es dans le fichier nomm? "25_France2012.zip" sur
# ICHEC campus.
# Vous pouvez aussi directement le t?l?charger avec la commande ci apr?s
# (pour autant que vous soyez d?j? connect? ? ICHEC campus ):
browseURL("http://icheccampus.ichec.be/claroline/backends/download.php?url=LzI1X0ZyYW5jZTIwMTIuemlw&cidReset=true&cidReq=21MQ010")

# Le fichier est ? d?compresser (d?zipper). Je vous conseille de ne 
# rien laisser d'autre que les textes dans le dossier o? vous d?compresserez 
# ceux-ci. 
# Une fois que cela est fait, on  stocke dans une variable le chemin dudit 
# r?pertoire contenant les textes? analyser:
cname <-"./R_exam/TextMining/"

# Chargement du corpus
docs<-Corpus(DirSource(cname, encoding = "UTF-8"))

# La commande ci-apr?s permet de stoker les noms des textes car l'exp?rience 
# montre qu'ils peuvent ?tre perdus lors des manipulations...
docnames<-names(docs)

# Afficher le texte num?ro1:
as.character(docs[[1]])

# Afficher le m?me texte mais sans les num?ros de lignes:
writeLines(as.character(docs[[1]]))

#=======================
# Preprocessing
#=======================
# Un certain nombre de manipulations sont n?cessaires avant de pratiquer 
# les analyse num?riques. Les deux plus importantes sont les suivantes:
#  - la lemmatisation (stemming en anglais):
#     La lemmatisation d'une forme ou d'un mot consiste ? en prendre sa forme
#     canonique qui est d?finie comme suit :
#     (a) pour un verbe : ce verbe ? l'infinitif,
#     (b) pour les autres mots : le mot au masculin singulier.
#     On notera donc que toutes les entr?es d'un dictionnaire sont lemmatis?es 
#     et qu'il en est de m?me pour les titres des articles de Wikip?dia 
#     (au moins ceux constitu?s d'un seul lemme). 
#     La lemmatisation permet en quelle que sorte de "remonter"  ? la racine du
#     mot, quel que soit le contexte o? il est utilis?.
#  - La suppresions des mots de liaison (stowrods en anglais). Il s'agit des
#    mots "non signifiants" mais n?cessaires ? la construction d'une phrase. 
#    Exemples : le, la, les, et, de, des, un, une, ou ,...

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs<-tm_map(docs, tolower)
# To plain text:
# La manipulation ci-dessus tranforme la nature des objets de 
# "PlainTextDocument" a "character" ce qui posera probl?me par la suite,
# on indique dons de nouveau que les objets sont des textes:
docs<-tm_map(docs, PlainTextDocument)

# La m?me manip nous a fait perdre les noms des textes, 
# on r?injecte les noms qui ont ?t? perdus :
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
# Le package appel? par "tm" pour effectuer la lemmatisation est
# le package "SnowballC"
install.packages("SnowballC")  # If necessary
library(SnowballC)

# Remarque: en classe nous avons effectu? la lemmatisation avec la 
# commande ci-dessous, mais apr?s examen approfondi, il manquait l'argument
# language="french", mais celui-ci doit ?tre transf?r? ? une autre m?thode
# appel?e par tm_map mais apparement la transmission de ce param?tre ne 
# se fait pas. Je conseille donc de passer la commande ci-dessous pour une
# autre langue que l'anglais a priori. 
#docs<-tm_map(docs, stemDocument)


#=======================
# Processing the Data
#=======================

# On cr?er une matrice de comptage qui aura en lignes les diff?rents texte (
# une ligne correspond ? un texte) et colonnes les mots rencontr?s dans tous
# les textes, ? l'exception de tous les "nettoyages d?j?" faits. 
# L'?l?ment [i,j] de  cette matrice contiendra le nombre de fois que le mot j
# se retrouve dans le texte i.
# Comme cette matrice contiendra beaucoup plus de z?ros que de valeurs non
# nulles , on parle de "sparse matrix". 
# Une forme de stockage sp?cifique est pr?vue pour ?viter
# d'occuper de l'espace m?moire pour les valeurs nulles...

dtm <- DocumentTermMatrix(docs,
        control = list(removePunctuation = TRUE,
                       stopwords = TRUE,
                       stripWhitespace = TRUE,
                       stemming = stemDocument))
# Notez bien que si cette commande propose en options la suppression 
# des mots de liaison, de la ponctuation, et des espaces, il semblerait
# que le commandes effectu?es ci-dessus sont plus efficaces...

# Un petit coup d'oeil aux mots gard?s sous leur forme lemmatis?e:
colnames(dtm)

# Frequencies of words
freq<-colSums(as.matrix(dtm))
# On r?alise un index (la liste des num?ros de mots ? suivre pour les afficher
# dans l'ordre d?croissant de fr?quences ):
ord<-order(freq, decreasing=TRUE)

# Words frequencies
# Most frequently occuring words
freq[head(ord)]
# Les mots les plus utils?s et par qui:
inspect(dtm[,head(ord, n=10)])

# On peut recalculer une autre matrice de comptage en ajoutant comme
# contrainte de n'accepter que les mots qui ont au moins 3 caract?res
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
# Mots les plus f?quents
#=======================
# On calcule de nouveau un vecteur de fr?quences:
freqr<-colSums(as.matrix(dtmr))
# Un index
ordr<-order(freqr, decreasing=TRUE)
# Words frequencies
# Most frequently occuring words
freq[head(ord)]
# Les mots les plus utils?s et par qui:
inspect(dtmr[,head(ordr, n=10)])

# Une autre fa?on de trouver les mots les plus utilis?s:
findFreqTerms(dtmr, lowfreq=40)

#On construit un data.frame avec les fr?quences de mots:
mfw<-data.frame(word=names(freqr), freq=freqr)
# On s'assure que les mots sont dans l'ordre de fr?quences
mfw<-mfw[order(mfw[,2], decreasing=TRUE),]

# On fait un barplot des fr?quences
barplot(mfw[,2], names.arg = mfw[,1],las=2, horiz = TRUE)
# Heu il y a trop de mots, on n'y voit rien :D
# On cr?e un nouvel index pour ne retenir que les mots apparaissant au 
# moins 20 fois:
mfw2<-subset(mfw, freqr[ordr]>20)
# On refait le barplot en ajoutant une palette de couleur (avec la 
# fonction heatcolors())  contenant autant de couleur que le nombre 
# de lignes(de mots donc) retenues dan smfw2.
barplot(mfw2[,2], names.arg = mfw2[,1],las=2,
        horiz = TRUE, main="Most Frequent Words",
        col=heat.colors(dim(mfw2)[1]))


# Les nuages de mots sont une fa?on plus agr?able de rep?senter les mots 
# les plus f?quents
install.packages("wordcloud") # si n?cessaire
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

# La m?me chose mais bas? sur les mots les plus fr?quents contenus dans
# le dataset mfw:
wordcloud(mfw[,1], mfw[,2],  max.words=100,
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)

# Idem mais en fixant la proportion de mots affich?s verticalement (moins 
# lisibles) ? 10%:
wordcloud(mfw[,1], mfw[,2], max.words = 100,
          colors = brewer.pal(8, "Dark2")
          , rot.per = 0.1, random.order = FALSE)


# Relationships between words 
# On cherche les mots les plus souvent utilis?s avec la racine "franc":
findAssocs(dtmr,"america" , corlimit = 0.8)
# Le param?tre corlimit  donne la cor?lation  minimale n?cessaire.
# La cor?lation est calcul?e classiquement ssur la matrice de donn?es
# fournie, ici la matrice ce comptage dtmr.
# Si les mots apparaissent toujours ensembles la cor?lation sera ?gale ? 1.


# Autre exemple:
findAssocs(dtmr,"job" , corlimit = 0.8)

#Remarque : il est plus interressant de chercher les associations
# sur une matrice de termes fr?quents pour ?viter 
# les corr?lations hautes sur des termes peu fr?quents (voire sur des mots 
# n'apparaissant qu'une seule fois)...

# Il est possible de tracer un graphe des mots en relation avec le package
# Rgraphviz, mais celui-ci n?cessite d'ajouter le repository Bioconductor
# (un repository est un serveur conservant les packages R):
setRepositories(addURLs ="https//bioconductor.org")
install.packages("Rgraphviz")
library(Rgraphviz)
# Graphe des mots apparaissant au moins 35 fois avec une corr?lation 
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
# Remarque: la transpos?e t() de la matrice permet de traiter les mots comme
# les "objets" ou "individus" avec comme variables les textes...
d <- dist(t(dtmss), method="euclidian")   
# Clustering:
fit <- hclust(d=d, method="complete")   
# Dendrogram:
plot(fit)
plot(fit, hang=-1)# Pour avoir tous les mots ? la m?me hauteur
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
