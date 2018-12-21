################################################################################
#               ICHEC - Cours MQ1 - Année Académique 2016-2017                 #
################################################################################
#                   L'Analyse en Composantes Principales - ACP                 #
#                       Principal Component Analysis - PCA                     #
################################################################################
# Remarque importante : les commandes R qui seront détaillées ci-dessous 
# sont (au choix):
# - à recopier manuellement dans l'interface R choisie (option pas si 
#   inintéressante en termes d'apprentissage)
# - à copier puis à coller dans l'interface R,
# - à envoyer du fichier vers l'interface en utilisant la fonction ad hoc de 
#   l'outil choisi (RStudio, Tinn-R, ...)
################################################################################
# Leçons du 19/04/2017                                                         #
################################################################################ 
# Partie 1 : Les bases de l'ACP                                                #
################################################################################ 
# Etape 1 : le dataset
##############################

# Pour cette introduction à l'ACP nous allons utiliser un mini fichier de données
# Ce dataframe contient les mesures (longueur, largeur, hauteur) et le genre
# de 48 tortues.
# Plus d'infos (exécuter la commande dans R(-Studio) ou copier-coller l'URL dans 
# votre navigateur):

library(ade4)
data(tortues)
?tortues
names(tortues)
sexcol <- ifelse(tortues[,"sexe"] == "M", "blue", "red")

# Etape 2 : Découverte du fichier (facultatif mais informatif)
##############################################################


# Plot d'une scatterplot matrix des données:
mesures <- tortues[ , 1:3]
pairs(mesures, col=sexcol, pch=19)

#Affichons les matrices de corrélation:
cor(mesures)

###############################################################################
# La partie suivante ne fait pas partie de l'ACP mais elle est destinée à
# faire comprendre le but ultime de cette dernière : la projection dans le 
# plan de dispersion maximale (si il existe).
# Les commandes utilisées ne sont pas à comprendre, mais simplement à utiliser.
###############################################################################
# Utilisation du package rgl permettant de faire de  la visualisation 
# 3D interactive.
# Téléchargement et installation du package (cette commande n'est à exécuter
# qu'une seule fois pour un ordinateur donné):
#install.packages("rgl") 
# Chargement de package dans le système:
library(rgl)

# Graphique 3D des 3 premières variables des données: 
#plot3d(mesures, type = "s", col = sexcol)
#plot3d(scale(mesures, center = TRUE, scale = TRUE), type = "s", col = sexcol)
plot3d(mesures, type = "p",size= 10, col = sexcol)
plot3d(scale(mesures, center = TRUE, scale = TRUE), type = "p",size= 10, col = sexcol)

# "Enveloppe" des données:
plot3d(ellipse3d(cor(mesures),scale=diag(cor(mesures)), col = "grey", alpha = 0.5), add=TRUE)
# Visualisation des 3 premiers axes originaux 
lines3d(rbind(c(0,0,0),c(2,0,0)),lwd=4, col="brown")
lines3d(rbind(c(0,0,0),c(0,2,0)),lwd=4, col="blue")
lines3d(rbind(c(0,0,0),c(0,0,2)),lwd=4, col= "green")
# "Jouez" avec le graphe jusqu'à visualiser les données dans leur dispersion 
# maximum :)
# Ce plan de dispersion maximale est celui que cherche numériquement l'ACP
# et ce indépendemment du nombre de variables initiales (la visu 3D n'étant
# pas toujours possible).
#
# Retour à l'ACP 
###############################################################################
mesures.CR<-scale(mesures, center = TRUE, scale = TRUE)

# La dispersion (écart-type et donc variance) est la caractéristique qui 
# permet de distinguer les données les unes des autres: sans dispersion
# toutes les données sont indistinguables.
# La dispersion est donc utilisée comme mesure de l'information contenue
# dans l'ensemble de données. Cette évaluation de la dispersion se fait 
# en calculant la matrice de variance-covariance:
Covar<-cov(mesures.CR)

# L'affichage est facultatif car la covariance est moins directement 
# interprétable que la corrélation.
# Les covariance donnent une mesure de la façon dont les variables
# varient les unes par rapport aux autres: comment elles covarient.
# Les corélations sont calculées à partir des covariances et variances.

# Les variances sont sur la diagonale de la matrice de variance-covariance:
# La diagonale d'une matrice est directement accessible via la fonction diag():
diag(Covar)

# La variance totale est la somme des variances des différentes variables
# originelles. Cette variance totale (aussi appelée inertie) servira de
# mesure de référence pour la mesure de la perte d'information durant l'ACP.
# Cette variance totale se calcule simplement en faisant la somme (fonction 
# sum()) de la diagonale de la matrice de variance-covariance :
total.var<-sum(diag(Covar))
# La somme des éléments de la diagonale d'une matrice carrée est aussi 
# appelée la trace de la matrice.

# Etape 2 : l'ACP "manuelle"
############################
# Comme expliqué dans les slides, l'ACP cherche les directions du 
# nuage de données ayant le maximum de dispersion.
# Cette recherche a comme solution les vecteurs propres de la matrice 
# de variance-covariance:
# - les vecteurs propres donnent les directions cherchées,
# - chaque vecteur propre est une nouvelle variable construite comme
#   combinaison linéaire des anciennes variables (appelées composantes),
# - ces vecteurs propres sont donnés sous forme d'une matrice,
# - les colonnes de cette matrice donnnent les coordonnées (projections)
#   de ces nouvelles variables dans le système des variables originelles,
# - les valeurs propres donnent les variances dans les directions trouvées,
# - ces valeurs propres sont classées dans l'ordre décroissant.

# Calcul des valeurs propres de la matrice de covariance
my.eig<-eigen(Covar)
# Les valeurs propres
my.eig$values
# Les vecteurs propres
my.eig$vectors

# Une caractéristique des vecteurs propres et valeurs propres est que 
# la matrice diagonale constituée des valeur propres représente la 
# même "information" (ici la variance-covariance des données) que la
# matrice originale, MAIS dans le nouveau système de  variables.
# De plus les traces des deux matrices sont égales. Cette information 
# reste donc inchangée par le changement de coordonnées.
# On peut donc mesurer l'information (variance) concentrée sur chacune 
# de ces nouvelles variables à partir de la variance totale calculée plus
# haut:
my.eig$values/total.var

# Comme les nouvelles variables (composantes, vect. pr.) sont classées
# par ordre décroissant de variances (valeur pr.) on peut calculer
# la quantité d'information contenue de façon cumulative dans les 
# x premières composantes : 
cumsum(my.eig$values/total.var)
# La fonction cumsum() fait une somme cumulative.
# Dans notre cas la somme cumulative se termine par 1: dans le cas
# des tortues les 3 premières composantes contiennent 100% de la variance totale
# (ce qui est une lapalissade :).

# On constate que la première composante contient 92% de la variance,
# et que les 2 premières composantes contiennent prés de 98% de la variance
# totale. En se concentrant sur ces deux composantes (et  en ignorant 
# les autres, les deux dernières dans le cas présent) on ne perd que 2.3%
# d'information. Les composantes retenues sont appelées les composantes 
# principales (d'où ACP).

# Le calcul  des vecteurs propres (composantes) et des valeurs propres 
# (variances des composantes) correspondantes ne sont pas les seules choses 
# à faire pour une ACP, il faut aussi:
# (1) calculer les coordonnées (projections) des individus dans les nouvelles
#   variables,
# (2) calculer les projections des variables originelles dans les composantes
#   (notamment pour des questions d'interprétations des composantes, cf. infra).

#Procédons:
# (1) calculer les coordonnées (projections) des individus dans les nouvelles
#   variables,

Q<-my.eig$vectors # matrice de transformation
Qinv<-solve(Q) # Inverse de Q
yinMypc<-t(Qinv%*% t(mesures.CR)) # On passe via des transposées car en Algèbre
# les coordonnées sont des vecteurs colonnes
# alors que nos données sont habituellement 
# stockées de telle façon que les valeurs
# (coordonnées dans l'espace originel) sont 
# stockées sous forme de lignes.

plot(yinMypc[,1:2],col=sexcol) # On plot les 2 premières composantes des tortues
abline(v=0,h=0) # on ajoutes les nouveaux axes

# (2) calculer les projections des variables originelles dans les composantes
#   (notamment pour des questions d'interprétations des composantes, cf. infra).

#   Calcul des coordonnées des vecteurs de la base canonique (aka les directions 
# de nos variables originales)  dans la nouvelle base de composantes:
pc<-t(Qinv)
# On tace ces vecteurs sur le graphique
arrows(0,0, pc[1,1], pc[1,2], col="red")
arrows(0,0, pc[2,1], pc[2,2], col="red")
arrows(0,0, pc[3,1], pc[3,2], col="red")

# calcul des contributions de chaque variable originale dans la composition
# de chaque composante:
#
#  1) Si les composantes (les vecteurs propres) sont normées:
#   ^  |       .
#   |  |   1 .
# y |  |   .          x^2+y^2= 1 ==> x^2 et y^2 représentent les contributions 
#   v  | .                         
#       --------
#       <------>
#          x
# 
#  2) Si les composantes ne sont pas normées, il suffit de diviser les carrés
#     x^2 et y^2 par le carré de la norme de la composante.
#     

# Contributions des variables orginales :
pc^2
# En %
pc^2 *100
# En 1000 èmes
pc^2*10000
# En 1000 èmes arrondis (cf. infra)
round(pc^2*10000)


# Nous allons donc maintenant faire appel aux outils de base de R pour faire l'ACP:
# la fonction princomp()

# Etape 3 : l'ACP de base dans R: princomp()
############################################

# PCA de base en R:
pca.r <- princomp(mesures.CR)
# On stocke le résultat dans une variable car princomp calcule
# plusieurs choses (cf. ci-dessus).
# Pour avoir un résumé de l'ACP:
summary(pca.r)

# Attention la première ligne de la commande ci-dessus donne, non pas 
# les variances (!) mais bien les écart-types, càd les racines carrées
# des premières.
# La commande ci-dessus est l'équivalent des trois commandes suivantes:
# sqrt(my.eig$values)
# my.eig$values/sum(my.eig$values)
# cumsum(my.eig$values/sum(my.eig$values))

# Comme un petit dessin vaut mieux qu'un long discours on donne souvent 
# un barplot des valeurs propres. Ce graphe est appelé screeplot ou encore 
# graphe des éboulis:
plot(pca.r) # shows a screeplot.
# La décroissance (forte ou non) valeur propres successives donne un information
# sur le nombre de ocmposantes qui concentrent l'information.

# Notons que le screeplot peut aussi être fait maunellement:
# barplot(my.eig$values,ylab="Variances of Components")

# La fonction loading() permet de connaître les coordonnées des composantes
# dans les variables originales, et donc le poids de chacune des variables
# originales dans le composition des nouvelles variables (les composantes):

loadings(pca.r)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary

# C'est l'information que nous avions en affichant les vecteurs propres : 
#my.eig$vectors

# Enfin la commande biplot() permet enfin de dessiner les projections des 
# données sur les 2 premières composantes ainsi que les projections sur ces
# deux composantes des variables originales:
biplot(pca.r)

# En résumé une ACP avec les outils de base de R se résume, pour les données
# tortues tient donc dans les commandes suivantes:

pairs(mesures) # si opportun par rapport au nombre de variables
cor(mesures) # informatif
pca.r <- princomp(mesures) # ACP
summary(pca.r) # valeurs propres et % d'infos dans les composantes
plot(pc.cr)  # idem mais visuellement
loadings(pca.r) # poids des variables originelles dans les composantes
biplot(pca.r) # projection "finale"

# Néanmoins cette dernière projection n'est pas toujours complétement lisible
# c'est pourquoi un certain nombres de packages ont développé leurs propres
# outils d'ACP. 


# Etape 4 : l'ACP en détail avec un l'aide de packages externes
###############################################################

# Utilisation du package ade4 permettant de faire de  l'ACP de façon plus 
# détaillée qu'en utilisant la fonction princomp de base.

# Téléchargement et installation du package (cette commande n'est à exécuter
# qu'une seule fois pour un ordinateur donné):
#install.packages("ade4") 
# Chargement de package dans la session R:
library(ade4)

# Exécution de l'ACP:
# - la fonction peut demander  interactivement le nombre d'axes à retenir,
#   et pour permettre ce choix elle affiche directement le screeplot:
#acp.ade4<-dudi.pca(mesures,center = TRUE, scale = TRUE )


# Si on préfère ne pas avoir d'interruption interactive qui demande le nombre
# d'axes, alors on  passe scannf à FALSE et le nombre d'axes à retenir via nf:
acp.ade4<-dudi.pca(mesures, scannf=FALSE, nf=3,center = TRUE, scale = TRUE )

# Dans les deux cas il est conseillé de centrer(center=TRUE)-réduire(scale=TURE)
# les données :
# - si on ne centre pas, la première composante sera la direction qui va 
#   de l'origine (point où toutes les variables originales sont nulles)au 
#   centre du nuage.
# - la réduction permet d'éviter les effets des différences importantes d'échelles 
#   entre les variables.

# Impression des valeurs propres
acp.ade4$eig
# Les variances cumulées
cumsum(acp.ade4$eig)
# Les variances en pourcentages:
acp.ade4$eig/sum(acp.ade4$eig)*100
# Le screeplot:
barplot(acp.ade4$eig/sum(acp.ade4$eig)*100)
# Les pourcentages cumulés :
cumsum(acp.ade4$eig/sum(acp.ade4$eig)*100)


# Le problème de l'ACP étant l'interprétation des nouveaux axes (les 
# composantes) puisqu'ils sont formés par combinaisons linéaires
# des anciennes variables. Càd que les composantes sont un mix
# des variables initiales.
# Une première façon de "comprendre" ces composantes est de regarder les 
# vecteurs propres qui contiennent les coefficients des combinaisons 
# linéaires évoquées ci-dessus:
acp.ade4$c1

# On peut aussi avoir une idée de la décompostion l'inertie (la part de la variance totale 
# expliquée) entre les variables et composantes (en 10000 èmes):
inertia.dudi(acp.ade4,col.inertia = T)$col.abs


# Le package ade4 fournit aussi d'autres outils.
# Plot des "droites" de corrélation des variables avec les deux premières 
# composantes: 
score(acp.ade4, xax=1)
score(acp.ade4, xax=2)
# Ces graphiques permettent de voir les liaisons entre les composantes et les variables.



# On peut tracer les cercles de correlation où la longueur des flèches 
# indique la part de leur information représentée par les deux axes. 
# L'angle entre deux  flèches représente la corrélation qui les lie : 
# - angle aigu = positive;
# - angle droit = nulle;
# - angle obtus = négative.
s.corcircle(acp.ade4$co)

# Enfin on peut passer aux projections des données dans les nouveaux axes.
# Représentation à la fois les individus et des variables dans le premier plan
# factoriel (deux premières composantes):
scatter(acp.ade4)
# Idem sans l'histogramme des valeurs propres
scatter(acp.ade4, posieig="none")
# Idem mais sans étiquettes, les individus étant représenté par des points
scatter(acp.ade4, posieig="none", clab.row=0)
# Comparez cette visualisation avec la visualisation 3D du debut...

# Ajout des groupes au graphe existant
s.class(dfxy=acp.ade4$li, fac=tortues[,"sexe"], add.plot=TRUE)

# Nouveau graphe
scatter(acp.ade4, posieig="none", clab.row=0)
# Groupes en couleur
s.class(dfxy=acp.ade4$li,fac=tortues[,"sexe"], add.plot=TRUE,col=c("red","blue","green"))

# On termine avec un nouveau layout comme ci-dessus pour reprendre les 
# différents graphiques en un seulplot.
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
barplot(acp.ade4$eig[1:acp.ade4$rank])
s.corcircle(acp.ade4$co)
s.label(acp.ade4$li)
#scatter(acp, posieig="none", clab.row=0)
s.class(dfxy=acp.ade4$li,fac=tortues[,"sexe"], add.plot=FALSE,col=sexcol)
layout(matrix(1:1, 1, 1, byrow = TRUE))

# On peut aussi ajouter les coordonnées des données dans les nouveaux axes pour faciliter
# des représentations ultérieures.
# Ajout des composantes au data frame initial:
tortuesPC<-cbind(tortues,acp.ade4$li)


# D'autres fonctions d'autres packages existent, on en trouvera une liste
# (certainement non-exhaustive) à la page suivante :
browseURL("http://gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/")



# Exercices:
# 1) Effectuez l'ACP et l'analyse de ses résultats pour les données swiss:
data(swiss, package="datasets")
?swiss
swiss
cor(swiss)
#acp.ade4<-dudi.pca(swiss,center = TRUE, scale = TRUE )
acp.ade4<-dudi.pca(swiss, scannf=FALSE, nf=6,center = TRUE, scale = TRUE )
acp.ade4$eig
acp.ade4$eig/sum(acp.ade4$eig)*100
cumsum(acp.ade4$eig/sum(acp.ade4$eig)*100)
barplot(acp.ade4$eig/sum(acp.ade4$eig)*100)
cumsum(acp.ade4$eig/sum(acp.ade4$eig)*100)
acp.ade4$c1
inertia.dudi(acp.ade4,col.inertia = T)$col.abs
score(acp.ade4, xax=1)
score(acp.ade4, xax=2)
score(acp.ade4, xax=3)
score(acp.ade4, xax=4)
s.corcircle(acp.ade4$co)
s.corcircle(acp.ade4$co,xax = 1, yax = 2)
s.corcircle(acp.ade4$co,xax = 1, yax = 3)
s.corcircle(acp.ade4$co,xax = 2, yax = 3)
scatter(acp.ade4, posieig="none", clab.row=0)
scatter(acp.ade4, posieig="none")

# 2) Effectuez l'ACP et l'analyse de ses résultats pour les données USArrest des TP précédents 
#    (pour les 4 première variables seulement).

# 3) Utilisez RStudio pour créer un rapport explicitant une des deux ACP ci-dessus (au choix), 
#    en créant un nouveau document Markdown (option Word).


# Pour terminer éventuellement :
# L'ACP avec Rcommander
