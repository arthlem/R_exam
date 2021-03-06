################################################################################
#               ICHEC - Cours MQ1 - Ann�e Acad�mique 2016-2017                 #
################################################################################
#                   L'Analyse en Composantes Principales - ACP                 #
#                       Principal Component Analysis - PCA                     #
################################################################################
# Remarque importante : les commandes R qui seront d�taill�es ci-dessous 
# sont (au choix):
# - � recopier manuellement dans l'interface R choisie (option pas si 
#   inint�ressante en termes d'apprentissage)
# - � copier puis � coller dans l'interface R,
# - � envoyer du fichier vers l'interface en utilisant la fonction ad hoc de 
#   l'outil choisi (RStudio, Tinn-R, ...)
################################################################################
# Le�ons du 19/04/2017                                                         #
################################################################################ 
# Partie 1 : Les bases de l'ACP                                                #
################################################################################ 
# Etape 1 : le dataset
##############################

# Pour cette introduction � l'ACP nous allons utiliser un mini fichier de donn�es
# Ce dataframe contient les mesures (longueur, largeur, hauteur) et le genre
# de 48 tortues.
# Plus d'infos (ex�cuter la commande dans R(-Studio) ou copier-coller l'URL dans 
# votre navigateur):

library(ade4)
data(tortues)
?tortues
names(tortues)
sexcol <- ifelse(tortues[,"sexe"] == "M", "blue", "red")

# Etape 2 : D�couverte du fichier (facultatif mais informatif)
##############################################################


# Plot d'une scatterplot matrix des donn�es:
mesures <- tortues[ , 1:3]
pairs(mesures, col=sexcol, pch=19)

#Affichons les matrices de corr�lation:
cor(mesures)

###############################################################################
# La partie suivante ne fait pas partie de l'ACP mais elle est destin�e �
# faire comprendre le but ultime de cette derni�re : la projection dans le 
# plan de dispersion maximale (si il existe).
# Les commandes utilis�es ne sont pas � comprendre, mais simplement � utiliser.
###############################################################################
# Utilisation du package rgl permettant de faire de  la visualisation 
# 3D interactive.
# T�l�chargement et installation du package (cette commande n'est � ex�cuter
# qu'une seule fois pour un ordinateur donn�):
#install.packages("rgl") 
# Chargement de package dans le syst�me:
library(rgl)

# Graphique 3D des 3 premi�res variables des donn�es: 
#plot3d(mesures, type = "s", col = sexcol)
#plot3d(scale(mesures, center = TRUE, scale = TRUE), type = "s", col = sexcol)
plot3d(mesures, type = "p",size= 10, col = sexcol)
plot3d(scale(mesures, center = TRUE, scale = TRUE), type = "p",size= 10, col = sexcol)

# "Enveloppe" des donn�es:
plot3d(ellipse3d(cor(mesures),scale=diag(cor(mesures)), col = "grey", alpha = 0.5), add=TRUE)
# Visualisation des 3 premiers axes originaux 
lines3d(rbind(c(0,0,0),c(2,0,0)),lwd=4, col="brown")
lines3d(rbind(c(0,0,0),c(0,2,0)),lwd=4, col="blue")
lines3d(rbind(c(0,0,0),c(0,0,2)),lwd=4, col= "green")
# "Jouez" avec le graphe jusqu'� visualiser les donn�es dans leur dispersion 
# maximum :)
# Ce plan de dispersion maximale est celui que cherche num�riquement l'ACP
# et ce ind�pendemment du nombre de variables initiales (la visu 3D n'�tant
# pas toujours possible).
#
# Retour � l'ACP 
###############################################################################
mesures.CR<-scale(mesures, center = TRUE, scale = TRUE)

# La dispersion (�cart-type et donc variance) est la caract�ristique qui 
# permet de distinguer les donn�es les unes des autres: sans dispersion
# toutes les donn�es sont indistinguables.
# La dispersion est donc utilis�e comme mesure de l'information contenue
# dans l'ensemble de donn�es. Cette �valuation de la dispersion se fait 
# en calculant la matrice de variance-covariance:
Covar<-cov(mesures.CR)

# L'affichage est facultatif car la covariance est moins directement 
# interpr�table que la corr�lation.
# Les covariance donnent une mesure de la fa�on dont les variables
# varient les unes par rapport aux autres: comment elles covarient.
# Les cor�lations sont calcul�es � partir des covariances et variances.

# Les variances sont sur la diagonale de la matrice de variance-covariance:
# La diagonale d'une matrice est directement accessible via la fonction diag():
diag(Covar)

# La variance totale est la somme des variances des diff�rentes variables
# originelles. Cette variance totale (aussi appel�e inertie) servira de
# mesure de r�f�rence pour la mesure de la perte d'information durant l'ACP.
# Cette variance totale se calcule simplement en faisant la somme (fonction 
# sum()) de la diagonale de la matrice de variance-covariance :
total.var<-sum(diag(Covar))
# La somme des �l�ments de la diagonale d'une matrice carr�e est aussi 
# appel�e la trace de la matrice.

# Etape 2 : l'ACP "manuelle"
############################
# Comme expliqu� dans les slides, l'ACP cherche les directions du 
# nuage de donn�es ayant le maximum de dispersion.
# Cette recherche a comme solution les vecteurs propres de la matrice 
# de variance-covariance:
# - les vecteurs propres donnent les directions cherch�es,
# - chaque vecteur propre est une nouvelle variable construite comme
#   combinaison lin�aire des anciennes variables (appel�es composantes),
# - ces vecteurs propres sont donn�s sous forme d'une matrice,
# - les colonnes de cette matrice donnnent les coordonn�es (projections)
#   de ces nouvelles variables dans le syst�me des variables originelles,
# - les valeurs propres donnent les variances dans les directions trouv�es,
# - ces valeurs propres sont class�es dans l'ordre d�croissant.

# Calcul des valeurs propres de la matrice de covariance
my.eig<-eigen(Covar)
# Les valeurs propres
my.eig$values
# Les vecteurs propres
my.eig$vectors

# Une caract�ristique des vecteurs propres et valeurs propres est que 
# la matrice diagonale constitu�e des valeur propres repr�sente la 
# m�me "information" (ici la variance-covariance des donn�es) que la
# matrice originale, MAIS dans le nouveau syst�me de  variables.
# De plus les traces des deux matrices sont �gales. Cette information 
# reste donc inchang�e par le changement de coordonn�es.
# On peut donc mesurer l'information (variance) concentr�e sur chacune 
# de ces nouvelles variables � partir de la variance totale calcul�e plus
# haut:
my.eig$values/total.var

# Comme les nouvelles variables (composantes, vect. pr.) sont class�es
# par ordre d�croissant de variances (valeur pr.) on peut calculer
# la quantit� d'information contenue de fa�on cumulative dans les 
# x premi�res composantes : 
cumsum(my.eig$values/total.var)
# La fonction cumsum() fait une somme cumulative.
# Dans notre cas la somme cumulative se termine par 1: dans le cas
# des tortues les 3 premi�res composantes contiennent 100% de la variance totale
# (ce qui est une lapalissade :).

# On constate que la premi�re composante contient 92% de la variance,
# et que les 2 premi�res composantes contiennent pr�s de 98% de la variance
# totale. En se concentrant sur ces deux composantes (et  en ignorant 
# les autres, les deux derni�res dans le cas pr�sent) on ne perd que 2.3%
# d'information. Les composantes retenues sont appel�es les composantes 
# principales (d'o� ACP).

# Le calcul  des vecteurs propres (composantes) et des valeurs propres 
# (variances des composantes) correspondantes ne sont pas les seules choses 
# � faire pour une ACP, il faut aussi:
# (1) calculer les coordonn�es (projections) des individus dans les nouvelles
#   variables,
# (2) calculer les projections des variables originelles dans les composantes
#   (notamment pour des questions d'interpr�tations des composantes, cf. infra).

#Proc�dons:
# (1) calculer les coordonn�es (projections) des individus dans les nouvelles
#   variables,

Q<-my.eig$vectors # matrice de transformation
Qinv<-solve(Q) # Inverse de Q
yinMypc<-t(Qinv%*% t(mesures.CR)) # On passe via des transpos�es car en Alg�bre
# les coordonn�es sont des vecteurs colonnes
# alors que nos donn�es sont habituellement 
# stock�es de telle fa�on que les valeurs
# (coordonn�es dans l'espace originel) sont 
# stock�es sous forme de lignes.

plot(yinMypc[,1:2],col=sexcol) # On plot les 2 premi�res composantes des tortues
abline(v=0,h=0) # on ajoutes les nouveaux axes

# (2) calculer les projections des variables originelles dans les composantes
#   (notamment pour des questions d'interpr�tations des composantes, cf. infra).

#   Calcul des coordonn�es des vecteurs de la base canonique (aka les directions 
# de nos variables originales)  dans la nouvelle base de composantes:
pc<-t(Qinv)
# On tace ces vecteurs sur le graphique
arrows(0,0, pc[1,1], pc[1,2], col="red")
arrows(0,0, pc[2,1], pc[2,2], col="red")
arrows(0,0, pc[3,1], pc[3,2], col="red")

# calcul des contributions de chaque variable originale dans la composition
# de chaque composante:
#
#  1) Si les composantes (les vecteurs propres) sont norm�es:
#   ^  |       .
#   |  |   1 .
# y |  |   .          x^2+y^2= 1 ==> x^2 et y^2 repr�sentent les contributions 
#   v  | .                         
#       --------
#       <------>
#          x
# 
#  2) Si les composantes ne sont pas norm�es, il suffit de diviser les carr�s
#     x^2 et y^2 par le carr� de la norme de la composante.
#     

# Contributions des variables orginales :
pc^2
# En %
pc^2 *100
# En 1000 �mes
pc^2*10000
# En 1000 �mes arrondis (cf. infra)
round(pc^2*10000)


# Nous allons donc maintenant faire appel aux outils de base de R pour faire l'ACP:
# la fonction princomp()

# Etape 3 : l'ACP de base dans R: princomp()
############################################

# PCA de base en R:
pca.r <- princomp(mesures.CR)
# On stocke le r�sultat dans une variable car princomp calcule
# plusieurs choses (cf. ci-dessus).
# Pour avoir un r�sum� de l'ACP:
summary(pca.r)

# Attention la premi�re ligne de la commande ci-dessus donne, non pas 
# les variances (!) mais bien les �cart-types, c�d les racines carr�es
# des premi�res.
# La commande ci-dessus est l'�quivalent des trois commandes suivantes:
# sqrt(my.eig$values)
# my.eig$values/sum(my.eig$values)
# cumsum(my.eig$values/sum(my.eig$values))

# Comme un petit dessin vaut mieux qu'un long discours on donne souvent 
# un barplot des valeurs propres. Ce graphe est appel� screeplot ou encore 
# graphe des �boulis:
plot(pca.r) # shows a screeplot.
# La d�croissance (forte ou non) valeur propres successives donne un information
# sur le nombre de ocmposantes qui concentrent l'information.

# Notons que le screeplot peut aussi �tre fait maunellement:
# barplot(my.eig$values,ylab="Variances of Components")

# La fonction loading() permet de conna�tre les coordonn�es des composantes
# dans les variables originales, et donc le poids de chacune des variables
# originales dans le composition des nouvelles variables (les composantes):

loadings(pca.r)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary

# C'est l'information que nous avions en affichant les vecteurs propres : 
#my.eig$vectors

# Enfin la commande biplot() permet enfin de dessiner les projections des 
# donn�es sur les 2 premi�res composantes ainsi que les projections sur ces
# deux composantes des variables originales:
biplot(pca.r)

# En r�sum� une ACP avec les outils de base de R se r�sume, pour les donn�es
# tortues tient donc dans les commandes suivantes:

pairs(mesures) # si opportun par rapport au nombre de variables
cor(mesures) # informatif
pca.r <- princomp(mesures) # ACP
summary(pca.r) # valeurs propres et % d'infos dans les composantes
plot(pc.cr)  # idem mais visuellement
loadings(pca.r) # poids des variables originelles dans les composantes
biplot(pca.r) # projection "finale"

# N�anmoins cette derni�re projection n'est pas toujours compl�tement lisible
# c'est pourquoi un certain nombres de packages ont d�velopp� leurs propres
# outils d'ACP. 


# Etape 4 : l'ACP en d�tail avec un l'aide de packages externes
###############################################################

# Utilisation du package ade4 permettant de faire de  l'ACP de fa�on plus 
# d�taill�e qu'en utilisant la fonction princomp de base.

# T�l�chargement et installation du package (cette commande n'est � ex�cuter
# qu'une seule fois pour un ordinateur donn�):
#install.packages("ade4") 
# Chargement de package dans la session R:
library(ade4)

# Ex�cution de l'ACP:
# - la fonction peut demander  interactivement le nombre d'axes � retenir,
#   et pour permettre ce choix elle affiche directement le screeplot:
#acp.ade4<-dudi.pca(mesures,center = TRUE, scale = TRUE )


# Si on pr�f�re ne pas avoir d'interruption interactive qui demande le nombre
# d'axes, alors on  passe scannf � FALSE et le nombre d'axes � retenir via nf:
acp.ade4<-dudi.pca(mesures, scannf=FALSE, nf=3,center = TRUE, scale = TRUE )

# Dans les deux cas il est conseill� de centrer(center=TRUE)-r�duire(scale=TURE)
# les donn�es :
# - si on ne centre pas, la premi�re composante sera la direction qui va 
#   de l'origine (point o� toutes les variables originales sont nulles)au 
#   centre du nuage.
# - la r�duction permet d'�viter les effets des diff�rences importantes d'�chelles 
#   entre les variables.

# Impression des valeurs propres
acp.ade4$eig
# Les variances cumul�es
cumsum(acp.ade4$eig)
# Les variances en pourcentages:
acp.ade4$eig/sum(acp.ade4$eig)*100
# Le screeplot:
barplot(acp.ade4$eig/sum(acp.ade4$eig)*100)
# Les pourcentages cumul�s :
cumsum(acp.ade4$eig/sum(acp.ade4$eig)*100)


# Le probl�me de l'ACP �tant l'interpr�tation des nouveaux axes (les 
# composantes) puisqu'ils sont form�s par combinaisons lin�aires
# des anciennes variables. C�d que les composantes sont un mix
# des variables initiales.
# Une premi�re fa�on de "comprendre" ces composantes est de regarder les 
# vecteurs propres qui contiennent les coefficients des combinaisons 
# lin�aires �voqu�es ci-dessus:
acp.ade4$c1

# On peut aussi avoir une id�e de la d�compostion l'inertie (la part de la variance totale 
# expliqu�e) entre les variables et composantes (en 10000 �mes):
inertia.dudi(acp.ade4,col.inertia = T)$col.abs


# Le package ade4 fournit aussi d'autres outils.
# Plot des "droites" de corr�lation des variables avec les deux premi�res 
# composantes: 
score(acp.ade4, xax=1)
score(acp.ade4, xax=2)
# Ces graphiques permettent de voir les liaisons entre les composantes et les variables.



# On peut tracer les cercles de correlation o� la longueur des fl�ches 
# indique la part de leur information repr�sent�e par les deux axes. 
# L'angle entre deux  fl�ches repr�sente la corr�lation qui les lie : 
# - angle aigu = positive;
# - angle droit = nulle;
# - angle obtus = n�gative.
s.corcircle(acp.ade4$co)

# Enfin on peut passer aux projections des donn�es dans les nouveaux axes.
# Repr�sentation � la fois les individus et des variables dans le premier plan
# factoriel (deux premi�res composantes):
scatter(acp.ade4)
# Idem sans l'histogramme des valeurs propres
scatter(acp.ade4, posieig="none")
# Idem mais sans �tiquettes, les individus �tant repr�sent� par des points
scatter(acp.ade4, posieig="none", clab.row=0)
# Comparez cette visualisation avec la visualisation 3D du debut...

# Ajout des groupes au graphe existant
s.class(dfxy=acp.ade4$li, fac=tortues[,"sexe"], add.plot=TRUE)

# Nouveau graphe
scatter(acp.ade4, posieig="none", clab.row=0)
# Groupes en couleur
s.class(dfxy=acp.ade4$li,fac=tortues[,"sexe"], add.plot=TRUE,col=c("red","blue","green"))

# On termine avec un nouveau layout comme ci-dessus pour reprendre les 
# diff�rents graphiques en un seulplot.
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
barplot(acp.ade4$eig[1:acp.ade4$rank])
s.corcircle(acp.ade4$co)
s.label(acp.ade4$li)
#scatter(acp, posieig="none", clab.row=0)
s.class(dfxy=acp.ade4$li,fac=tortues[,"sexe"], add.plot=FALSE,col=sexcol)
layout(matrix(1:1, 1, 1, byrow = TRUE))

# On peut aussi ajouter les coordonn�es des donn�es dans les nouveaux axes pour faciliter
# des repr�sentations ult�rieures.
# Ajout des composantes au data frame initial:
tortuesPC<-cbind(tortues,acp.ade4$li)


# D'autres fonctions d'autres packages existent, on en trouvera une liste
# (certainement non-exhaustive) � la page suivante :
browseURL("http://gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/")



# Exercices:
# 1) Effectuez l'ACP et l'analyse de ses r�sultats pour les donn�es swiss:
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

# 2) Effectuez l'ACP et l'analyse de ses r�sultats pour les donn�es USArrest des TP pr�c�dents 
#    (pour les 4 premi�re variables seulement).

# 3) Utilisez RStudio pour cr�er un rapport explicitant une des deux ACP ci-dessus (au choix), 
#    en cr�ant un nouveau document Markdown (option Word).


# Pour terminer �ventuellement :
# L'ACP avec Rcommander
