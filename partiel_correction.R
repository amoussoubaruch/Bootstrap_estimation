###########################################################################
# Partiel
# Sujet : Regression logistique, bootstrap, Selection de variables
##########################################################################

###################
# Partie 1
###################
# Lecture des donn�es

prost <- read.table("http://www.math.univ-toulouse.fr/~besse/Wikistat/data/prostate_r.dat", 
           header=TRUE,sep="")


# Ligne colonne
dim(prost)

# Structure des donn�es
str(prost)

# nombre de valeur manquante
sum(is.na(prost))

# Quelques stats descr de la table
summary(prost)

# Distribution des variables 
hist(prost$acid,probability=TRUE,col='blue')
lines(density(prost$acid), col="red", lwd=2)

# Et pour les autres variables pareil


###################
# Partie 2
###################

# Propri�t�s asymptotiques : loi normale

# 2.a
# Fonction pour l'estimation bootstrap
mediane_boost <- function(B,vecteur) {
  mediane = numeric(B)
  for (i in 1:B) {
    echant = sample(vecteur,replace=TRUE)
    mediane[i] = median(echant)
  }
  return (mediane)
}

B<- 1000
vercteur_bootsrap <- mediane_boost(B, prost$acid)

# 2.b 
summary(vercteur_bootsrap)
var(vercteur_bootsrap)

#2.c
a<-0.05
IC_born_inf <-vercteur_bootsrap[floor(B*(a/2))]
IC_born_sup <-vercteur_bootsrap[ceiling(B*(1-a/2))]
 
#2.d
# moyenne(m�dianes bootstrap) -m�diane(y) 



