###########################################################################
# Boostrap & MonteCarlo
#Estimation bootstrap de la médiane, sa variance et son IC
##########################################################################

#" Soit le vecteur y
y<-c(12,9.5,5,7.9,15.2,7.0,7.6,8.2,7.1,13.4,6.9,11.3,6.3,11.9,5.8
    ,2.8,7.8,27.7,20.4,9.2,10.5,3.2,11.2,6.4,1.7,8.6,3.3,9.9,19.1,
    7.7,1.2,6.6,2.2,6.1,3.8,9.7,3.7,12.3,12.2,11.6,12.4,32.4,6.7,
    7.4,53.8,29.1,28.9) 

#Estimation de la médiane
median(y)
# Variance de y
var(y)
# Quelques stats 
summary(y)

# Fonction pour estimation de B stat par bootstrap
mediane_boost <- function(R,vecteur) {
  mediane = numeric(R)
  for (i in 1:R) {
    echant = sample(vecteur,replace=TRUE)
    mediane[i] = median(echant)
  }
  return (mediane)
}

# Pour notre vecteur y en choississant de faire 2000 échantillon bootstrap on obtient
B = 2000
vecteur_bootstrap <- mediane_boost(B,y)

# Stat du vecteur_bootstrap
summary(vecteur_bootstrap)

# Une variance de la médiane peut est approché par la variance de ce vecteur
var(vecteur_bootstrap)

# Onpeut estimer le biais de la médiane de l'échantillon 
# moyenne(médianes bootstrap) -médiane(y) 
mean(vecteur_bootstrap) -median(y) 

# Hitogramme de la distribution de vecteur_bootstrap
hist(vecteur_bootstrap,probability=TRUE,col='blue')
lines(density(vecteur_bootstrap), col="red", lwd=2)
# La figure représente une loi très proche de celle de la vraie loi de la médiane

# Intervalle de confiance par méthode des percentiles
# Pour un seuil de confiance a = 5% on obtient
a = 0.5
IC_borne_inf<-vecteur_bootstrap[floor(B*(a/2))]
IC_borne_sup<-vecteur_bootstrap[ceiling(B*(1-a/2))]
IC_borne_inf 
IC_borne_sup

##### Exercices
data(cars)
str(cars) # Deux variables : group, extra
attach(cars)
