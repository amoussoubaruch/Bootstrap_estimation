###########################################################################
# Boostrap & Regression
##########################################################################

###########################################################################
###########################################################################
# Partie 1  : Bootstrap par paire
###########################################################################
###########################################################################

# On utilise ici les données car de R
data(cars)
attach(car)

## Regression linéaire et représntation de la droite estimé
plot(cars)
reg=lm(dist~speed,data=cars)
abline(reg,col="red")
n=nrow(cars)
x=21
points(x,predict(reg,newdata= data.frame(speed=x)),pch=19,col="blue")

## Intervalle de confiance de la prédiction et représentation onbtenu par bootstrap
Yx=rep(NA,500)
B=matrix(NA,500,2)
for(s in 1:500){indice=sample(1:n,size=n,
                              replace=TRUE)
base=cars[indice,]
#points(base,pch=3)
reg=lm(dist~speed,data=base)
abline(reg,col="light blue")
points(x,predict(reg,newdata=data.frame(speed=x)),pch=19,col="blue")
Yx[s]=predict(reg,newdata=data.frame(speed=x))
B[s,]=coefficients(reg)
}
# Représntatio intervalle de confiance
U=predict(reg,interval ="confidence",
             level=.9,newdata=
               data.frame(speed=0:30))
lines(0:30,U[,2],col="red",lwd=2)
lines(0:30,U[,3],col="red",lwd=2)



###########################################################################
      # Estimation bootstrap des coeficients du modèle linéiare #
############################################################################
# Considrons le jeu de données Duncan
library(car)
library(MASS)

data(Duncan)
attach(Duncan)

# Regression lineaire : prestige en fonction du revenu et l'education
mod.duncan.hub <- rlm(prestige ~ income + education, data=Duncan)
summary(mod.duncan.hub)

# Fonction pour le calcul de coef
boot.coef <- function(data, indices, maxit=20){
  data <- data[indices,] # select obs. in bootstrap sample
  mod <- rlm(prestige ~ income + education, data=data)
  coefficients(mod) # return coefficient vector
}



# Utilisation de la library boot de R pour faire le bootstrap 
library(boot)
# Soit B le nombre d'échantillons bootstrap 
system.time(reg.coef <- boot(Duncan, boot.coef, 1000))
# Statistiques de coefficients
reg.coef

# 
duncan.array <- boot.array(reg.coef)
duncan.array

plot(reg.coef, index=2)    # income coef
plot(reg.coef, index=3) # education coef

# Exemple d'ellispe
data.ellipse(reg.coef$t[,2], reg.coef$t[,3],
              xlab="income coefficient", ylab="education coefficient",
              cex=.3, levels=c(.5, .95, .99), robust=T)

###########################################################################
###########################################################################
        # Partie 2  : Bootstrap sur résidus
###########################################################################
###########################################################################

# Considérons toujours le jeu de données duncan

y <- Duncan$prestige 
x1 <- Duncan$income 
x2 <- Duncan$education

# Regression lineaire
reg <- lm(y ~ x1 + x2)
# Affichage des coeff
beta0<-reg$coef[1]  # intercept : -6.064663
beta1<-reg$coef[2]  # 0.5987328
beta2<-reg$coef[3]  # 0.5458339 

# On peut calculer les résidus obtenus
res <- Duncan$prestige -beta0-beta1*Duncan$income-beta2*Duncan$education

# On échatillonne ensuite les résidus
# Fonction pour le calcul la variance des résidus
erreurboot<-function(dose,X,B){
  val<-NULL
  for(i in(1 :B)){
    indiceboot<-sample(1 :length(X),length(X),replace=T)
    resboot<-X[indiceboot]
    ychapo<-beta0+beta1*Duncan$income+beta2*Duncan$education
    reg<-lm(ychapo ~x1+x2)
    beta0etoil<-reg$coef[1]
    beta1etoil<-reg$coef[2]
    beta2etoil<-reg$coef[3]
    yetoil<-beta0etoil+beta1etoil*dose+beta2etoil*dose^2
    val<-rbind(val,yetoil) }
  sd(val) }




