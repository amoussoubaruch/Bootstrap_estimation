###########################################################################{}
# Boostrap & MonteCarlo
# Estimation de paramètre: moyenne, écart type, Variance
#                          médiane,  coeficient de correlation
############################################################################

# Jeu de donnée mtcars
help(mtcars)
data(mtcars)
attach(mtcars)

# data structure
str(mtcars)

# Columns names
colnames(mtcars)

# Distribution disp
par(mfrow=c(1,1))
hist(mtcars$disp,probability=TRUE,col='blue')
lines(density(mtcars$disp), col="red", lwd=2)

# Statistique de la variable disp
mean(mtcars$disp) #  230.7219
median(mtcars$disp) #  196.3
var(mtcars$disp) #  15360.8
sd(mtcars$disp) #  123.9387
summary(mtcars$disp)

######################################################################
          # Estimation bootstrap de la moyenne #
######################################################################

# Fonction pour obtenir un vecteur bootstrap de taille B
moy_boost <- function(B,vecteur) {
  moyenne = numeric(B)
  for (i in 1:B) {
    echant = sample(vecteur,replace=TRUE)
    moyenne[i] = mean(echant)
  }
  return (moyenne)
}

Vect_boot_moy1 = moy_boost(100, mtcars$disp)
Vect_boot_moy2 = moy_boost(1000, mtcars$disp)
Vect_boot_moy3 = moy_boost(10000, mtcars$disp)
Vect_boot_moy4 = moy_boost(100000, mtcars$disp)

par(mfrow=c(2,2))
hist(Vect_boot_moy1,probability=TRUE,col='blue', main= 'B = 100')
lines(density(Vect_boot_moy1), col="red", lwd=2)
hist(Vect_boot_moy2,probability=TRUE,col='blue', main= 'B = 1000')
lines(density(Vect_boot_moy2), col="red", lwd=2)
hist(Vect_boot_moy3,probability=TRUE,col='blue', main= 'B = 10000')
lines(density(Vect_boot_moy3), col="red", lwd=2)
hist(Vect_boot_moy4,probability=TRUE,col='blue', main= 'B = 100000')
lines(density(Vect_boot_moy4), col="red", lwd=2)

# Error standard moy
vec <- mtcars$disp
v <- NULL
for (i in seq(100,10000,1)){
  moy <- moy_boost(i, vec)
  ## erreur standard
  #error = sqrt(sum((mediane - mean(mediane))**2)/(length(mediane)-1))
  v <- append(v, sqrt(sum((moy - mean(moy))**2)/(length(moy)-1)))
}
#Representation de l'erreur standard
plot(v, pch=15, xlab="Nombre d'échantillon bootstrap", ylab="Erreur estimation",main="Standard Error")

## Temps de calcul
vec <- mtcars$disp
v <- NULL
for (i in seq(100,10000,1)){
  v <- append(v, system.time(moy_boost(i, vec))[3])
}
plot(v, pch=15, main="Temps de calcul",xlab="Nombre d'échantillon bootstrap", ylab="Erreur estimation")


######################################################################
          # Estimation bootstrap de la médiane #
######################################################################
# Fonction pour obtenir un vecteur bootstrap de taille B
mediane_boost <- function(B,vecteur) {
  mediane = numeric(B)
  for (i in 1:B) {
    echant = sample(vecteur,replace=TRUE)
    mediane[i] = median(echant)
  }
  return (mediane)
}

Vect_boot_med1 = mediane_boost(100, mtcars$disp)
Vect_boot_med2 = mediane_boost(1000, mtcars$disp)
Vect_boot_med3 = mediane_boost(10000, mtcars$disp)
Vect_boot_med4 = mediane_boost(100000, mtcars$disp)

par(mfrow=c(2,2))
hist(Vect_boot_med1,probability=TRUE,col='blue', main= 'B = 100')
lines(density(Vect_boot_med1), col="red", lwd=2)
hist(Vect_boot_med2,probability=TRUE,col='blue', main= 'B = 1000')
lines(density(Vect_boot_med2), col="red", lwd=2)
hist(Vect_boot_med3,probability=TRUE,col='blue', main= 'B = 10000')
lines(density(Vect_boot_med3), col="red", lwd=2)
hist(Vect_boot_med4,probability=TRUE,col='blue', main= 'B = 100000')
lines(density(Vect_boot_med4), col="red", lwd=2)


# Error standard moy
vec <- mtcars$disp
v <- NULL
for (i in seq(100,10000,1)){
  med <- mediane_boost(i, vec)
  ## erreur standard
  #error = sqrt(sum((mediane - mean(mediane))**2)/(length(mediane)-1))
  v <- append(v, sqrt(sum((med - mean(med))**2)/(length(med)-1)))
}
#Representation de l'erreur standard
par(mfrow=c(1,1))
plot(v, pch=15, xlab="Nombre d'échantillon bootstrap", ylab="Erreur estimation",main="Standard Error")

## Temps de calcul
vec <- mtcars$disp
v <- NULL
for (i in seq(10,100000,1000)){
  v <- append(v, system.time(mediane_boost(i, vec))[3])
}
plot(v, pch=15, main="Temps de calcul",xlab="Nombre d'échantillon bootstrap", ylab="Time en min")


######################################################################
        # Estimation bootstrap intervalle de confiance #
######################################################################

# Méthode des percentiles
ICbootquantile<-function(alpha,B,Y){
  VectMoy<-rep(0,B)
  for(i in 1 :B){
    X<-Y[sample(1 :length(Y),length(Y),replace=T)]
    VectMoy[i]<-mean(X) }
  VectMoyCroiss<-sort(VectMoy)
  icinf<-VectMoyCroiss[B*(alpha/2)]
  icsup<-VectMoyCroiss[B*(1-alpha/2)]
  cbind(icinf,icsup) }

# Test avec la variable diap
ICbootquantile(0.5,1000,vec)

# Méthode bootstrap t
ICboott<-function(alpha,B,Y){
  moyenne<-mean(Y)
  ecart<-sd(Y)
  Zboot<-rep(0,B)
  for(i in 1 :B){
    indices<-sample(1 :length(Y),length(Y),replace=T)
    X<-Y[indices]
    Zboot[i]<-(mean(X)-moyenne)/(sd(X)/sqrt(length(Y))) }
  Zcroiss<-sort(Zboot)
  ICinf<-(moyenne-Zcroiss[B*(1-alpha/2)]*ecart/sqrt(length(Y)))
  ICsup<-(moyenne-Zcroiss[B*(alpha/2)]*ecart/sqrt(length(Y)))
  cbind(ICinf,ICsup) }

ICboott(0.5,1000,vec)

######################################################################
      # Estimation bootstrap coeficient de correlation #
######################################################################
# On calcule la correlation entre la variable disp & mpg
cor(mtcars$disp,mtcars$mpg)  # -0.8475514

# Estmation bootstrap 
bootcor<-function(B,G1,G2){
  xBootcor<-NULL
  for(i in 1 :B){
    indices<-sample(1 :length(G1),length(G1),replace=T)
    Xb<-G1[indices]
    Yb<-G2[indices]
    xBootcor<-rbind(xBootcor,cor(Xb,Yb)) }
  return (xBootcor)
}

# Exemple avec nos deux variables selectionnées
bootcor(1000,mtcars$disp,mtcars$mpg)

# Standard error 
erreurbootcor<-function(B,G1,G2){
  xBootcor<-NULL
  for(i in 1 :B){
    indices<-sample(1 :length(G1),length(G1),replace=T)
    Xb<-G1[indices]
    Yb<-G2[indices]
    xBootcor<-rbind(xBootcor,cor(Xb,Yb)) }
  sd(xBootcor) }

for(i in 1 :B){
erreurbootcor(100,mtcars$disp,mtcars$mpg)
}






