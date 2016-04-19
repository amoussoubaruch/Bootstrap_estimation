###########################################################################
# Boostrap & MonteCarlo
#Estimation bootstrap de l'écart type de l'AUC et IC
##########################################################################
# On utilise dans ce exo le package boot pour réaliser le bootstarp
library(boot)

# On utilise les données coronary :pPatients atteints de maladies coronariennes

# Read txt file

data <- read.table("C:/Users/b.amoussou-djangban/Documents/POC Group One Point/MonteCarlo_Boostrap/Script_final/coronary.txt",
                 header=T,sep="")

attach(data) 
# Variable à prédire : Coron
# Variable explicative :age et poids
logistique_reg <-glm(coron~age+wt,family=binomial,weights = exposure,data=data)
logistique_reg1 <-glm(coron~age*wt,family=binomial,data=data)
summary(logistique_reg)
names(logistique_reg)
logistique_reg$coefficients
################################################################
# prédiction de la maladie coronarienne par P(coron|age, poids)
################################################################
logistique_reg$fitted.values
################################################################
#### fabrication de la matrice qui contiendra #######
#### en 1re colonne : la sensibilité ####### 
#### en 2ème colonne: 1- spécificité #######
################################################################
Fit<-fitted.values(logistique_reg)
mmin<-min(Fit)
mmax<-max(Fit)
mmin
# 0.02003988
mmax
# 0.4805969
rocc<-matrix(0,24,2)

for(ii in (1:24))
{ # sensibilité
  rocc[ii,1]<-sum(coron*(Fit > ii*.02))/sum(coron)
  # 1 - spécificité
  rocc[ii,2]<-1-sum((1- coron)*(Fit<=ii*.02))/sum(1-coron)
}
rocc
#########################################################
### On doit réordonner les abscisses
#########################################################

ord <- order(rocc[,2], rocc[,1])

ord # ord contient les indices
# des éléments du tableau ordonné
# selon les valeurs croissantes
# de la deuxième colonne, en séparant
# les ex-aequo grâce à la première colonne
rocc1 <- rocc[ord,]
rocc1
##########################################################
win.graph()
plot(rocc1[,2],rocc1[,1], type="l",lwd=4,
     xlab="1-Specificite",ylab="Sensibilite") 

title("COURBE ROC POUR LE MODELE LOGISTIQUE :
      coron ~ age + weight")
##########################################################
# Calcul de l'aire au-dessous de la courbe ROC
roca<-0
for(ii in 2:24)
{ delta <- rocc1[ii,2] - rocc1[ii-1,2]
roca <- roca + delta * ((rocc1[ii,1]+rocc1[ii-1,1])/2)}
# Aire au-dessous de la courbe :
roca
# 0.7390584
##########################################################
# rappelons que
# Fit<-fitted.values(coron.glm.AWT) (cf regression #logistique)
mmin <- min(Fit)
mmax <- max(Fit)
delta <- .01
# nn = nombre des points en lesquels
# la sensibilité et la spécificité
# seront calculées.
nn <- floor((mmax - mmin) / delta)
boot.frame <- data.frame(coron,Fit)
attach(boot.frame)
ROCarea <- function(coron,Fit)
{
  rocc<-matrix(0,nn,2)
  for(ii in (1:nn))
  {
    rocc[ii,1]<-sum(coron*
                      (Fit >ii*mmin))/sum(coron)
    # sensibilite
    rocc[ii,2]<-1-(sum
                   ((1-coron)*(Fit<=ii*mmin))/sum(1-coron))
    #1-specificite 
    
  }
  ord <- order(rocc[,2], rocc[,1])
  rocc1 <- rocc[ord,]
  # coron et Fit ordonnes
  roca<-0
  for(ii in 2:nn)
  {
    delta <- rocc1[ii,2] - rocc1[ii-1,2]
    roca <- roca + delta * ((rocc1[ii,1]+rocc1[ii-1])/2 )
  }
  c(roca)
  # roca = area under the ROC curve is
  # returned by the function ROCarea
}
# fin de la fonction ROCarea
#############################################################
boot.roc<-function(boot.frame,i)
{z<-ROCarea(boot.frame[i,1],boot.frame[i,2])
z}
boot.obj6 <- boot(data = boot.frame, statistic =
                    boot.roc, R=1000) 
summary(boot.obj6)
plot(boot.obj6) 

