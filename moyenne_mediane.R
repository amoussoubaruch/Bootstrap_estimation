###########################################################################
# Boostrap & MonteCarlo
# L'objectif de ce code est 
##########################################################################

# Nous travaillons avec le data set sleep
data(sleep)
str(sleep) # Deux variables : group, extra

attach(sleep)
## Moyenne de la var extra, ecart type de la var extra, taille par groupe
tapply(extra, group, mean)
tapply(extra, group, sd)
tapply(extra, group, length)

## Distribution de la variable extra
hist(sleep$extra,probability=TRUE,col='blue')
lines(density(sleep$extra), col="red", lwd=2)

# Conculsion : La distribution de la variable n'est pas normale

################################
#Estimation de la moyenne
#########################
# Les estimateurs de ses paramaètres sont ils robustes??
#Le bostrap doit fournir des extimateurs plus robustes
## Moyenne par group
# Group1
group1 <- sleep$extra[sleep$group==1]

# Un estimateur de la moyenne
mean(group1)
sd(group1)
median(group1)
#générer plusieurs échalion bostrap

moy_boost <- function(R,vecteur) {
  moyenne = numeric(R)
  for (i in 1:R) {
    echant = sample(vecteur,replace=TRUE)
    moyenne[i] = mean(echant)
  }
  return (moyenne)
}
vec <- sleep[sleep$group==1,1]
T1 <- Sys.time()
v <- NULL
for (i in seq(10,100000,1000)){
  v <- append(v, system.time(moy_boost(i, vec))[3])
}
v

plot(v, pch=15, main="Temps de calcul")

moyenne = moy_boost(100000, vec)
hist(moyenne,probability=TRUE,col='blue')
lines(density(moyenne), col="red", lwd=2)


################################
#Estimation de la médiane
#########################

# Les estimateurs de ses paramaètres sont ils robustes??
#Le bostrap doit fournir des extimateurs plus robustes
mediane_boost <- function(R,vecteur) {
  mediane = numeric(R)
  for (i in 1:R) {
    echant = sample(vecteur,replace=TRUE)
    mediane[i] = median(echant)
  }
  return (mediane)
}
vec <- sleep[sleep$group==1,1]
v <- NULL
for (i in seq(100,10000,1)){
  mediane <- mediane_boost(i, vec)
  ## erreur standard
  #error = sqrt(sum((mediane - mean(mediane))**2)/(length(mediane)-1))
  v <- append(v, sqrt(sum((mediane - mean(mediane))**2)/(length(mediane)-1)))
}
v
plot(v, pch=15, main="Erreur Standart")


mediane = mediane_boost(1000, vec)
hist(mediane,probability=TRUE,col='blue')
lines(density(mediane), col="red", lwd=2)



################################
#Estimation de la l'écart type
#########################

# Les estimateurs de ses paramaètres sont ils robustes??
#Le bostrap doit fournir des extimateurs plus robustes
std_boost <- function(R,vecteur) {
  std = numeric(R)
  for (i in 1:R) {
    echant = sample(vecteur,replace=TRUE)
    std[i] = sd(echant)
  }
  return (std)
}
vec <- sleep[sleep$group==1,1]
v <- NULL
for (i in seq(10,100000,1000)){
  std <- std_boost(i, vec)
  ## erreur standard
  #error = sqrt(sum((mediane - mean(mediane))**2)/(length(mediane)-1))
  v <- append(v, sqrt(sum((std - mean(std))**2)/(length(std)-1)))
}
v
plot(v, pch=15, main="Erreur Standart")


std = std_boost(100, vec)
hist(std,probability=TRUE,col='blue')
lines(density(std), col="red", lwd=2)


################################################
# Animation
#############################################
library(animation)
R = 1000
mediane = numeric(R)
for (i in 1:R) {
  #group2 = rnorm(10, mean=2.33, sd=1.9)
  mediane[i] = median(sample(sleep[sleep$group==1,1],replace=TRUE))
  #  abline(v=mediane[i])
}

#dev.new(noRStudioGD = FALSE)
saveGIF({
  ani.options(loop = 1)
  #  ani.options(nmax = 50, interval = 0.05)
  plot(density(sleep[sleep$group==1,1]))
  idx = seq(5,length(mediane),25)
  for (i in seq_along(idx)) {
    plot(density(mediane[1:idx[i]]))
    points(mean(mediane[1:idx[i]]<0.05),0, pch=16)
    ani.pause()  ## pause for a while ('interval')
  }
}, movie.name = "alpha.gif",
outdir = "~",ani.width = 600, ani.height = 600)

############ Test

saveGIF(for(i in 1:10) plot(runif(10), ylim = 0:1),
        movie.name = "~/POC Group One Point/Life_Science/animation.gif", 
        img.name = "Rplot", convert = "C:\\Program Files\\ImageMagick-6.9.3-Q16\\convert.exe", 
        cmd.fun, clean = TRUE)

ani.options(loop=1)
ani.options(convert='C:\\Program Files\\ImageMagick-6.9.0-Q16\\convert.exe')
saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
}
#movie.name = "~/POC Group One Point/Life_Science/animation.gif"
)

saveGIF({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30, ani.width = 600, 
ani.height = 600)




#### Test boostrap

traite <- c(94,197,16,38,99,141,23)
controle <- c(52,104,146,10,51,30,40,27,46)

mean(traite)
mean(controle)

sd(traite)
sd(controle)

moy_boost <- function(R,vecteur) {
  moyenne = numeric(R)
  for (i in 1:R) {
    echant = sample(vecteur,replace=TRUE)
    moyenne[i] = mean(echant)
  }
  return (moyenne)
}

distri1 <- moy_boost(50,traite)
length(distri1)
sqrt(sum((traite - mean(traite))**2)/(length(traite)-1))
for (i in 1:10) {
  print (i)
}

### Coeficinet de correlation
dat<-data.frame(wt,sbp)
boot.cor<-function(dat,i)
{z<-cor(dat[i,1],dat[i,2])
z}
boot.obj4<-boot(data=dat,statistic=boot.cor,R=1000)
boot.obj4
