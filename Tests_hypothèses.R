###########################################################################
          # Boostrap & Tests d'hypothèses # 
###########################################################################

# Considérons un vecteur de données représentant des extraits d'un test
#       sur la vitesse de la lumière

speed <- c(28, -44, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24,40 , 21, 31, 34, -2, 25, 19)
length(speed)
# Un histogramme de ce vecteur

hist(speed)

# Reprensentation 
qqnorm(speed)
qqline(speed)

##########################################################################
# Test d'hypothèse sur la moyenne
#############################################################################
mean(speed) # 21.75

# Satistic du test : t-statistic
(mean(speed) - 33.02)/(sd(speed)/sqrt(20)) #  -2.859247

# Comme le t-stat est sensible au valeur extrêmes, nous supprimons de 
#       vecteur speed les deux valeurs extrêmes -2 et -44
# Nous le renommons
betterspeed <- speed[-c(2,18)]
betterspeed
# QQplot
qqnorm(betterspeed)

# Création d'un nouvelle échantillon centré en 33.02
newspeed <- betterspeed - mean(betterspeed) + 33.02
mean(betterspeed)

# Distribution
hist(betterspeed)

# Bootstarp
bstrap <- c()
for (i in 1:1000){bstrap <- c(bstrap, mean(sample(newspeed,20,replace=T)))}

#Now you'll see that the observed value of our test statistic is
#26.7222.
#Our pvalue is now the probability of seeing something more than
#(33.02 - 26.722) = 6.298 units away from 33.02.

# P value
(sum(bstrap < 26.722) + sum(bstrap > 39.218))/1000

t.test(betterspeed,alternative="two.sided",mu=33.02)

# Nous pouvons aussi choisis comme stat de test la moyenne
newspeed <- speed - mean(speed) + 33.02
mean(newspeed)

bstrap <- c()
for (i in 1:1000){
   newsample <- sample(newspeed, 20, replace=T)
   bstrap <- c(bstrap, mean(newsample))}
hist(bstrap)
# P value
(sum(bstrap < 21.75) + sum(bstrap > 44.29))/1000



