###########################################################################
# Boostrap & Analyse factorielle
# Cas de l'acp
##########################################################################
# Nous travaillons avec les données de crimes
#importing the data file
crime.data <-read.csv("C:/Users/b.amoussou-djangban/Documents/POC Group One Point/MonteCarlo_Boostrap/Script_final/Data used/Crime_dataset .csv",header=T,sep=";")
#performing the pca with princomp
crime.pca <- princomp(crime.data,cor=T)
# Valuerpropre
eig.val <- crime.pca$sdev^2
print(eig.val)
biplot(crime.pca)

# Bootstrap estimation
#n : number of instance, p : number of variables
n <- nrow(crime.data)
p <- ncol(crime.data)
#generation of a dataset
gendata <- function(n,p){
  df <- list()
  for (k in 1:p){
    x <- rnorm(n)
    df[[k]] <- x
  }
  df <- data.frame(df)
  colnames(df) <- 1:p
  return(df)
}
#pca on gendata
pca.gendata <- function(n,p){
  data.gen <- gendata(n,p)
  pca <- princomp(data.gen,cor=T)
  eig <- pca$sd^2
  return(eig)
}
set.seed(1)
#repeating T times the analysis
T <- 1000
res <- replicate(T, pca.gendata(n,p))
#computing the mean of the eigenvalues
rnd.mean <- apply(res,1,mean)
print(rnd.mean)
#computing the 0.95 percentile
rnd.95 <- apply(res,1,quantile,probs=(0.95))
print(rnd.95)



