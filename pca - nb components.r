rm(list=ls())
#importing the data file
library(xlsx)
setwd("D:/DataMining/Databases_for_mining/dataset_for_soft_dev_and_comparison/fact_analysis/acp choix nb axes")
crime.data <- read.xlsx(file="crime_dataset_pca.xls",sheetIndex=1,header=T)
#performing the pca with princomp
crime.pca <- princomp(crime.data,cor=T)
eig.val <- crime.pca$sdev^2
print(eig.val)
biplot(crime.pca)

#*****************
#PARALLEL ANALYSIS
#*****************

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
#computing the proportion of values lower than eig
prop <- rep(0,length(eig.val))
names(prop) <- names(eig.val)
for (k in 1:length(eig.val)){
 prop[k] <- length(which(res[k,] > eig.val[k]))/T
}
print(prop)

#**************
# RANDOMIZATION
#**************
set.seed(1)
one.randomization <- function(dataset){
 dataset.rdz <- data.frame(lapply(dataset,function(x){sample(x,length(x),replace=F)}))
 pca.rdz <- princomp(dataset.rdz,cor=T)
 eig.rdz <- pca.rdz$sd^2
 return(eig.rdz)
}

#repeat the procedure
res.rdz <- replicate(T,one.randomization(crime.data))

#mean
rdz.mean <- apply(res.rdz,1,mean)
print(rdz.mean)
#quantile
rdz.95 <- apply(res.rdz,1,quantile,probs=(0.95))
print(rdz.95)


#**********
# BOOTSTRAP
#**********


#creating one replication of the dataset
one.replication <- function(dataset){
 n <- nrow(dataset)
 index <- sort(sample.int(n,replace=T))
 out.dataset <- dataset[index,]
 return(out.dataset)
}
#performing a pca on a replication of the dataset
pca.replication <- function(dataset){
 one.dataset <- one.replication(dataset)
 pca <- princomp(one.dataset,cor=T)
 eig <- pca$sd^2
 return(eig)
}

#bootstraping pca
res.boot <- replicate(T,pca.replication(crime.data))

#quantile 0.05
boot.05 <- apply(res.boot,1,quantile,probs=(0.05))
print(boot.05)

#quantile 0.95
boot.95 <- apply(res.boot,1,quantile,probs=(0.95))
print(boot.95)

#median
boot.50 <- apply(res.boot,1,quantile,probs=(0.50))
print(boot.50)
