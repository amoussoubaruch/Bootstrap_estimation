###########################################################################
# Boostrap & Parallélisation 
##########################################################################

## Code 1
# Chargement des données depuis le site internet
bank <- scan("http://pages.stat.wisc.edu/~gvludwig/327-5/bank.txt")
# Représentation de la densité
plot(density(bank), main = "Phone call durations")

# Représntation loi gamma
require(stats4, quiet=TRUE)
plot(density(bank), main = "Phone call durations")
dens <- function(x, param=res){
  z <- coef(res)
  return(dgamma(x, rate=z[["lambda"]], 
                shape=z[["alpha"]]))
}
log.lik <- function(alpha, lambda) {
  if(alpha > 0 & lambda > 0){
    ret <- -sum(dgamma(bank, shape=alpha, rate=lambda, log=TRUE))
  } else {
    ret <- NA
  }
  return(ret)
}
a.s <- (mean(bank)^2)/var(bank)
l.s <- mean(bank)/var(bank)
res <- mle(log.lik, method="L-BFGS-B", lower=c(0.00001,0.00001), start=list(alpha=a.s, lambda=l.s))
curve(dens, min(bank), max(bank), add=TRUE, col="Red")

########### Bootstrap en sequentiel
# Packages
require(doRNG) # To set seeds
require(foreach)
require(stats4) # For mle(), wrapper of optim()

# Setting parallelization parameters
registerDoMC(8)

# Declare functions first
log.lik <- function(alpha, lambda) {
  if(alpha > 0 & lambda > 0){
    ret <- -sum(dgamma(bank, shape=alpha, rate=lambda, log=TRUE))
  } else {
    ret <- NA
  }
  return(ret)
}

# Original data in capital letters
BANK <- scan("http://pages.stat.wisc.edu/~gvludwig/327-5/bank.txt")

# Bootstrap parameters
n <- length(BANK)
B <- 500 # As many as you can afford!
# set.seed(1) # Need doRNG package
registerDoRNG(1, once=FALSE)

# Bootstrap implementation
results <- foreach(i = 1:B, .combine = 'rbind') %dopar% {
  bank <- sample(BANK, size=n, replace=TRUE)
  a.s <- (mean(bank)^2)/var(bank)
  l.s <- mean(bank)/var(bank)
  res <- mle(log.lik, method="L-BFGS-B", lower=c(0.00001,0.00001), 
             start=list(alpha=a.s, lambda=l.s))
  coef(res) # Last item is 
}
results
write.csv(results, "results_par.csv")

test.par <- read.csv("results_par.csv", row.names=1)
str(test.par)

plot(density(test.par$lambda), main = "Lambda")
plot(density(bank), main = "Phone call durations")
###########################################################################
## Code 2
# Un second code pour faire la parrallisation
###########################################################################
library(doMC)
library(foreach)
library(igraph)
# Jaccard coeficcient function (taken from package fpc)
clujaccard = function (c1, c2, zerobyzero = NA) {
  if (sum(c1) + sum(c2) - sum(c1 & c2) == 0)
    out = zerobyzero
  else
    out = sum(c1 & c2)/(sum(c1) + sum(c2) - sum(c1 & c2))
  return(out)
}
registerDoMC() # registers the parallel backend
B = 1000 # number of bootstrap replicates to create
load("igraph_network.Rdata") # load a previously saved network (network name: g)
fg = fastgreedy.community(g) # compute original clustering
mm = which.max(fg$modularity) # find level of max modularity
moc = community.to.membership(g, fg$merges, mm)$membership # get membership
noc = length(unique(moc)) # count the number original clusters
bg = g # make a copy of g for bootstrapping
clusters = foreach(i=seq(B), .combine=cbind) %dopar% {
  E(bg)$weight = sample(E(g)$weight, replace=TRUE) # resample the edge weights
  fg = fastgreedy.community(bg) # compute bootstrap clustering
  mm = which.max(fg$modularity) # find level of max modularity
  mbc = community.to.membership(bg, fg$merges, mm)$membership # get membership
  nbc = length(unique(mbc)) # count the number new clusters
  bootresult = c()
  for (j in seq(0, noc-1)) { # for each of the original clusters...
    maxgamma = 0
    if (nbc > 0) {
      for (k in seq(0, nbc-1)) { # for each of the new clusters...
        bv = as.vector(mbc == k)
        ov = as.vector(moc == j)
        jc = clujaccard(ov, bv, zerobyzero=0)
        if (jc > maxgamma) # if these two clusters are most similar...
          maxgamma = jc
      }
    }
    bootresult = c(bootresult, maxgamma) # combine results
  }
  return (bootresult) # return the results of this iteration (and cbind with the rest)
}
bootmean = apply(clusters, 1, mean) # mean Jaccard coefficient for each cluster
