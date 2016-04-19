###########################################################################
# SVM # Parallélisation
###########################################################################


pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
### PREPARE FOR THE DATA ###
df1 <- read.csv("credit_count.txt")
df2 <- df1[df1$CARDHLDR == 1, ]
x <- paste("AGE + ACADMOS + ADEPCNT + MAJORDRG + MINORDRG + OWNRENT + INCOME + SELFEMPL + INCPER + EXP_INC")
fml <- as.formula(paste("as.factor(DEFAULT) ~ ", x))
### SPLIT DATA INTO K FOLDS ###
set.seed(2016)
df2$fold <- caret::createFolds(1:nrow(df2), k = 4, list = FALSE)
### PARAMETER LIST ###
cost <- c(10, 100)
gamma <- c(1, 2)
parms <- expand.grid(cost = cost, gamma = gamma)
### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(df2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    deve <- df2[df2$fold != j, ]
    test <- df2[df2$fold == j, ]
    mdl <- e1071::svm(fml, data = deve, type = "C-classification", kernel = "radial", cost = c, gamma = g, probability = TRUE)
    pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$DEFAULT, prob = attributes(pred)$probabilities[, 2])
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
}