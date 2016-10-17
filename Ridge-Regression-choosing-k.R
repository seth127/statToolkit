library(gdata)
library(car)
library(perturb)
setwd("~/Documents/DSI/notes/2-STAT-6021/homework")

gas <- read.xls("data-table-B3.xls")
gas <- gas[complete.cases(gas$x3),]

ridgeit <- function(df) {
  gasm <- as.matrix(df)
  x <- gasm[,2:ncol(gasm)]
  y <- gasm[,1]
  
  gas.ridge <- glmnet(x, y, alpha=0)
  
  ## Ridge coefficicent estimates
  #coef(gas.ridge)
  
  # pick optimal lambda with cross-validated test set MSE
  set.seed (1)
  cv.out = cv.glmnet(x, y, alpha=0, nfolds=nrow(x))
  plot(cv.out)
  bestlam=cv.out$lambda.min
  print(paste("Best Lambda:", bestlam))
  
  ## Create the ridge trace plot
  plot(gas.ridge,xvar="lambda",label=TRUE)
  abline(v=log(bestlam), lty=2)
  
  ## fit with best lambda
  best.ridge <- glmnet(x, y, alpha=0, lambda=bestlam)
  
  ## look at residual sum of squares
  ridge.preds <- predict(best.ridge ,s=bestlam ,newx=x)
  orig.preds <- predict(lm(y ~ x))
  
  ridge.SSres <- sum( (y - ridge.preds)^2 )
  orig.SSres <- sum( (y - orig.preds)^2 )
  
  print("SSres")
  print(paste("Ridge:", ridge.SSres))
  print(paste("Original:", orig.SSres))
  
  ## R^2
  
  ridge.R2 <- sum((ridge.preds - mean(y))^2) / sum((y - mean(y))^2)
  orig.R2 <- sum((orig.preds - mean(y))^2) / sum((y - mean(y))^2)
  
  print("R^2")
  print(paste("Ridge:", ridge.R2))
  print(paste("Original:", orig.R2))
  
  # check a held out test set MSE
  train=sample(1:nrow(gasm), nrow(gasm)*.75)
  
  trainx <- x[train, ]
  trainy <- y[train]
  
  testx <- x[-train, ]
  testy <- y[-train]
  
  tt.ridge <- glmnet(trainx, trainy, alpha=0, lambda=bestlam)
  tt.orig <- lm(y ~ ., data=gas[train,])
  
  # print test MSE
  MSE.ridge <- sum((testy - predict(tt.ridge,s=bestlam,newx=testx))^2) / nrow(testx)
  MSE.orig <- sum((testy - predict(tt.orig,newdata=gas[-train,]))^2) / nrow(testx)
  print("Test Set MSE")
  print(paste("Ridge:", MSE.ridge))
  print(paste("Original:", MSE.orig))
}

ridgeit(gas)