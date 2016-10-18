library(gdata)
library(car)
library(glmnet)

## for testing purposes
#gas <- read.xls("data-table-B3.xls")
#gas <- gas[complete.cases(gas$x3),]

ridgeit <- function(df, seed = 1) { ### RESPONSE MUST BE NAMED 'y'
  #library(car)
  #library(glmnet)
  #dfm <- as.matrix(df)
  x <- model.matrix(y~., data=df)[,2:ncol(df)]
  y <- df$y
  
  df.ridge <- glmnet(x, y, alpha=0)
  
  ## Ridge coefficicent estimates
  #coef(df.ridge)
  
  # pick optimal lambda with cross-validated test set MSE
  set.seed (seed)
  cv.out = cv.glmnet(x, y, alpha=0, nfolds=nrow(x))
  plot(cv.out)
  bestlam=cv.out$lambda.min
  print(paste("Best Lambda:", bestlam))
  
  ## Create the ridge trace plot
  plot(df.ridge,xvar="lambda",label=TRUE)
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
  train=sample(1:nrow(df), nrow(df)*.75)
  
  trainx <- x[train, ]
  trainy <- y[train]
  
  testx <- x[-train, ]
  testy <- y[-train]
  
  tt.ridge <- glmnet(trainx, trainy, alpha=0, lambda=bestlam)
  tt.orig <- lm(y ~ ., data=df[train,])
  
  # print test MSE
  MSE.ridge <- sum((testy - predict(tt.ridge,s=bestlam,newx=testx))^2) / nrow(testx)
  MSE.orig <- sum((testy - predict(tt.orig,newdata=df[-train,]))^2) / nrow(testx)
  print("Test Set MSE")
  print(paste("Ridge:", MSE.ridge))
  print(paste("Original:", MSE.orig))
  
  #return the optimal model
  glmnet(x, y, alpha=0, lambda=bestlam)
}


lassoit <- function(df, seed = 1) { ### RESPONSE MUST BE NAMED 'y'
  #library(car)
  #library(glmnet)
  #dfm <- as.matrix(df)
  x <- model.matrix(y~., data=df)[,2:ncol(df)]
  y <- df$y
  
  df.lasso <- glmnet(x, y, alpha=1)
  
  ## lasso coefficicent estimates
  #coef(df.lasso)
  
  # pick optimal lambda with cross-validated test set MSE
  set.seed (seed)
  cv.out = cv.glmnet(x, y, alpha=1, nfolds=nrow(x))
  plot(cv.out)
  bestlam=cv.out$lambda.min
  print(paste("Best Lambda:", bestlam))
  
  ## Create the lasso trace plot
  plot(df.lasso,xvar="lambda",label=TRUE)
  abline(v=log(bestlam), lty=2)
  
  ## fit with best lambda
  best.lasso <- glmnet(x, y, alpha=1, lambda=bestlam)
  
  ## look at residual sum of squares
  lasso.preds <- predict(best.lasso ,s=bestlam ,newx=x)
  orig.preds <- predict(lm(y ~ x))
  
  lasso.SSres <- sum( (y - lasso.preds)^2 )
  orig.SSres <- sum( (y - orig.preds)^2 )
  
  print("SSres")
  print(paste("Lasso:", lasso.SSres))
  print(paste("Original:", orig.SSres))
  
  ## R^2
  
  lasso.R2 <- sum((lasso.preds - mean(y))^2) / sum((y - mean(y))^2)
  orig.R2 <- sum((orig.preds - mean(y))^2) / sum((y - mean(y))^2)
  
  print("R^2")
  print(paste("Lasso:", lasso.R2))
  print(paste("Original:", orig.R2))
  
  # check a held out test set MSE
  train=sample(1:nrow(df), nrow(df)*.75)
  
  trainx <- x[train, ]
  trainy <- y[train]
  
  testx <- x[-train, ]
  testy <- y[-train]
  
  tt.lasso <- glmnet(trainx, trainy, alpha=1, lambda=bestlam)
  tt.orig <- lm(y ~ ., data=df[train,])
  
  # print test MSE
  MSE.lasso <- sum((testy - predict(tt.lasso,s=bestlam,newx=testx))^2) / nrow(testx)
  MSE.orig <- sum((testy - predict(tt.orig,newdata=df[-train,]))^2) / nrow(testx)
  print("Test Set MSE")
  print(paste("Lasso:", MSE.lasso))
  print(paste("Original:", MSE.orig))
  
  #return the optimal model
  glmnet(x, y, alpha=1, lambda=bestlam)
}

rlpred <- function(mod, newdata) {
  if('y' %in% names(newdata)) {
    newdata <- subset(newdata, select = -y) # get rid of y, if it's included
  }
  mm <- model.matrix(~., data=newdata)[,2:(ncol(newdata)+1)]
  as.numeric(predict(mod, newx=mm))
}

# ## test it
# 
# ridgemod <- ridgeit(gas)
# lassomod <- lassoit(gas)
# 
# plot(gas$x5, gas$y)
# points(gas$x5, rlpred(ridgemod, gas), col="red")
# points(gas$x5, rlpred(lassomod, gas), col="blue")
