setwd("~/Documents/DSI/notes/2-STAT-6021/team assignments")

train <- read.csv("teamassign05train.csv", header=T, stringsAsFactors = F)

## cross validate with the lm function
cv.lm <- function(vars, train, k=5) { ## vars should be a character vector of variable names/combinations
  # the function to do the cross-validation
  theCV <- function(var, train, k, seed) {
    # create formula
    form = paste('y ~', var)
    #make column for preds
    train$preds <- numeric(nrow(train))
    #randomize the indexes
    nums <- sample(row.names(train), nrow(train))
    #split the indexes into k groups
    nv <- split(nums, cut(seq_along(nums), k, labels = FALSE))
    #subset the training data into k folds
    trainlist <- list()
    for (i in 1:k) {
      trainlist[[i]] <- train[nv[[i]], ]
    }
    #trainlist
    #run on each fold
    for (i in 1:k) {
      ftrainlist <- trainlist[-i]
      ftrain <- ftrainlist[[1]]
      for (j in 2:length(ftrainlist)) {
        ftrain <- rbind(ftrain, ftrainlist[[j]])
      }
      mod <- lm(as.formula(paste(form,' - preds')), data = ftrain) ### the model
      trainlist[[i]]$preds <- predict(mod, newdata = trainlist[[i]])
    }
    #reassemble
    cvdata <- ftrainlist[[1]]
    for (j in 2:length(trainlist)) {
      cvdata <- rbind(cvdata, trainlist[[j]])
    }
    
    # cross-validated test set MSE
    ###degfree <- nrow(cvdata) - ncol(subset(cvdata, select = -c(y, preds))) ##just use n?
    MSE <- sum((cvdata$y-cvdata$preds)^2) / nrow(cvdata)
    
    ##training set stats
    m <- lm(as.formula(paste(form,' - preds')), data = train)
    # adjusted R-squared
    aR2 <- summary(m)$adj.r.squared
    
    # p-value from F-stat
    lmp <- function (modelobject) {
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    p <- lmp(m)
    
    list(form, MSE, aR2, p)
  }
  #
  # now call that function on all the variable combinations
  dfM <- sapply(vars, theCV, train=train, k=k, simplify = 'array', USE.NAMES = F)
  df <- data.frame(formula = unlist(dfM[1,]),
                   MSE = unlist(dfM[2,]),
                   adj.R2 = unlist(dfM[3,]),
                   p.value = unlist(dfM[4,]), stringsAsFactors = F)
  df
}

cv.lm('x7', train)
plus <- cv.lm(c('x7', 'x5', 'x3+x5'), train)


### FORWARD SUBSET SELECTION

FSS <- function(train, k) { #### the y variable MUST be called "y"
  # master vector of variables
  varsMaster <- names(train)[!grepl("y", names(train))]
  # cross validation on single variables
  df <- cv.lm(varsMaster, train, k)
  # create a master df to store all levels
  dfMaster <- df
  # pick the best one
  winner <- df[df$MSE==min(df$MSE), 1]
  varsWinner <- gsub(" ", "", gsub("y ~ ", "", winner))
  # subset out remaining vars
  varsRemain <- varsMaster[!(varsMaster %in% unlist(strsplit(varsWinner, "+", fixed = T)))]
  # paste remaining vars onto winners to create new combinations
  newVars <- paste(varsWinner, varsRemain, sep="+")
  #
  # loop over all combinations, picking the best one each level
  while(length(varsRemain) > 0) {
    # run cross-validation with new variable combinations
    df <- cv.lm(newVars, train, k)
    # store new level stats in master df
    dfMaster <- rbind(dfMaster, df)
    # pick best one from new level
    winner <- df[df$MSE==min(df$MSE), 1]
    varsWinner <- gsub(" ", "", gsub("y ~ ", "", winner))
    print(paste("varsWinner", varsWinner, df[df$MSE==min(df$MSE), 2])) #####
    # subset out remaining vars
    varsRemain <- varsMaster[!(varsMaster %in% unlist(strsplit(varsWinner, "+", fixed = T)))]
    #print(paste("varsRemain", paste(varsRemain, collapse = ", "))) ###
    # paste remaining vars onto winners to create new combinations
    newVars <- paste(varsWinner, varsRemain, sep="+")
  }
  # output
  print(paste("optimal model:", dfMaster[dfMaster$MSE == min(dfMaster$MSE), 1]))
  list(dfMaster[dfMaster$MSE == min(dfMaster$MSE), 1], ## the optimal formula
       dfMaster[dfMaster$MSE == min(dfMaster$MSE), ], ## the optimal formula plus stats for it
       dfMaster) ## stats for all of the options tested
}

w <- FSS(train, 5)
w[[1]]
w[[2]]
# if you want to look choose by adj.R2 instead, just look at w[[3]] and pick the lowest adj.R2

### BACKWARD SUBSET SELECTION

BSS <- function(train, k) { #### the y variable MUST be called "y"
  # master vector of variables
  varsMaster <- names(train)[!grepl("y", names(train))]
  # cross validation on all variables together
  varsAll <- paste(varsMaster, collapse = "+")
  df <- cv.lm(varsAll, train, k)
  # create a master df to store all levels
  dfMaster <- df
  
  # pick the best one
  winner <- varsAll
  varsWinner <- gsub(" ", "", gsub("y ~ ", "", winner))
  # subset out remaining vars
  varsRemain <- unlist(strsplit(varsWinner, "+", fixed = T))
  # paste remaining vars onto winners to create new combinations
  newVars <- character()
  for (i in 1:length(varsRemain)) {
    newVars[i] <- paste(varsRemain[-i], collapse = "+")
  }

  #
  # loop over all combinations, picking the best one each level
  while(length(newVars) > 1) {
    # run cross-validation with new variable combinations
    df <- cv.lm(newVars, train, k)
    # store new level stats in master df
    dfMaster <- rbind(dfMaster, df)
    # pick best one from new level
    winner <- df[df$MSE==min(df$MSE), 1]
    varsWinner <- gsub(" ", "", gsub("y ~ ", "", winner))
    print(paste("varsWinner", varsWinner, df[df$MSE==min(df$MSE), 2])) ###
    # make options for next level
    varsRemain <- unlist(strsplit(varsWinner, "+", fixed = T))
    newVars <- character()
    for (i in 1:length(varsRemain)) {
      newVars[i] <- paste(varsRemain[-i], collapse = "+")
    }
  }
  # output 
  print(paste("optimal model:", dfMaster[dfMaster$MSE == min(dfMaster$MSE), 1]))
  list(dfMaster[dfMaster$MSE == min(dfMaster$MSE), 1], ## the optimal formula
       dfMaster[dfMaster$MSE == min(dfMaster$MSE), ], ## the optimal formula plus stats for it
       dfMaster) ## stats for all of the options tested
}

bw <- BSS(train, 5)
bw[[1]]
bw[[2]]
# if you want to look choose by adj.R2 instead, just look at bw[[3]] and pick the lowest adj.R2

##### OTHER MODELS CROSS VALIDATION
## cross validated RANDOM FOREST
cv.rf <- function(train, k=5, returnDF=F) {
  #make column for preds
  train$preds <- factor(x=rep(levels(train$y)[1], nrow(train)), 
                        levels = levels(train$y))
  #randomize the indexes
  nums <- sample(row.names(train), nrow(train))
  #split the indexes into k groups
  nv <- split(nums, cut(seq_along(nums), k, labels = FALSE))
  #subset the training data into k folds
  trainlist <- list()
  for (i in 1:k) {
    trainlist[[i]] <- train[nv[[i]], ]
  }
  #trainlist
  #run on each fold
  for (i in 1:k) {
    ftrainlist <- trainlist[-i]
    ftrain <- ftrainlist[[1]]
    for (j in 2:length(ftrainlist)) {
      ftrain <- rbind(ftrain, ftrainlist[[j]])
    }
    ############# THE MODEL #######################
    #mod <- lm(as.formula(paste(form,' - preds')), data = ftrain) ### the model
    mod <- randomForest(y ~ .-preds, data=ftrain, importance=TRUE,proximity=FALSE) ### the model
    ###############################################
    trainlist[[i]]$preds <- predict(mod, newdata = trainlist[[i]])
    print(paste("finished fold", i))
  }
  #reassemble
  cvdata <- ftrainlist[[1]]
  for (j in 2:length(trainlist)) {
    cvdata <- rbind(cvdata, trainlist[[j]])
  }
  # return stats
  ##raw accuracy
  ra <- nrow(cvdata[cvdata$y == cvdata$preds,]) / nrow(cvdata)
  print(paste("Raw Accuracy:", ra))
  ##balanced error rate
  ###http://spokenlanguageprocessing.blogspot.com/2011/12/evaluating-multi-class-classification.html
  nk <- length(levels(train$y))
  recall <- numeric(nk)
  for (i in 1:nk) {
    ck <- levels(train$y)[i]
    recall[i] <- nrow(predDF[predDF$y==ck & predDF$preds==ck,]) / nrow(predDF[predDF$y==ck,])
  }
  BER <- 1 - (sum(recall)/nk)
  print(paste("Balanced Error Rate:", BER))
  # return actual predictions
  cvdata <- cvdata[order(as.numeric(row.names(cvdata))), ]
  if(returnDF == T) {
    return(cvdata[,c('y', 'preds')])
  } else {
    return()
  }
}

predDF <- cv.rf(moddf, returnDF = T)
