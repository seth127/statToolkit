setwd("~/Documents/DSI/notes/2-STAT-6021/team assignments")

train <- read.csv("teamassign05train.csv", header=T, stringsAsFactors = F)

## cross validate with the lm function
cv.lm <- function(vars, train, k=5) { ## vars should be a character vector of variable names/combinations
  # the function to do the cross-validation
  theCV <- function(var, train, k) {
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
  dfMSE <- data.frame(t(sapply(vars, theCV, train=train, k=k, USE.NAMES = F)))
  names(dfMSE) <- c('formula', 'MSE', 'adj.R2', 'p-value')
  dfMSE
}

cv.lm('x7', train)
cv.lm(c('x7', 'x5', 'x3+x5'), train)


### FORWARD SUBSET SELECTION

FSS <- function(train, k) { #### the y variable HAS to be called "y"
  vars <- names(train)[!grepl("y", names(train))]
  
  dfMSE <- data.frame(formula = character(length(vars)),
                      MSE = numeric(length(vars)),
                      stringsAsFactors = F)
  
  onelevel <- function(prevWinner) {
    dfMSE <- data.frame(formula = character(length(vars)), 
                        MSE = numeric(length(vars)),
                        stringsAsFactors = F)
    prevX <- unlist(strsplit(prevWinner$formula, "y ~  "))[2]
    
    for (i in 1:length(vars)) {
      vars <- vars[-prevX] ################need to figure out how to kick out old vars
      dfMSE[i, ] <- CVn(train, 5, paste('y ~', prevX, "+", vars[i]))
    }
    dfMSE <- rbind(dfMSE, prevWinner)
    winner <- dfMSE[dfMSE$MSE == max(dfMSE$MSE), ]
    #winner
    dfMSE
  }
  
  for (i in 1:length(vars)) {
    dfMSE[i, ] <- CVn(train, 5, paste('y ~ ', vars[i]))
  }
  winner <- dfMSE[dfMSE$MSE == max(dfMSE$MSE), ]
  
  onelevel(winner)
}

w <- FSS(train, 5)