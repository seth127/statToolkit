
###
x <- round(abs(rnorm(20) * 10) , 2)
y <- 5 + 3*x + rnorm(20, mean=0, sd = 3)

Sxx <- sum( (x - mean(x) )^2 )
Sxy <- sum( (x - mean(x) * y ) )

model <- lm(y ~ x)
preds <- predict(model)
#
plot(x, y)
points(x, preds, col = "red")
#
SSres <- sum( (y - preds)^2 )
SSr <- sum( (preds - mean(y))^2 )
SSt <- sum( (y - mean(y))^2 )
###
MSres <- sum((y - preds)^2) / (length(x) - 2)
#aka
MSres <- sum(resid(model)^2) / model$df.residual
###

# R Squared
R2 <- sum((preds - mean(y))^2) / sum((y - mean(y))^2)
# aka
R2 <- SSr / SSt

# CORRELATION COEFFICIENT
cor(x, y)
#[1] 0.9908928
#and just to double check...
cov(x, y) / ( sqrt(var(x) * var(y)) )
#[1] 0.9908928
#and again...
Sxy / ( sqrt( Sxx * SSt ) )
#[1] 0.9908928

# and note that...
B1 <- sqrt(SSt / Sxx) * cor(x, y) 
# B1 will be the same as
lm(y ~ x)$coefficients[2] #fuckin whoa
# sqrt(SSt / Sxx) # is known as the "scale factor" by which you multiply 
##the correlation between x and y to get the slope

#also note that
cor(x, y)^2 #(often written as "r")
# will be equal to
R2 # r squared from above

# CONFIDENCE INTERVAL FOR INTERCEPT
ciB0 <- function(model, df, perc) { #df = degrees of freedom, perc = the percentage you want to test i.e. .95 for 95% confidence interval
  low <- (1 - perc) / 2
  high <- 1 - low
  B0 <- summary(model)$coefficients[1,1]
  B0se <- summary(model)$coefficients[1,2]
  ciB0 <- c(B0 + (qt(low, nrow(df) - 2) * B0se), B0 + (qt(high, nrow(df) - 2) * B0se))
  ciB0
}

# CONFIDENCE INTERVAL FOR SLOPE (OF VARIABLE v)
ciB1 <- function(model, df, perc, v) { #df = degrees of freedom, perc = the percentage you want to test i.e. .95 for 95% confidence interval
  low <- (1 - perc) / 2
  high <- 1 - low
  B1 <- summary(model)$coefficients[v,1]
  B1se <- summary(model)$coefficients[v,2]
  ciB1 <- c(B1 + (qt(low, nrow(df) - 2) * B1se), B1 + (qt(high, nrow(df) - 2) * B1se))
  ciB1
}

# HYPOTHESIS TEST FOR SIGNIFICANCE OF A VARIABLE IN A MODEL
hypVar <- function(mod, df, varNum, perc) {
  degFree <- nrow(df) - nrow(summary(mod)$coefficients)
  testStat <- qt(perc, degFree)
  t <- abs(summary(mod31)$coefficients[varNum, 3])
  print(paste("t:", t))
  print(paste("testStat:", testStat))
  t > testStat
}

# R SQUARED
R2 <- function(model, y) {
  ybar <- mean(y)
  preds <- predict(model)
  sum((preds - ybar)^2) / sum((y - ybar)^2)
}

# PREDICTION INTERVAL
piYihat <- function(x, y, xi, yihat, preds, perc) {
  low <- (1 - perc) / 2
  high <- 1 - low
  n <- length(x)
  xbar <- mean(x)
  
  MSres <- sum((y - preds)^2) / (n - 2)
  Sxx <- sum((x - xbar )^2)
  mess <- sqrt( MSres * (1 + (1/n) + ( (xi - xbar)^2 / Sxx ) ) )
  
  pi <- c(yihat + (qt(low, n - 2) * mess), yihat + (qt(high, n - 2) * mess))
  pi
}

#yihat = predict(MODEL, newdata=data.frame(VARIABLENAME=Xi))
#preds = predict(MODEL)

#piYihat(b1$x8, b1$y, 1800, yihat, preds, .9)
# 4.936392 13.349749 is the 90% prediction interval

# CONFIDENCE INTERVAL
ciYihat <- function(x, y, xi, yihat, preds, perc) {
  low <- (1 - perc) / 2
  high <- 1 - low
  n <- length(x)
  xbar <- mean(x)
  
  MSres <- sum((y - preds)^2) / (n - 2)
  Sxx <- sum((x - xbar )^2)
  mess <- sqrt( MSres * (1/n + ( (xi - xbar)^2 / Sxx ) ) )
  
  ci <- c(yihat + (qt(low, n - 2) * mess), yihat + (qt(high, n - 2) * mess))
  ci
}

#yihat = predict(mod24, newdata=data.frame(x1=275))
#preds = predict(mod24)

#ciYihat(b3$x1, b3$y, 275, yihat, preds, .95)
# 19.58807 21.80952 is the 95% confidence interval


## HYPOTHESIS TESTS AND CONFIDENCE INTERVALS FOR CORRELATIONS
# hard to say if these are really useful or not...
##### HYPOTHESIS
r <- cor(x, y)

arctanh <- function(r) {
  0.5 * log((1 + r) / (1 - r))
}

# the hypothesis test of H0: r = .5
Z0 = (arctanh(r) - arctanh(.5)) * sqrt(length(x) - 3)
# if Z0 is larger than
qt(.975, length(x) - 2)
#then you can reject

###### CONFIDENCE INTERVALS
tanh <- function(u) {
  (exp(u) - exp(-u)) / (exp(u) + exp(-u))
} 

# the 99% confidence interval
quants <- c(.005, .995)
tanh(arctanh(r) - (qnorm(quants) / sqrt(length(x) - 3)))