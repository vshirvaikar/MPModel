### Variable selection illustration in Section 5.2

library(MASS)
library(ggplot2)
library(reshape2)
library(dplyr)
library(rjags)
library(scales)

# Predictive resampling with penalty and "blockstrapping"
resample <- function(X, Y, method, blocks){
  data.trial <- data.frame(cbind(Y, X))
  X = data.frame(X)
  for(block in 1:blocks){
    model.low = lm(Y~1, data=data.trial)
    if(method == "AIC") {pen=2} else {pen=log(nrow(data.trial))}
    model.step = stepAIC(model.low, direction="forward", k=pen, trace=FALSE,
                         scope=list(lower=model.low, upper=lm(Y~., data=data.trial)))
    Y.pred = predict(model.step, X, se=T)
    if(sum(is.na(Y.pred$se.fit)) > 0){
      Y.new = Y.pred$fit
    } else {
      Y.new = rnorm(length(Y), Y.pred$fit, Y.pred$se.fit)
    }
    data.imp = cbind(Y.new, X)
    colnames(data.imp)[1] = "Y"
    data.trial = rbind(data.trial, data.imp)
  }
  model.vars <- names(coef(model.step))
  model.vars <- gsub("X", "", model.vars[grep("X", model.vars)]) 
  print(method)
  return(as.numeric(model.vars))
}

# Gibbs sampling approximation of RJ-MCMC
jump <- function(X, Y){
  jags.data <- list("N" = length(Y), "K" = k, "X" = X, "y" = Y)
  model.path = "gibbs.jags" # specify correctly
  init.values <- function() {
    list(
      beta0 = rnorm(1), # intercept parameter
      beta = rnorm(k), # individual coefficients
      tau = max(rgamma(1, 0.01, 0.01), 1e-200), # noise variance
      gamma = rbinom(k, 1, 0.5) # inclusion of coefficient
    )
  }
  parameters <- c("beta", "gamma")
  model <- jags.model(file = model.path, data = jags.data, 
                      inits = init.values, n.chains = 3, n.adapt = 1000)
  update(model, 5000) # burn-in period
  samples <- coda.samples(model, variable.names = parameters, n.iter = 10000, thin = 10)
  samples.matrix <- as.matrix(samples)
  gamma.samples <- samples.matrix[, grep("^gamma", colnames(samples.matrix))]
  return(colMeans(gamma.samples))
}
