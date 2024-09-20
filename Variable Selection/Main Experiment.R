### Variable selection illustration in Section 5.2

library(MASS)
library(ggplot2)
library(reshape2)
library(dplyr)
library(rjags)
library(scales)

n.tests = c(10, 20, 50, 100)
k = 20
trials = 100
blocks = 10

sel.bic = matrix(0, nrow=length(n.tests), ncol=k)
rownames(sel.bic) = n.tests
colnames(sel.bic) = paste("X", 1:k, sep="")
sel.aic = sel.bic
sel.rjm = array(0, dim=c(length(n.tests), k, trials))

# Direct comparison of methods with regenerated data
for(trial in 1:trials){
  print(trial)
  print(Sys.time())
  for(i in seq_along(n.tests)){
    n.test = n.tests[i]
    Xobs <- matrix(rnorm(n.test * k), ncol = k)
    colnames(Xobs) <- colnames(sel.bic)
    beta <- c(rep(1, 5), rep(0, 15))
    Y <- as.numeric(Xobs %*% beta + rnorm(n.test))
    
    vars.bic = resample(Xobs, Y, "BIC", blocks)
    sel.bic[i, vars.bic] <- sel.bic[i, vars.bic] + 1
    vars.aic = resample(Xobs, Y, "AIC", blocks)
    sel.aic[i, vars.aic] <- sel.aic[i, vars.aic] + 1
    weights.rjm = jump(Xobs, Y)
    sel.rjm[i,,trial] <- weights.rjm
  }
}

# Calculate average Gibbs sampling posterior probability
sel.rj1 = apply(sel.rjm, c(1, 2), function(x) mean(x)*100)
rownames(sel.rj1) = rownames(sel.bic)
colnames(sel.rj1) = colnames(sel.bic)

# Calculate number of trials with Gibbs probability > 0.5
sel.rj2 = apply(sel.rjm, c(1, 2), function(x) sum(x >= 0.5))
rownames(sel.rj2) = rownames(sel.bic)
colnames(sel.rj2) = colnames(sel.bic)
