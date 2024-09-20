### Second density estimation illustration with two-component GMM

library(dirichletprocess)
library(mclust)
library(ggplot2)
library(parallel)

n <- 20 # change to n = 50 for second trial
weights <- c(0.3, 0.3, 0.4)
means <- c(-4, 0, 3)
sds <- c(1, 1, 1)

x_obs <- numeric(0)
for(i in seq_along(weights)) {
  x_obs = c(x_obs, rnorm(n*weights[i], means[i], sds[i]))
}
x_obs <- sample(x_obs) # shuffle

# Plot sample densities
plot(density(x_obs), main="", xlab="", ylab="")

# Classic DPMM approach with Gibbs sampling
chains = 8
burnin = 500
iterations = burnin+2000
clust.dp = array(0, dim=c(chains, iterations))
for(chain in 1:chains){
  dp.model <- DirichletProcessGaussian(x_obs)
  dp.fit <- Fit(dp.model, iterations, progressBar = TRUE)
  clust.dp[chain, ] <- sapply(dp.fit$weightsChain, function(wc) length(wc))
}

# Diagnose MCMC convergence with trace plot
data.mcmcplot <- data.frame(Iteration = rep(1:iterations, chains),
                            Clusters = array(clust.dp), 
                            Trace = factor(rep(1:chains, each = 2000)))
ggplot(data.mcmcplot, aes(x = Iteration, y = Clusters, color = Trace)) + 
  theme_minimal() + geom_line()

# Martingale approach with EM algorithm
N = 600
trials = 400
clust.mp = array(0, dim=c(N, trials))

for(trial in 1:trials){
  print(trial)
  x_imp = x_obs
  for(j in 1:N){
    em.model = Mclust(x_imp) # automatically calculates BIC for k from 1 to 9
    clust.mp[j, trial] = em.model$G
    x_new = sim(modelName = em.model$modelName, parameters = em.model$parameters, 1)[,2]
    x_imp = c(x_imp, x_new)
  }
}

# Plot results
dist.dp = array(clust.dp[,(burnin+1):iterations])
post.dp <- prop.table(table(factor(dist.dp, levels = 1:max(dist.dp))))
plot.dp <- data.frame(G = names(post.dp), Percentage = as.numeric(post.dp))
plot.dp$G <- factor(plot.dp$G, levels = 1:max(dist.dp))
ggplot(plot.dp, aes(x = G, y = Percentage)) +
  geom_bar(stat = "identity", fill = "gray70") +
  labs(title = "", x = "", y = "Posterior Probability") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

dist.mp = clust.mp[j,]
post.mp <- prop.table(table(factor(dist.mp, levels = 1:max(dist.mp))))
plot.mp <- data.frame(G = names(post.mp), Percentage = as.numeric(post.mp))
plot.mp$G <- factor(plot.mp$G, levels = 1:max(dist.mp))
ggplot(plot.mp, aes(x = G, y = Percentage)) +
  geom_bar(stat = "identity", fill = "gray70") +
  labs(title = "", x = "", y = "Posterior Probability") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
