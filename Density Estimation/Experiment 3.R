### Third density estimation illustration with real-world galaxy data

library(dirichletprocess)
library(mclust)
library(ggplot2)
library(parallel)
library(MASS)

data(galaxies)
x_obs = galaxies/1000

# Plot sample densities
plot(density(x_obs), main="", xlab="", ylab="")

# Classic DPMM approach with u0 prior shifted to true mean
chains = 8
burnin = 500
iterations = burnin+2000
clust.dp1 = array(0, dim=c(chains, iterations))
for(chain in 1:chains){
  dp.model <- DirichletProcessGaussian(x_obs, g0Priors = c(mean(x_obs),1,1,1))
  dp.fit <- Fit(dp.model, iterations, progressBar = TRUE)
  clust.dp1[chain, ] <- sapply(dp.fit$weightsChain, function(wc) length(wc))
}
# and with alpha concentration prior decreased for fewer clusters
clust.dp2 = array(0, dim=c(chains, iterations))
for(chain in 1:chains){
  dp.model <- DirichletProcessGaussian(x_obs, g0Priors = c(mean(x_obs),1,1,1),
                                       alphaPriors = c(1,8))
  dp.fit <- Fit(dp.model, iterations, progressBar = TRUE)
  clust.dp2[chain, ] <- sapply(dp.fit$weightsChain, function(wc) length(wc))
}

# Martingale approach with EM algorithm
N = 1500
trials = 100
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
dist.dp1 = array(clust.dp1[,(burnin+1):iterations])
dist.dp2 = array(clust.dp2[,(burnin+1):iterations])
dist.mp = clust.mp[j,]

proportions <- function(x) {
  counts <- sapply(1:8, function(b) sum(x == b))
  count_bin9 <- sum(x >= 9)
  props <- c(counts, count_bin9) / length(x)
  return(props)
}

prop.mp <- proportions(dist.mp)
prop.dp2 <- proportions(dist.dp2)
prop.dp1 <- proportions(dist.dp1)

bins <- c(as.character(1:8), ">8")
dfmp <- data.frame(Method = "Resampling",
                   bin = factor(bins, levels = bins), proportion = prop.mp)
dfdp2 <- data.frame(Method = "DPMM with tuned prior",
                    bin = factor(bins, levels = bins), proportion = prop.dp2)
dfdp1 <- data.frame(Method = "DPMM with default prior",
                    bin = factor(bins, levels = bins), proportion = prop.dp1)
df.plot <- bind_rows(dfmp, dfdp2, dfdp1)
df.plot$Method <- factor(df.plot$Method, levels = c("Resampling", 
                                                    "DPMM with tuned prior", "DPMM with default prior"))
ggplot(df.plot, aes(x = bin, y = proportion, fill = Method)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  scale_fill_manual(values = c("white", "gray70", "black")) +
  labs(x = "", y = "Posterior Probability", title = "") +
  theme_minimal() + theme(legend.position = "right")
