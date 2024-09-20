### First density estimation illustration with two-component GMM

library(mclust)
library(tidyr)
library(ggplot2)

n = 50
means <- c(rep(-1, n/2), rep(1, n/2))
z = rnorm(n)
sd.seq = seq(0.5, 1, 0.025)

trials = 100
N = 200
clust.mp = array(0, dim=c(length(sd.seq), trials))

for(i in seq_along(sd.seq)){
  set.seed(247)
  sd = sd.seq[i]
  x_obs = sample(means+sd*z)
  for(trial in 1:trials){
    x_imp = x_obs
    for(j in 1:N){
      em.model = Mclust(x_imp, G=1:2, modelNames="E") 
      x_new = sim(modelName = em.model$modelName, 
                  parameters = em.model$parameters, 1)[,2]
      x_imp = c(x_imp, x_new)
    }
    clust.mp[i, trial] = em.model$G
  }
}

# Plot sample densities
sd = .9
x_obs = sample(means+sd*z)
plot(density(x_obs, bw=.5), xlab="", ylab="", main="")

# Plot results
rowcounts = apply(clust.mp[,], 1, function(x) table(factor(x, levels = 1:2)))
plot.data = data.frame(t(rbind(sd.seq, rowcounts)))
plot.data = pivot_longer(plot.data, cols = c(X1, X2), 
                         names_to = "Components", values_to = "Trials")
plot.data$Components = ifelse(plot.data$Components == "X1", "1", "2")
plot.data$Components <- factor(plot.data$Components, levels = c("2", "1"))

ggplot(plot.data, aes(x = sd.seq, y = Trials, fill = Components)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "", x = "Standard Deviation", y = "Percentage of Trials") +
  scale_fill_manual(values = c("black", "gray70")) +
  theme_minimal() +  theme(plot.background = element_blank(),
                           panel.grid.minor = element_blank(), 
                           panel.background = element_blank())
