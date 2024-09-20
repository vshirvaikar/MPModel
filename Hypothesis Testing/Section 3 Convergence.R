### Convergence diagram for two-sided hypothesis test in Section 3

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

n = 30 # repeat for 30, 100, 300, 1000
obs.data <- rnorm(n, mean=0.1)
t.test(obs.data)$p.value
big_n = n*31
nulls = 0
trials = 100

means = array(0, dim=c(big_n, trials))
for(trial in 1:trials){
  imp.data = obs.data
  for(j in (n+1):big_n){
    x_bar = mean(imp.data)
    if((j-1)*x_bar^2 < log(j-1)){
      new_obs = rnorm(1)
    } else {
      new_obs = rnorm(1, x_bar, 1)
    }
    imp.data = c(imp.data, new_obs)
    means[j, trial] = mean(imp.data)
  }
  x_bar = mean(imp.data)
  if((big_n)*x_bar^2 < log(big_n)){
    nulls = nulls+1
  }
}
df_plot = jellyfish(means[(n+1):big_n,])

ggplot(df_plot, aes(x=n, y=value, group=variable)) + 
  ylab("Sample Mean") + xlab("Forward Step") +
  geom_line(size=0.1) + theme(legend.position="none") +
  geom_vline(xintercept = n*20, linetype = "dashed", color = "black")
