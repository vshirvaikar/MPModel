### Convergence diagram for simple point hypothesis test in Section 2

library(ggplot2)

n = 100
theta0 = 0
var = 1
x_obs = rnorm(n, theta0, sqrt(var))

compareLL = function(data, mean0, mean1, var){
  ll0 = sum(dnorm(data, mean = mean0, sd = sqrt(var), log = TRUE))
  ll1 = sum(dnorm(data, mean = mean1, sd = sqrt(var), log = TRUE)) 
  if(ll0 > ll1){return(0)} else{return(1)}
}

N = 2500
trials = 100
theta1 = 0.1
meanLLs = array(0, dim=c(N, trials))
for(trial in 1:trials){
  x_imp = x_obs
  for(j in 1:N){
    if(compareLL(x_imp, theta0, theta1, var) == 0){
      x_new = rnorm(1, theta0, sqrt(var))
    } else {
      x_new = rnorm(1, theta1, sqrt(var))
    }
    x_imp = c(x_imp, x_new)
    meanLLs[j, trial] = mean(x_imp)
  }
}
df_plot = jellyfish(meanLLs)

ggplot(df_plot, aes(x=n, y=value, group=variable)) + 
  ylab("Sample Mean") + xlab("Forward Step") +
  geom_line(size=0.005) + theme(legend.position="none") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black")
