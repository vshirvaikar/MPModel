### Simple point hypothesis testing demonstration in Section 2

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
trials = 1000
interval = 0.01
theta1s = seq(-0.3, 0.3, interval)
winners = array(0, dim=c(length(theta1s), trials))
for(i in seq_along(theta1s)){
  print(i)
  theta1 = theta1s[i]
  if(theta1 == 0){
    next
  }
  for(trial in 1:trials){
    x_imp = x_obs
    for(j in 1:N){
      if(compareLL(x_imp, theta0, theta1, var) == 0){
        x_new = rnorm(1, theta0, sqrt(var))
      } else {
        x_new = rnorm(1, theta1, sqrt(var))
      }
      x_imp = c(x_imp, x_new)
    }
    winners[i, trial] = compareLL(x_imp, theta0, theta1, var)
  }
}
results = rowSums(winners)/trials
results[(length(results)+1)/2] = 1/2

data.plot <- data.frame(x = theta1s, y = results)
data.plot$color <- ifelse(data.plot$x == 0, "0", "1")
ggplot(data.plot, aes(x, y, fill = color)) + theme_minimal() +
  geom_bar(stat = "identity", width = interval*0.7) + 
  scale_y_continuous(limits = c(0, 1)) +   
  scale_fill_manual(values = c("1"="gray70", "0"="black"), guide = FALSE) +
  labs(title = "", x = expression(paste("Alternate ", theta)), 
       y = expression("Posterior Probability " ~ P(H[1] ~ "|" ~ x))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(xintercept = mean(x_obs), color = "black")
