library(dplyr)

# Convergence plot for first illustration with 2 components 
sd = 0.6
meanEMs = array(0, dim=c(N, trials))
x_obs = sample(means+sd*z)
for(trial in 1:trials){
  x_imp = x_obs
  for(j in 1:N){
    em.model = Mclust(x_imp, G=1:2, modelNames="E") 
    meanEMs[j, trial] = em.model$G
    x_new = sim(modelName = em.model$modelName, 
                parameters = em.model$parameters, 1)[,2]
    x_imp = c(x_imp, x_new)
  }
}

df_plot = jellyfish(meanEMs)
jitter <- data.frame(variable = paste0("X", 1:100), 
                     adj = seq(-0.1, 0.1, length.out=100)) # add some noise
df_plot <- df_plot %>% left_join(jitter, by = "variable") %>% 
  mutate(value = value + adj) %>% select(-adj) 

ggplot(df_plot, aes(x=n, y=value, group=variable)) + 
  ylab("Number of Components") + xlab("Forward Step") +
  geom_line(size=0.1) + theme(legend.position="none") +
  scale_y_continuous(limits = c(0.7, 2.3), breaks = c(1, 2)) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black")

# Convergence plot for second illustration with 3 components 
tentacles = 100
df_plot = jellyfish(clust.mp, tentacles)
jitter <- data.frame(variable = paste0("X", 1:tentacles), 
                     adj = seq(-0.1, 0.1, length.out=tentacles))
df_plot <- df_plot %>% left_join(jitter, by = "variable") %>% 
  mutate(value = value + adj) %>% select(-adj) 

ggplot(df_plot, aes(x=n, y=value, group=variable)) + 
  ylab("Number of Components") + xlab("Forward Step") +
  geom_line(size=0.1) + theme(legend.position="none") +
  scale_y_continuous(limits = c(0.5, 9.5), breaks = seq(1, 9))
