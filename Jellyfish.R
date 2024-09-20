### Data processing function to generate "jellyfish plot"
### convergence diagrams used in introduction and illustrations

library(reshape)
library(ggplot2)

jellyfish = function(mat, tentacles=100){
  df = data.frame(mat)
  N = dim(df)[1]
  df_plot = data.frame(n = seq.int(1, N), df[,sample(ncol(df),tentacles)])
  return(melt(df_plot, id.vars = "n"))
}

# Introduction schematic diagram
N = 1000
trials = 100
x_obs = rnorm(n)
means = array(0, dim=c(N, trials))
for(trial in 1:trials){
  x_imp = x_obs
  for(j in 1:N){
    x_imp = c(x_imp, rnorm(1))
    means[j, trial] = mean(x_imp)
  }
}

jelly = means[1:1000,]*4+2.5
sqrtbase = sqrt(seq(0, 1, length.out=1000))
jelly[,1:15] = jelly[,1:15] - 1.5 * sqrtbase
jelly[,16:50] = jelly[,16:50] - 0.5 * sqrtbase
jelly[,51:85] = jelly[,51:85] + 0.5 * sqrtbase
jelly[,86:100] = jelly[,86:100] + 1.5 * sqrtbase
df.plot = jellyfish(jelly)

ggplot(df.plot, aes(x=n, y=value, group=variable)) + 
  ylab("Sample Mean") + xlab("Resampling Step") + ylim(0.8, 4.5) +
  geom_line(size=0.005) + theme(legend.position="none") +
  annotate("text", x=966, y=1.34, label = expression(M[1]), size = 6) +
  annotate("text", x=966, y=2.43, label = expression(M[2]), size = 6) +
  annotate("text", x=966, y=3.25, label = expression(M[3]), size = 6) +
  annotate("text", x=966, y=4.25, label = expression(M[4]), size = 6)
### geom_line size = 0.1 to view in console // 0.005 to export
