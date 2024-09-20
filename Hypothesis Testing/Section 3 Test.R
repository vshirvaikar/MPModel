### Two-sided hypothesis testing example in Section 3

library(parallel)

seeds = 100
sample.sizes <- c(30, 100, 300, 1000)

parallax <- function(seed) {
  output = array(0, c(length(sample.sizes), 3))
  for(i in seq_along(sample.sizes)) {
    set.seed(seed)
    n = sample.sizes[i]
    print(paste0(seed, " ", n))
    
    # Generate with mean=0 under H0 or mean=/=0 under H1
    obs.data <- rnorm(n, mean=0.1)
    
    output[i, 1] <- t.test(obs.data)$p.value
    
    x_bar = mean(obs.data)
    lik.ratio = prod(exp(x_bar*(obs.data-x_bar/2)))
    output[i, 2] = min(1, 1/lik.ratio)
    
    big_n = n*21
    nulls = 0
    trials = 1000
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
      }
      x_bar = mean(imp.data)
      if((big_n)*x_bar^2 < log(big_n)){
        nulls = nulls+1
      }
    }
    output[i, 3] = nulls/trials
  }
  return(output)
}

results <- mclapply(1:seeds, parallax, mc.cores = 4)

p.values <- array(0, c(seeds, length(sample.sizes)))
our.values = array(0, c(seeds, length(sample.sizes)))
e.values = array(0, c(seeds, length(sample.sizes)))
for(i in 1:seeds){
  p.values[i,] = results[[i]][,1]
  e.values[i,] = results[[i]][,2]
  our.values[i,] = results[[i]][,3]
}
