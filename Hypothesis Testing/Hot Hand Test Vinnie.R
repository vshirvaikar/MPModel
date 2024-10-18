library(stats)
library(VGAM)

ll.bb <- function(ab, k, n) {-1*sum(dbetabinom.ab(k, n, ab[1], ab[2], log=TRUE))}
bic <- function(LL, d, g) {2*LL-d*log(g)} # ll.bb is positive (optim minimizes)

vinnie = read.csv("~/vinnie.csv")
shots.obs = vinnie$n
makes.obs = vinnie$k

trials = 100
n = dim(vinnie)[1]
N = 620

diff.ll = array(0, dim=c(N, trials))
diff.bic = array(0, dim=c(N, trials))
for(trial in 1:trials){
  print(trial)
  makes = makes.obs
  shots = shots.obs
  for(game in 1:N){
    ll.null = -1 * ll.bb(c(1, 1), makes, shots)
    opt.alt = optim(c(1, 1), ll.bb, n=shots, k=makes, 
                    method="L-BFGS-B", lower=c(0.01, 0.01))
    ll.alt = -1 * opt.alt$value
    diff.ll[game, trial] = ll.null - ll.alt
    bic.null <- bic(ll.null, 1, n+game-1)
    bic.alt <- bic(ll.alt, 2, n+game-1)
    diff.bic[game, trial] = bic.null - bic.alt # positive means NULL wins
    
    shots.new = sample(shots, 1)
    if(bic.null > bic.alt){
      p.new = rbeta(1, 1, 1)
    } else {
      p.new = rbeta(1, opt.alt$par[1], opt.alt$par[2])
    }
    makes.new = rbinom(1, shots.new, p.new)
    shots = c(shots, shots.new)
    makes = c(makes, makes.new)
  }
}
winners = ifelse(diff.bic > 0, "null", "alt")
null.prob = sum(winners[N,] == "null")/trials
