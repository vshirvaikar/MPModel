### "Hot hand" multi-level hypothesis test from Section 5.3

library(stats)
library(VGAM)

ll.p <- function(p, k, n) {-1*sum(dbetabinom(k, n, p, log=TRUE))}
ll.bb <- function(ab, k, n) {-1*sum(dbetabinom.ab(k, n, ab[1], ab[2], log=TRUE))}
bic <- function(LL, d, g) {2*LL-d*log(g)} # ll.bb is positive (optim minimizes)

n = 20
shots.obs = sample(5:15, n, replace = TRUE)

trials = 100
N = 180
ws = seq(0.5, 2.5, 0.5)
null.prob = numeric(0)
diffs.ll = list()
diffs.bic = list()
for(w in ws){
  print(w)
  makes.obs = sapply(shots.obs, function(x) rbinom(1, x, rbeta(1, w, w)))
  diff.ll = array(0, dim=c(N, trials))
  diff.bic = array(0, dim=c(N, trials))
  for(trial in 1:trials){
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
  diffs.ll = append(diffs.ll, list(diff.ll))
  diffs.bic = append(diffs.bic, list(diff.bic))
  winners = ifelse(diff.bic > 0, "null", "alt")
  null.prob = c(null.prob, sum(winners[N,] == "null")/trials)
}
barplot(null.prob, names.arg = ws, ylab = "% Accept H0")
