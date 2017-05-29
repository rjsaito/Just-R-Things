# 5/15/2017
# Riki Saito rsaito@mazdausa.com
# Multi-Armed Bandit Test


# Set path
setwd("C:/Users/rjsai/Dropbox/Data Science/Just-R-Things/Machine Learning/Multi Armed Bandit/") 

# binomial trial function
trial = function(n = c(100, 100), p = c(.5, .5)){
  x = rbinom(n[1], 1, p = p[1])
  y = rbinom(n[2], 1, p = p[2])
  samp = list(x = x, y = y)
  return(samp)
}

# traditional ab test
ABtest = function(rounds, n, p.crit = .05, p = c(.5, .5)){
  x_list = y_list = data.frame(NULL)
  sigRound = NA
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    sim = trial(n = n_list, p = p)
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    successes = c(sum(x_list$x), sum(y_list$y))
    counts = c(length(x_list$x), length(y_list$y))
    hypTest = prop.test(successes, counts)
    pval = hypTest$p.value
    if(pval < p.crit){
      est = hypTest$estimate
      bestArm = which.max(est)
      n_list = rep(0, 2)
      n_list[bestArm] = n
      signif = T
      if(is.na(sigRound)) sigRound = i
    } else {
      n_list = c(n/2, n/2)
      signif = T
    }
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}

# epsilion-greedy multi armed bandit test
mab_eg = function(rounds, n, p.crit = .05, p = c(.5, .5)){
  eps = .1
  x_list = y_list = data.frame(NULL)
  sigRound = NA
  bestArm_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, p = p)
    } else {
      if(signif){
        n_list = rep(0, 2)
        n_list[bestArm] = n
      } else {
        n_list = rep(n*eps, 2)
        n_list[bestArm] = n*(1-eps)
      }
      sim = trial(n = n_list, p = p)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    successes = c(sum(x_list$x), sum(y_list$y))
    counts = c(length(x_list$x), length(y_list$y))
    hypTest = prop.test(successes, counts)
    pval = hypTest$p.value
    if(pval < p.crit){
      est = hypTest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      bestArm = which.max(c(mean(x_list$x), mean(y_list$y)))
      signif = F
    }
    bestArm_list = c(bestArm_list, bestArm)
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}


#epsilon-decreasing multi armed bandit test
mab_ed = function(rounds, n, power = 3/2, p.crit = .05, p = c(.5, .5)){
  f_eps = function(x) 1/(x^power)
  x_list = y_list = NULL
  sigRound = NA
  bestArm_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, p = p)
    } else {
      if(signif){
        n_list = rep(0, 2)
        n_list[bestArm] = n
      } else {
        eps = f_eps(i)
        n_list = rep(n*eps, 2)
        n_list[bestArm] = n*(1-eps)
      }
      sim = trial(n = n_list, p = p)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    successes = c(sum(x_list$x), sum(y_list$y))
    counts = c(length(x_list$x), length(y_list$y))
    hypTest = prop.test(successes, counts)
    pval = hypTest$p.value
    if(pval < p.crit){
      est = hypTest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      bestArm = which.max(c(mean(x_list$x), mean(y_list$y)))
      signif = F
    }
    bestArm_list = c(bestArm_list, bestArm)
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}


# ucb1 (optimism in the face of uncertainty)
upperBound = function(t, n, C) sqrt(C*2*log(t)/n) 

mab_ucb = function(rounds, n, C = 1, p.crit = .05, p = c(.5, .5)){
  x_list = y_list = NULL
  sigRound = NA
  bestArm_list = NULL
  ucb_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, p = p)
    } else {
      n_list = rep(0, 2)
      n_list[bestArm] = n
      sim = trial(n = n_list, p = p)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    successes = c(sum(x_list$x), sum(y_list$y))
    counts = c(length(x_list$x), length(y_list$y))
    hypTest = prop.test(successes, counts)
    pval = hypTest$p.value
    if(pval < p.crit){
      est = hypTest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
    } else {
      ucb1 = mean(x_list$x) + upperBound(i*n, length(x_list$x), C)
      ucb2 = mean(y_list$y) + upperBound(i*n, length(y_list$y), C)
      ucb_list = rbind(ucb_list, c(ucb1, ucb2))
      bestArm = which.max(c(ucb1, ucb2))
      signif = F
    }
    bestArm_list = c(bestArm_list, bestArm)
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}


#####################################################################################

### simulation ###
pacman::p_load(dplyr, tidyr, ggplot2, magrittr)


reps = 1000

Rounds = 20
P.crit = c(0, .01, 0.05)
N = c(50, 100, 300, 500)
P = list(c(.3, .7), c(.4, .6), c(.45, .55))
funs = c("ABtest", "mab_eg", "mab_ed", "mab_ucb")
testNames = c("A/B Test", "Epsilon-Greedy  Multi Armed Bandit", "Epsilon-Decreasing Multi Armed Bandit", "UCB1 Multi Armed Bandit")


p_test = function(x_list, y_list, SigAt){
  rounds = max(x_list$round, y_list$round)
  all_results = NULL
  for(i in 1:rounds){
    x = subset(x_list, round <= i)$x
    y = subset(y_list, round <= i)$y
    xy = c(sum(x), sum(y))
    n = c(length(x), length(y))
    test = prop.test(xy, n)
    pval = test$p.value
    est = test$estimate
    overall_mean = mean(c(x, y))
    sdx = sd(x)
    sdy = sd(y)
    overall_sd = sd(c(x, y))
    results = c(SigAt = ifelse(i == 1, SigAt, NA), round = i, pval = pval, est, mean = overall_mean, "sd of x" = sdx, "sd of y" = sdy, sd = overall_sd)
    all_results = rbind(all_results, results)
  }
  return(all_results)
}


testSim = function(rounds, n, reps, p = c(.5, .5), p.crit = .05, FUN = ABtest){
  simulation = do.call("rbind", replicate(reps, FUN(rounds = rounds, n = n, p.crit = p.crit, p = p) %$% p_test(x, y, SigAt), simplify = FALSE))
  results = simulation %>%
    as.data.frame(stringsAsFactors = F) %>%
    group_by(round) %>%
    summarise(    
      avg_reward = mean(mean),
      avg_reward_sd = mean(sd),
      avg_diff = mean(`prop 2` - `prop 1`),
      avg_pval = mean(pval, na.rm = T)
    ) %>%
    as.data.frame(stringsAsFactors = F)
  avg_sig = mean(simulation[,1], na.rm = T)
  return(list(Result = results, AvgSigRound = avg_sig))
}


# loop over parameters and tests
all_results = all_sig_at = NULL
for(p.crit in P.crit){
  for(rounds in Rounds){
    for(n in N){
      for(p in P){
        iteration = c(rounds, n, p)
        names(iteration) = c("rounds", "n", "p1", "p2")
        for(f in funs){
          result_list = testSim(rounds = rounds, n = n, reps = reps, p = p, p.crit = p.crit, FUN = get(f))
          result = result_list$Result
          sig_at = result_list$AvgSigRound
          testName = testNames[which(funs == f)]
          all_results = rbind(all_results, cbind(rounds, n, p.crit = p.crit, p1 = p[1], p2 = p[2], result, Design = testName))
          all_sig_at = rbind(all_sig_at, c(iteration, SigAt = sig_at, p.crit = p.crit, Design = testName))
        }   
        cat(paste0("Rounds: ", rounds, ", N: ", n, ", p: ", paste0(p, collapse = "/"), " \n"))
      } 
    }
  }
}

all_results_mod = all_results %>%
  data.frame(stringsAsFactors = F) %>%
  mutate_at(vars(rounds:avg_pval), funs(as.numeric)) %>%
  mutate(params = paste0("p.crit = ", p.crit, ", n = ", n, ", p = ", p1, "/", p2),
         Design = as.character(Design)) %>%
  rename(Reward = avg_reward,
         Rounds = rounds)

all_sig_at_mod = all_sig_at %>%
  data.frame(stringsAsFactors = F) %>%
  mutate_at(vars(rounds:SigAt), funs(as.numeric)) %>%
  mutate(params = paste0("p.crit = ", p.crit, ", n = ", n, ", p = ", p1, "/", p2),
         Design = as.character(Design))



n_size = 100
pl <- ggplot(data = filter(all_results_mod, n == n_size), aes(x = round, y = Reward, colour = Design)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0,20, by = 2)) + ylab("Avg. Sale Price") + xlab("# Rounds") + ggtitle(paste0("Simulation: Auction Experiment (n = ", n_size, ")")) + geom_vline(data = filter(all_sig_at_mod, n == n_size), aes(xintercept = SigAt, colour = Design), show.legend = T) 
pl + facet_wrap(~ params, scales = "free_y")



# to a pdf
pdf("ab_mab_simulation_result.pdf", height = 8, width = 15)
for(n_size in N){
  pl <- ggplot(data = filter(all_results_mod, n == n_size), aes(x = round, y = Reward, colour = Design)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0,20, by = 2)) + ylab("Avg. Sale Price") + xlab("# Rounds") + ggtitle(paste0("Simulation: Auction Experiment (n = ", n_size, ")"))
  print(pl + facet_wrap(~ params, scales = "free_y") +  geom_vline(data = filter(all_sig_at_mod, n == n_size), aes(xintercept = SigAt, colour = Design), show.legend = T))
}
dev.off()


# to a tiff
for(n_size in N){
  tiff(paste0("ab_mab_simulation_result_n_", n_size, ".tiff"), units="in", width=15, height=8, res=300, compression = 'lzw')
  pl <- ggplot(data = filter(all_results_mod, n == n_size), aes(x = round, y = Reward, colour = Design)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0,20, by = 2)) + ylab("Avg. Sale Price") + xlab("# Rounds") + ggtitle(paste0("Simulation: Auction Experiment (n = ", n_size, ")"))
  print(pl + facet_wrap(~ params, scales = "free_y") +  geom_vline(data = filter(all_sig_at_mod, n == n_size), aes(xintercept = SigAt, colour = Design), show.legend = T))
  dev.off()
}



