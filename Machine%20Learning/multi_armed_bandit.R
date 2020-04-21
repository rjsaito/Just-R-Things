# 5/15/2017
# Multi-Armed Bandit Test


# binomial trial function
trial = function(n = c(100, 100), p = c(.5, .5), mu = c(200, 220), sd = c(50, 100), type = "binomial"){
  if(type == "binomial"){
    x = rbinom(n[1], 1, p = p[1])
    y = rbinom(n[2], 1, p = p[2])
  } else if(type == "linear"){
    x = rnorm(n[1], mean = mu[1], sd = sd[1])
    y = rnorm(n[2], mean = mu[2], sd = sd[2])
  } else stop("Define valid type")
  samp = list(x = x, y = y)
  return(samp)
}


# traditional ab test
ABtest = function(rounds, n, type = "linear", p.crit = .05, ...){
  x_list = y_list = data.frame(NULL)
  sigRound = NA
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    sim = trial(n = n_list, type = type, ...)
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    if(type == "binomial"){
      successes = c(sum(x_list$x), sum(y_list$y))
      counts = c(length(x_list$x), length(y_list$y))
      hypTest = prop.test(successes, counts)
    } else if(type == "linear"){
      hypTest = t.test(x_list$x, y_list$y)
    } else stop("Define valid type")
    pval = hypTest$p.value
    if(pval < p.crit){
      est = hypTest$estimate
      bestArm = which.max(est)
      n_list = rep(0, 2)
      n_list[bestArm] = n
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      n_list = c(n/2, n/2)
      signif = T
    }
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}




# epsilion-greedy multi armed bandit test
mab_eg = function(rounds, n, p.crit = .05, ...){
  eps = .1
  x_list = y_list = NULL
  sigRound = NA
  bestArm_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, ...)
    } else {
      if(signif){
        n_list = rep(0, 2)
        n_list[bestArm] = n
      } else {
        n_list = rep(n*eps, 2)
        n_list[bestArm] = n*(1-eps)
      }
      sim = trial(n = n_list, ...)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    # if t test shows significant result, move to one group only
    ttest = t.test(x_list$x, y_list$y)
    p = ttest$p.value
    if(p < p.crit){
      est = ttest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      bestArm = which.max(c(mean(x_list), mean(y_list)))
      signif = F
    }
    bestArm_list = c(bestArm_list, bestArm)
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}


#epsilon-decreasing multi armed bandit test
mab_ed = function(rounds, n, power = 3/2, p.crit = .05, ...){
  f_eps = function(x) 1/(x^power)
  x_list = y_list = NULL
  sigRound = NA
  bestArm_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, ...)
    } else {
      if(signif){
        n_list = rep(0, 2)
        n_list[bestArm] = n
      } else {
        eps = f_eps(i)
        n_list = rep(n*eps, 2)
        n_list[bestArm] = n*(1-eps)
      }
      sim = trial(n = n_list, ...)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    # if t test shows significant result, move to one group only
    ttest = t.test(x_list$x, y_list$y)
    p = ttest$p.value
    if(p < p.crit){
      est = ttest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      bestArm = which.max(c(mean(x_list), mean(y_list)))
      signif = F
    }
    bestArm_list = c(bestArm_list, bestArm)
  }
  return(list(x = x_list, y = y_list, SigAt = sigRound))
}


# ucb1 (optimism in the face of uncertainty)
upperBound = function(step, n, C) sqrt(C*2*log(step)/n) 

mab_ucb = function(rounds, n, C = 1000, p.crit = .05, ...){
  x_list = y_list = NULL
  bestArm_list = NULL
  ucb_list = NULL
  n_list = c(n/2, n/2)
  for(i in 1:rounds){
    if(i == 1){
      sim = trial(n = n_list, ...)
    } else {
      n_list = rep(0, 2)
      n_list[bestArm] = n
      sim = trial(n = n_list, ...)
    }
    x_list = rbind(x_list, data.frame(x = sim$x, round = rep(i, n_list[1])))
    y_list = rbind(y_list, data.frame(y = sim$y, round = rep(i, n_list[2])))
    # if t test shows significant result, move to one group only
    ttest = t.test(x_list$x, y_list$y)
    p = ttest$p.value
    if(p < p.crit){
      est = ttest$estimate
      bestArm = which.max(est)
      signif = T
      if(is.na(sigRound)) sigRound = i
      # else even groups
    } else {
      ucb1 = mean(x_list) + upperBound(i*n, length(x_list), C)
      ucb2 = mean(y_list) + upperBound(i*n, length(y_list), C)
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


reps = 50

Rounds = 10
N = seq(100, 500, by = 200)
Mu = list(c(10000, 10500), c(10000, 11000), c(10000, 12000))
Sd = list(c(2000, 6000), c(4000, 4000), c(6000, 2000))
funs = c("ABdesign")

, "mab_eg", "mab_ed", "mab_ucb")
testNames = c("A/B Test", "Epsilon-Greedy  Multi Armed Bandit", "Epsilon-Decreasing Multi Armed Bandit", "UCB1 Multi Armed Bandit")


t_test = function(x_list, y_list, SigAt){
  rounds = max(x_list$round, y_list$round)
  all_results = NULL
  for(i in 1:rounds){
    x = subset(x_list, round <= i)$x
    y = subset(y_list, round <= i)$y
    test = t.test(x, y)
    p = test$p.value
    est = test$estimate
    overall_mean = mean(c(x, y))
    sdx = sd(x)
    sdy = sd(y)
    overall_sd = sd(c(x, y))
    results = c(SigAt = ifelse(i == 1, SigAt, NA), round = i, p = p, est, mean = overall_mean, "sd of x" = sdx, "sd of y" = sdy, sd = overall_sd)
    all_results = rbind(all_results, results)
  }
  return(all_results)
}


p_test = function(x_list, y_list, SigAt){
  rounds = max(x_list$round, y_list$round)
  all_results = NULL
  for(i in 1:rounds){
    x = subset(x_list, round <= i)$x
    y = subset(y_list, round <= i)$y
    xy = c(sum(x), sum(y))
    n = c(length(x), length(y))
    test = prop.test(xy, n)
    p = test$p.value
    est = test$estimate
    overall_mean = mean(c(x, y))
    sdx = sd(x)
    sdy = sd(y)
    overall_sd = sd(c(x, y))
    results = c(SigAt = ifelse(i == 1, SigAt, NA), round = i, p = p, est, mean = overall_mean, "sd of x" = sdx, "sd of y" = sdy, sd = overall_sd)
    all_results = rbind(all_results, results)
  }
  return(all_results)
}




testSim = function(rounds, n, reps, mu = c(200, 220), sd = c(50, 100), p = c(.5, .5), p.crit = .05, FUN = ABdesign, type = "binomial"){
  if(type == "binomial"){
    simulation = do.call("rbind", replicate(reps, ABdesign(rounds = rounds, n = n, mu = mu, sd = sd, type = type, p.crit = p.crit) %$% p_test(x, y, SigAt), simplify = FALSE))
  } else if(type == "linear"){
    simulation = do.call("rbind", replicate(reps, ABdesign(rounds = rounds, n = n, mu = mu, sd = sd, type = type, p.crit = p.crit) %$% t_test(x, y, SigAt), simplify = FALSE))    
  }
  results = simulation %>%
    as.data.frame() %>%
    group_by(round) %>%
    summarise(    
      avg_reward = mean(mean),
      avg_reward_sd = mean(sd),
      avg_diff = mean(`mean of y` - `mean of x`),
      avg_pval = mean(p)
    )
  avg_sig = mean(simulation[,1], na.rm = T)
  return(list(Result = results, AvgSigRound = avg_sig))
}


# loop over parameters and tests
all_results = all_sig_at = NULL
for(rounds in Rounds){
  for(n in N){
    for(mu in Mu){
      for(sd in Sd){
        iteration = c(rounds, n, mu, sd)
        names(iteration) = c("rounds", "n", "mu1", "mu2", "sd1", "sd2")
        for(f in funs){
          result_list = testSim(rounds, n, reps, mu, sd, get(f))
          result = result_list$Result
          sig_at = result_list$AvgSigRound
          testName = testNames[which(funs == f)]
          all_results = rbind(all_results, cbind(rounds, n, mu1 = mu[1], mu2 = mu[2], sd1 = sd[1], sd2 = sd[2], result, Design = testName))
          all_sig_at = rbind(all_sig_at, c(iteration, SigAt = sig_at, Design = testName))
        }   
        cat(paste0("Rounds: ", rounds, ", N: ", n, ", mu: ", paste0(mu, collapse = "/"), ", sd: ",  paste0(sd, collapse = "/"), " \n"))
      }
    } 
  }
}

all_results_mod = all_results %>%
  data.frame(stringsAsFactors = F) %>%
  mutate_at(vars(rounds:avg_pval), funs(as.numeric)) %>%
  mutate(params = paste0("mu = ", mu1, "/", mu2, ", sd = ", sd1, "/", sd2),
         Design = as.character(Design)) %>%
  rename(Reward = avg_reward,
         Rounds = rounds)

all_sig_at_mod = all_sig_at %>%
  data.frame(stringsAsFactors = F) %>%
  mutate_at(vars(rounds:SigAt), funs(as.numeric)) %>%
  mutate(params = paste0("mu = ", mu1, "/", mu2, ", sd = ", sd1, "/", sd2))


n_size = 100
p <- ggplot(data = filter(all_results_mod, n == n_size), aes(x = round, y = Reward, colour = Design)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0,10, by = 2)) + ylab("Avg. Sale Price") + xlab("# Rounds") + ggtitle(paste0("Simulation: Auction Experiment (n = ", n_size, ")"))
p + facet_wrap(~ params, scales = "free_y") +  geom_vline(data = filter(all_sig_at_mod, n == n_size), aes(xintercept = SigAt, colour = Design))
