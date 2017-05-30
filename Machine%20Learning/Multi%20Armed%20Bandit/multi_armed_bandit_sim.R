# Riki Saito rjsaito@gmail.com
# Functions for A/B Testing and Multi-Armed Bandit Test simulations

#' Binomial random sample generator for two group simulation
#'
#' @param n: a vector of length 2 for sample size of binomial simulation
#' @param p : a vector of length 2 for binomial probability
#'
#' @return samp : return sample as a list of 'x' and 'y'
#' @export
#'
#' @examples
trial = function(n = c(100, 100), p = c(.5, .5)){
  x = rbinom(n[1], 1, p = p[1])
  y = rbinom(n[2], 1, p = p[2])
  samp = list(x = x, y = y)
  return(samp)
}

#' A/B Test simulation on two randomly sampled groups from a binomial distribution
#' 
#' @param rounds : number of rounds of testing
#' @param n : a vector of length 2 for sample size of binomial simulation
#' @param p.crit : a critival p-value for the means of two proportion test that occurs at every round
#' @param p : a vector of length 2 for binomial probability
#'
#' @return a list of 1) x_list (data.frame of values and round from samples in first group), 2) y_lisy (data.frame of values and round from samples in second group), and 3) round at which statistical significance was found between the two groups
#' @export
#'
#' @examples ABtest(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
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

#' Epsilion-Greedy Multi Armed Bandit test Simulation
#'
#' @param rounds : number of rounds of testing
#' @param n : a vector of length 2 for sample size of binomial simulation
#' @param p.crit : a critival p-value for the means of two proportion test that occurs at every round
#' @param p : a vector of length 2 for binomial probability
#'
#' @return a list of 1) x_list (data.frame of values and round from samples in first group), 2) y_lisy (data.frame of values and round from samples in second group), and 3) round at which statistical significance was found between the two groups
#' @export
#'
#' @examples mab_eg(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
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


#' Epsilion-Decreasing Multi Armed Bandit test Simulation
#'
#' @param rounds : number of rounds of testing
#' @param n : a vector of length 2 for sample size of binomial simulation
#' @param power: a value for power of the function defining the rate of epsilon-decrease (default = 3/2)
#' @param p.crit : a critival p-value for the means of two proportion test that occurs at every round
#' @param p : a vector of length 2 for binomial probability
#'
#' @return a list of 1) x_list (data.frame of values and round from samples in first group), 2) y_lisy (data.frame of values and round from samples in second group), and 3) round at which statistical significance was found between the two groups
#' @export
#'
#' @examples mab_ed(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5)
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

#' Upper Confidence Bound
#'
#' @param t : total time or total number of samples seen
#' @param n : time or number of samples seen within the test version
#' @param C : constant for the upper bound calculation (default = 1)
#'
#' @return
#' @export
#'
#' @examples
upperBound = function(t, n, C = 1) {
  sqrt(C*2*log(t)/n) 
}

#' UCB1 Multi Armed Bandit test Simulation (Optimism in the Face of Uncertainty)
#'
#' @param rounds : number of rounds of testing
#' @param n : a vector of length 2 for sample size of binomial simulation
#' @param C: constant for the upper bound calculation (default = 1)
#' @param p.crit : a critival p-value for the means of two proportion test that occurs at every round
#' @param p : a vector of length 2 for binomial probability
#'
#' @return a list of 1) x_list (data.frame of values and round from samples in first group), 2) y_lisy (data.frame of values and round from samples in second group), and 3) round at which statistical significance was found between the two groups
#'
#' @examples mab_ucb(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5)
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