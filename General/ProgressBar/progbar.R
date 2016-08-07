pacman::p_load(foreach)

trials <- 200
n <- 100000

len <- trials
pb <- winProgressBar(title = "Begin Loop" , min = 0, max = len, width = 300)

out <- foreach(i = seq_len(trials), .combine = rbind) %do% {
  x <- rnorm(n)
  y <- sample(0:1, n, replace=T)
  fit <- glm(y~x, family=binomial(logit))
  coefs <- coef(fit)
  
  Sys.sleep(0.1); setWinProgressBar(pb, i, title=paste("Trial:", i, "out of", len, "done"))
  return(coefs)
}
     