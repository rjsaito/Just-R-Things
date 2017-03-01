pacman::p_load(doParallel, foreach, dplyr, tidyr, ggplot2)

cores <- detectCores()
cl <- makeCluster(cores[1])
registerDoParallel(cl)

trials <- c(1:2, 5, 10, 15, 20, 30, 40, 50, 100, 150, 200)
N <- c(10000, 50000, seq(100000, 500000, by = 100000), 1000000)


proc_time <- 
  foreach(trial = trials, .combine = rbind) %do% {
    foreach(n = N, .combine = rbind) %do% {
      
      stime <- system.time({
        foreach(icount(trial), .combine = rbind) %do% {
          x <- rnorm(n)
          y <- sample(0:1, n, replace=T)
          fit <- glm(y~x, family=binomial(logit))
          coef(fit)
        }
      })[3]
      
      ptime <- system.time({
        foreach(icount(trial), .combine = rbind) %dopar% {
          x <- rnorm(n)
          y <- sample(0:1, n, replace=T)
          fit <- glm(y~x, family=binomial(logit))
          coef(fit)
        }
      })[3]
      data.frame(trial, n, stime, ptime)
    }
  }

proc_time %>%
  gather("proc","seconds", 3:4) %>%
  ggplot(aes(x = n, y = time)) + geom_line(aes(col = proc)) + geom_point(aes(col = proc)) + facet_wrap( ~ trial, ncol = 3, scales = "free_y") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(proc_time, ".../proc_time.rds")
  
