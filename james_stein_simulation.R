
# for a multivariate normal distribution woth diagonal covariance matrix and paramer size p <= 3

#Simulation on James Stein Estimator
pacman:::p_load(mvtnorm, dplyr, ggplot2, tidyr)


# set parameters
mu = 50
sigma = 15
p = 5
reps = 500

# MSE function
mse = function(x, theta) mean((theta - x)^2)

# James-Stein 
js_est = function(mu, sigma, p, v = NULL){
  Mu = rep(mu, p)
  Sigma = diag(p)*sigma^2
  
  #obvious estimator for Mu is x
  x = as.vector(rmvnorm(1, Mu, Sigma))
  
  #obvious estimator for Mu is x
  that = x
  
  #James-Stein estimator
  if(!is.null(v)) {
    that_js = (1 - (p-3)*(sigma^2)/sum((x - v)^2))*(x-v) + v
  } else {
    that_js = (1 - (p-2)*(sigma^2)/sum(x^2))*x
  }
  
  return(cbind(that = that, that_js = that_js))
}

js_sim = replicate(reps, js_est(mu,sigma,p))
js_v_sim = replicate(reps, js_est(mu,sigma,p,v = 45))

means = apply(js_sim, 1:2, mean)
means_v = apply(js_v_sim, 1:2, mean)

(results = apply(js_sim, 2:3, function(x) mse(x, mu)) %>% rowMeans())
(results_v = apply(js_v_sim, 2:3, function(x) mse(x, mu)) %>% rowMeans())


pop = 100
dothis = function(){
  mu = 50
  y = rnorm(p*pop, mu, sigma); v = 50
  ybars = matrix(y, ncol = 5) %>% colMeans
  
  a = ybars
  b = (1 - (p-2)*((sigma^2)/pop)/sum(ybars^2))*ybars
  c = (1 - (p-3)*((sigma^2)/pop)/sum((ybars-v)^2))*(ybars-v) + v
  return(cbind(a,b,c))
}
dothat = replicate(1000, colMeans((dothis() - mu)^2)) %>% t() %>% data.frame()
colMeans(dothat)

names(dothat) = 1:3
dowhat = gather(dothat, "y", "x", 1:3)


plt <- ggplot(dowhat, aes(factor(y), x))
plt + geom_boxplot()





# james stein in linear regression


pop = 100
dowhat = function(pop = 100, p = 3, mu = 0, sigma = 1, v = mu){
  mu = 50
  x = rnorm(p*pop, mu, sigma); v = 50
  theta = rnorm(p)
  y = sum(x %*% theta) + rnorm(pop)
  ybars = matrix(y, ncol = 5) %>% colMeans
  
  a = ybars
  b = (1 - (p-2)*((sigma^2)/pop)/sum(ybars^2))*ybars
  c = (1 - (p-3)*((sigma^2)/pop)/sum((ybars-v)^2))*(ybars-v) + v
  return(cbind(a,b,c))
}
dothat = replicate(1000, colMeans((dothis() - mu)^2)) %>% t() %>% data.frame()
colMeans(dothat)



# simple linear regression case
Sigma = diag(p)*sigma^2

#obvious estimator for Mu is x
x = as.vector(rmvnorm(1, Mu, Sigma))

Sigma = diag(p)*sigma^2

#obvious estimator for Mu is x
x = as.vector(rmvnorm(1, Mu, Sigma))

x = rnorm(100)
y = x + rnorm(100)

# new predictions
xnew = rnorm(1)


X=cbind(1, x) 
beta.hat = qr.solve(t(X)%*%X) %*% t(X)%*%y
y.hat = sum(c(1,xnew) * beta.hat)

qr.solve(crossprod(X)) %*% crossprod(X,y)


theta_mle = 




