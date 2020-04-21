# Riki Saito
# Multivariate Adstock Model


# load packages
pacman::p_load(minpack.lm)


set.seed(2222)

# simulate data
adstock <- function(x,rate=0){
  return(as.numeric(stats::filter(x=x,filter=rate,method="recursive")))
}

# adstock with block variable
adstock_block <- function(x, block, rate = 0){
  block_uniq = unique(block)
  adstock = sapply(block_uniq, function(y) {
    z = x[block == y]
    as.numeric(stats::filter(x=z,filter=rate,method="recursive"))
  })
  return(as.vector(adstock))
}


n_weeks = 104

base = 50
ad1 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))
ad2 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))
ad3 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))

ad1_rate = .7
ad2_rate = .4
ad3_rate = .5

sales = round(base + adstock(ad1, ad1_rate) + adstock(ad2, ad2_rate) + adstock(ad3, ad3_rate) + rnorm(n_weeks, sd = 5), 0)



# https://www.linkedin.com/pulse/function-i-wrote-r-derive-optimal-adstock-rate-from-data-angela-ju/


AdstockRateMV <- function(Impact, Ads, Block = NULL, maxiter = 100, scaled = F, rate_min = 0, rate_max = 1, print_plot = T){
  # parameter names
  params = letters[2:(ncol(Ads)+1)]
  # ad variable names
  ads = paste0("ad_", params)
  # rate variable names
  rates = paste0("rate_", params)
  # create partial formula
  if(all(!is.na(Block))){
    param_fm = paste(
      paste(params, "*adstock_block(", ads,  ",Block,", rates, ")", sep = ""),
      collapse = " + "
    )    
  } else {
    param_fm = paste(
      paste(params, "*adstock(", ads,  ",", rates, ")", sep = ""),
      collapse = " + "
    )
  }
  # create whole formula
  fm = as.formula(paste("Impact ~ a +", param_fm))
  # starting values for nls
  start = c(rep(1, length(params) + 1), rep(rate_min, length(rates)))
  names(start) = c("a", params, rates)
  # create input data 
  Ads_df = Ads
  names(Ads_df) = ads
  Data = cbind(Impact, Ads_df)
  # scale
  if(scaled){
    sc = scale(Data)
    Data = data.frame(sc)
  }
  # fit model
  modFit <- nls(data = Data, fm, start = start, control = nls.control(maxiter = maxiter, warnOnly = T))
  # if all decay rates greater than 0, done. If not, use a constrained nls model (with lower and upper parameter limits)
  if(!all(summary(modFit)$coefficients[rates, 1] > rate_min) |
     !all(summary(modFit)$coefficients[rates, 1] < rate_max)){
    library(minpack.lm)
    lower = c(rep(-Inf, length(params) + 1), rep(rate_min, length(rates)))
    upper = c(rep(Inf, length(params) + 1), rep(rate_max, length(rates)))
    modFit <- nlsLM(fm, data = Data, start = start,
                   lower = lower, upper = upper,
                   control = nls.lm.control(maxiter = maxiter)) 
  }
  # model coefficients
  AdstockInt = summary(modFit)$coefficients[1, 1]
  AdstockCoef = summary(modFit)$coefficients[params, 1]
  AdstockRate = summary(modFit)$coefficients[rates, 1]
  # print formula with coefficients
  param_fm_coefs = paste(
    paste(round(AdstockCoef, 2), " * adstock(", names(Ads),  ", ", round(AdstockRate, 2), ")", sep = ""),
    collapse = " + "
  )
  fm_coefs = as.formula(paste("Impact ~ ", round(AdstockInt, 2), " +", param_fm_coefs))
  # rename coef and rates with original variable names
  names(AdstockCoef) = paste0("coef_", names(Ads))
  names(AdstockRate) = paste0("rate_", names(Ads))
  # calculate percent error
  if(scaled) warning("MAPE on scaled data may not be an appropriate performance measure.")
  mape = mean(abs((Data$Impact-predict(modFit))/Data$Impact) * 100)
  # goodness of fit by visual inspection
  if(print_plot) print(plot(Data$Impact, predict(modFit), xlab = "Observed", ylab = "Predicted", main = "Observed vs. Predicted Impact"))
  # model summary table
  modSum = summary(modFit)$coef
  row.names(modSum) = c("(Intercept)", paste0("coef_", names(Ads)), paste0("rate_", names(Ads)))
  # return outputs
  return(list(fm = fm_coefs, coef = AdstockCoef, rates = AdstockRate, mape = mape, summary = modSum))
}



# individually
Impact = sales
AdstockRateMV(Impact, data.frame(ad1))$rates
AdstockRateMV(Impact, data.frame(ad2))$rates
AdstockRateMV(Impact, data.frame(ad3))$rates

# together
Ads = data.frame(ad1, ad2, ad3 )
AdstockRateMV(Impact, Ads)




# simulation
adstock_sim = function(){
  base = 50
  ad1 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))
  ad2 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))
  ad3 = sapply(rnorm(n_weeks, mean = 20, sd = 10), function(x) round(max(x, 0), 0))
  
  ad1_rate = .7
  ad2_rate = .4
  ad3_rate = .5
  
  sales = round(base + adstock(ad1, ad1_rate) + adstock(ad2, ad2_rate) + adstock(ad3, ad3_rate) + rnorm(n_weeks, sd = 5), 0)
  
  
  Impact = sales
  Ads = data.frame(ad1, ad2, ad3 )
  mod = AdstockRateMV(Impact, Ads)
  return(c(mod[[2]], mod[[3]]))
  
}



mod_rep = replicate(n = 100, adstock_sim())
rowMeans(mod_rep)
