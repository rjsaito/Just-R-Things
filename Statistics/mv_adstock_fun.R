#multivariate adstock function
AdstockRateMV <- function(Impact, Ads, maxiter = 100){
  # parameter names
  params = letters[2:(ncol(Ads)+1)]
  # ad variable names
  ads = paste0("ad_", params)
  # rate variable names
  rates = paste0("rate_", params)
  # create partial formula
  param_fm = paste(
    paste(params, "*adstock(", ads,  ",", rates, ")", sep = ""),
    collapse = " + "
  )
  # create whole formula
  fm = as.formula(paste("Impact ~ a +", param_fm))
  # starting values for nls
  start = c(rep(1, length(params) + 1), rep(.1, length(rates)))
  names(start) = c("a", params, rates)
  # input data
  Ads_df = Ads
  names(Ads_df) = ads
  Data = cbind(Impact, Ads_df)
  # fit model
  modFit <- nls(data = Data, fm, start = start, control = nls.control(maxiter = maxiter, warnOnly = T))
  # if all decay rates greater than 0, done. If not, use a constrained nls model (with lower and upper parameter limits)
  if(!all(summary(modFit)$coefficients[rates, 1] > 0)){
    library(minpack.lm)
    lower = c(rep(-Inf, length(params) + 1), rep(0, length(rates)))
    upper = c(rep(Inf, length(params) + 1), rep(1, length(rates)))
    modFit <- nlsLM(fm, data = Data, start = start,
                   lower = lower, upper = upper,
                   control = nls.lm.control(maxiter = maxiter)) 
  }
  # model coefficients
  AdstockInt = round(summary(modFit)$coefficients[1, 1])
  AdstockCoef = round(summary(modFit)$coefficients[params, 1], 2)
  AdstockRate = round(summary(modFit)$coefficients[rates, 1], 2)
  # print formula with coefficients
  param_fm_coefs = paste(
    paste(round(AdstockCoef, 2), " * adstock(", names(Ads),  ", ", round(AdstockRate, 2), ")", sep = ""),
    collapse = " + "
  )
  fm_coefs = as.formula(paste("Impact ~ ", AdstockInt, " +", param_fm_coefs))
  # rename rates with original variable names
  names(AdstockRate) = paste0("rate_", names(Ads))
  # calculate percent error
  mape = mean(abs((Impact-predict(modFit))/Impact) * 100)
  # return outputs
  return(list(fm = fm_coefs, base = AdstockInt, rates = AdstockRate, mape = mape))
}
