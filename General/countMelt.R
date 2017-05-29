
# count data to long binary
countMelt = function(df, n, x){
  require(data.table, foreach, iterators)
  dt = data.table(df)
  melt = foreach(row = iter(dt, by = 'row'), .combine = rbind) %do%{
    N = row[,n, with = F]
    X = row[,x, with = F]
    y = rep(c(1,0), c(X, N-X))
    dt_out = data.table(row[,!c(eval(n),eval(x)), with = F], y)
    dt_out
  }
  return(melt)
}
