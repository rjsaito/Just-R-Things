#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings) {
  require(dplyr)
  strings %>%
    tolower() %>%
    removePunctuation() %>%
    stripWhitespace() %>%
    trim()  
}

#Convert roman numerals to integers (except for roman numeral I)
roman.to.int <- function(x){
  xs <- strsplit(x, " ")[[1]]
  ind <- which(suppressWarnings(!is.na(as.roman(xs))) & tolower(xs) != "i")
  newx <- paste(replace(xs, ind, as.roman(xs[ind])), collapse = " ")
  return(newx)
}