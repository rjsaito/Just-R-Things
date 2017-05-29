### Installing/Loading Packages at Once

#ipak - https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#can save as as source function or in R profile (automatically load on start up)
pacman::p_load(dplyr, tidyr, magrittr, data.table)
#####################################################################################

