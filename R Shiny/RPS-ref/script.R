#https://www.psychologytoday.com/blog/the-blame-game/201504/the-surprising-psychology-rock-paper-scissors

#https://flowingdata.com/2010/07/30/how-to-win-rock-paper-scissors-every-time/
  
#http://arxiv.org/pdf/1404.5199v1.pdf

#http://rpshand.blogspot.com/2011/12/learning-algorithms.html

#rock paper scissor machine learning
choices <- c("rock", "paper", "scissors")
priors <- c(.35, .3, .35)

input <-  sample(choices, 1, replace = T, prob = priors) 
computer <- sample(choices, 1, replace = T, prob = priors) 

diff <- which(choices == input) - which(choices == computer)

result <- if(diff == 0) "tie" else
  if(diff %in% c(1, -2)) "win" else
  if(diff %in% c(-1, 2)) "lose"

out <- cbind(input, computer, result)

print(out)


#create dataframe

#convert sequence + win to dataframe


pacman::p_load(data.table, dplyr)

ch_short <- c("r","p","s")
outcome <- c("W","L","T")
seq1 <- merge(ch_short, outcome, by = NULL); names(seq1) = c("move","outcome")
dt1 <- data.table(seq1, n = 0); setkey(dt1, move, outcome)

seq2 <- merge(seq1, seq1, by = NULL, suffixes = c(".1",".2"))
dt2 <- data.table(seq2, n = 0); setkey(dt2, move.1, outcome.1, move.2, outcome.2)

seq3 <- merge(seq2, seq1, by = NULL); names(seq3)[5:6] = c("move.3", "outcome.3")
dt3 <- data.table(seq3, n = 0); setkey(dt3, move.1, outcome.1, move.2, outcome.2, move.3, outcome.3)


moves = c("r","p","s")
outcomes = c("W","T","L")


i = 1
dt1[.(moves[i], outcome[i]), n:= n+1]

i = 2
dt1[.(moves[i], outcome[i]), n:= n+1]
dt2[.(moves[i-1], outcome[i-1], moves[i], outcome[i]), n:= n+1]
  
i = 3
dt1[.(moves[i], outcome[i]), n:= n+1]
dt2[.(moves[i-1], outcome[i-1], moves[i], outcome[i]), n:= n+1]
dt3[.(moves[i-2], outcome[i-2], moves[i-1], outcome[i-1], moves[i], outcome[i]), n:= n+1]




#code to save new responses (https://daattali.com/shiny/persistent-data-storage/)
get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

#edited
save_data_dropbox <- function(data, path = "", file_name = "temp.rds") {
  data <- t(data)
  file_path <- file.path(paste(tempdir(), file_name, sep = "/"))
  saveRDS(data, file_path)
  drop_upload(file_path, dest = path)
}


#code to read all (https://daattali.com/shiny/persistent-data-storage/)
load_data_dropbox <- function(path, file_name) {
  files_info <- drop_dir(file_name)
  file_paths <- files_info$path
  data <- lapply(file_paths, drop_read_csv, stringsAsFactors = FALSE) %>% 
    do.call(rbind, .)
  data
}



  