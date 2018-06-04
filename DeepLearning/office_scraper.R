# Riki Saito rjsaito@gmail.com
# Scraper for The Office quotes

pacman::p_load(rvest, stringr)


the_office_scraper <- function(doc, delay = 0){
  
  if(!"pacman" %in% installed.packages()) install.packages("pacman")
  pacman::p_load_gh("trinker/sentimentr")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) {
    warning("delay was less than 0: set to 0")
  } else if(delay > 0) {
    sec = max(0, delay + runif(1, -1, 1))
  }
  
  print("Sleeping...")
  Sys.sleep(sec)
  print(paste0("Scraper is awake. Sleep time is ", round(sec, 2), " seconds."))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  # scrape
  strings = doc %>%
    html_nodes(".quote") %>%
    html_text() %>%
    str_replace_all("\t|\r|\n|&nbsp", "") %>%
    str_subset("^(?!Deleted Scene)")
  
  return(strings)
}

# get all quotes
season_episode_df = data.frame(
  season = 1:9,
  episodes = c(6, 22, 23, 14, 26, 24, 24, 24, 23)
)

season_episodes = apply(season_episode_df, 1, function(x)
  data.frame(season = x[1], episodes = 1:x[2])
) %>%
  do.call("rbind", .)

n = nrow(season_episodes)

quote_list = list()
for(i in 1:n){
  
  season = season_episodes$season[i]
  episode = str_pad(season_episodes$episodes[i],  side = "left", width = 2, pad = "0")
  
  url <- paste0("officequotes.net/no", season, "-", episode, ".php")
  doc = html_session(url)
  
  quotes = the_office_scraper(doc, delay = 1)
  
  quote_list[[i]] = quotes
  
  print(paste0(i, " out of ", n, " done."))
  
}

quote_vec = unlist(quote_list)

# parse out everyone's quote
string_to_quote <- function(string){
  pacman::p_load(stringr)
  loc = str_locate_all(string, "\\.[[:alpha:]]+:|\\?[[:alpha:]]+:|\\![[:alpha:]]+:|\\S[[:alpha:]]+:|\\\"[[:alpha:]]+:|\\-[[:alpha:]]+:")[[1]]
  n = nrow(loc) + 1
  end = nchar(string)
  df = NULL
  for(i in n:1){
    start = ifelse(i == 1, 1, loc[(i-1), 1] + 1)
    split_string = str_sub(string, start, end) %>%
      str_split(":", n = 2) %>%
      unlist() %>%
      str_replace_all("\\[.+\\]", "") %>%
      trimws() 
    df = rbind(df, split_string)
    end = start - 1
  }
  if(ncol(df) != 2) return(data.frame(Name = NULL, Qoute = NULL))
  row.names(df) = NULL
  df = data.frame(df, stringsAsFactors = F)
  names(df) = c("Name", "Quote")
  return(df)
}

quote_vec_sub = str_subset(quote_vec, ".+")

# parse
quote_clean_df = lapply(quote_vec_sub, string_to_quote) %>%
  do.call("rbind", .)
