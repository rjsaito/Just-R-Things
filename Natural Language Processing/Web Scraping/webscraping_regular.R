source("C:/Users/rsaito/Dropbox/Data Science/Just-R-Things/Natural Language Processing/Sentiment Analysis/amazonscraper.R")


prod_code = "B01EHIQAUE"
pages <- 22

prod <- paste0("https://www.amazon.com/dp/", prod_code) %>%
  read_html() %>%
  html_nodes("#productTitle") %>%
  html_text() %>%
  gsub("\n", "", .) %>%
  trim()


reviews_all <- NULL
for(page_num in 1:pages){
  # url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/ref=cm_cr_pr_top_link_",page,"?ie=UTF8&pageNumber=",page,"&showViewpoints=0&sortBy=byRankDescending")
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  
  reviews <- amazon_scraper(url, reviewer = F)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
  wait(2)
}


write.csv(reviews_all, file.choose(new = T))