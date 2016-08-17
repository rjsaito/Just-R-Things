#first, download phantomjs from http://phantomjs.org/download.html and copy phantomjs.exe onto path, obtained with getwd()
pacman::p_load(RSelenium)
pJS <- phantom()
startServer()

eCap <- list(phantomjs.page.settings.loadImages = FALSE)
remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap)
remDr$open(silent = T)




# ueboom2 = B014M8ZO8S
# megaboom = B00TXBOWVA
# ueroll = B00YARCGQA

#######################
prod_code = "B00YARCGQA"
########################


#load produce page in browser
remDr$navigate(paste0("https://www.amazon.com/dp/", prod_code))
prod_page <-  remDr$getPageSource()[[1]] %>% read_html()

prod <- html_nodes(prod_page, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
pages <- ceiling(html_nodes(prod_page, "#acrCustomerReviewText") %>% html_text() %>% .[1] %>% gsub(" customer reviews|,", "", .) %>% as.numeric()/10)


reviews_all <- NULL
for(page_num in 1:pages){
  
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  
  tryCatch(remDr$navigate(url), error = function(e) {
    remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap); 
    remDr$open(silent = T)
    remDr$navigate(url)
  })
  
  doc <- remDr$getPageSource()[[1]] %>% read_html()

  reviews <- amazon_scraper(doc, reviewer = F, delay = 1)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}
  

#remDr$screenshot(display = TRUE)


