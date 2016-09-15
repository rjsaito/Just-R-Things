pacman::p_load_gh("trinker/sentimentr")
pacman::p_load(XML, dplyr, stringr, rvest,audio, slam)

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings) {
  require(dplyr)
  strings %>%
    str_extract_all("[:alnum:]|[:space:]") %>% 
    sapply(., function(x) paste(x, collapse="")) %>%
    tolower() %>%
    removePunctuation() %>%
    stripWhitespace() %>%
    trim()  
}


source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")

#scrape data
prod_code = "B0043WCH66"

url <- paste0("https://www.amazon.com/dp/", prod_code)
doc <- read_html(url)

prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
pages <- 10

reviews_all <- NULL
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)
  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}


sent_agg <- with(reviews_all, sentiment_by(comments)) 
best_reviews <- slice(reviews_all, top_n(sent_agg, 3, ave_sentiment)$element_id)
worst_reviews <- slice(reviews_all, top_n(sent_agg, 3, -ave_sentiment)$element_id)


with(best_reviews, sentiment_by(comments)) %>% highlight()
with(worst_reviews, sentiment_by(comments)) %>% highlight()





reviews_sentences = get_sentences(reviews_all$comments) %>% unlist()


prod_words <- prod %>% strsplit(., " ") %>% unlist() %>% removePunctuation() %>% tolower()

reviews_clean <- reviews_sub %>%
  str_clean() %>%
  removeWords(removePunctuation(stopwords('english'))) %>%
  removeWords(prod_words) 

#sentiment scores
reviews_score <- sentiment_by(reviews_clean)$ave_sentiment
names(reviews_score) = 1:length(reviews_score)

reviews_tm <- VectorSource(reviews_clean) %>% Corpus()  %>% TermDocumentMatrix()
word_score <- with(reviews_tm, data.frame(doc = j, word_ind = i)) %>%
  merge(data.frame(reviews_score), by.x = "doc", by.y = "row.names") %>%
  group_by(word_ind) %>%
  summarise(avg_sc = mean(reviews_score)) %>%
  merge(data.frame(word = reviews_tm$dimnames$Terms, stringsAsFactors = F), by.x = "word_ind", by.y = "row.names")


wss <- round(word_score$avg_sc*2) 
word_color <- diverge_hcl(diff(range(wss))+1, h = c(0, 120), c = 260)[wss-min(wss)+1]

freq <- row_sums(reviews_tm)
words <- names(freq)

wordcloud(words, freq, colors = word_color, max.words = 50)  



sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(words[1:100], list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(words[1:100], pos_tag_annotator, a2)
annotations_in_spans(a3)

