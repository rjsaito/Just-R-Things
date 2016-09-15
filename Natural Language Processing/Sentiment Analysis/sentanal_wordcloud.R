pacman::p_load_gh("trinker/sentimentr")
pacman::p_load(dplyr, stringr, wordcloud, tm, colorspace, slam, gsubfn, foreach, audio, openNLP)


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

#part of speech tagger
tagPOS <-  function(x, ...) {
  require(openNLP)
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  data.frame(word = s[a3w], POStags = POStags, stringsAsFactors = F)
}


###########################################################################################################
reviews = read.csv("Z:/AMR_Intern Riki/amazon reviews/amazon_reviews_ue_roll.csv", stringsAsFactors = F)

prod = unique(reviews$prod)
reviews_lines = get_sentences(reviews$comments) %>% unlist()


prod_words <- prod %>% strsplit(., " ") %>% unlist() %>% removePunctuation() %>% tolower()

reviews_clean <- reviews_lines %>%
  str_clean() %>%
  removeWords(removePunctuation(stopwords('english'))) %>%
  removeWords(prod_words) 

#sentiment scores
reviews_sentiment <- sentiment_by(reviews_clean)
reviews_score <- reviews_sentiment$ave_sentiment
names(reviews_score) = 1:length(reviews_score)

#term document matrix of reviews
reviews_tm <- VectorSource(reviews_clean) %>% Corpus()  %>% TermDocumentMatrix()

#word scores
wordscore <- with(reviews_tm, data.frame(doc = j, word_ind = i)) %>%
  merge(data.frame(reviews_score), by.x = "doc", by.y = "row.names") %>%
  group_by(word_ind) %>%
  summarise(avg_sc = mean(reviews_score)) %>%
  merge(data.frame(word = reviews_tm$dimnames$Terms, stringsAsFactors = F), by.x = "word_ind", by.y = "row.names")

#word frequency
wordfreq <-  row_sums(reviews_tm) %>% data.frame(freq = ., word = names(.), stringsAsFactors = F)
row.names(wordfreq) = NULL

#part of speech
wordPOS <- tagPOS(wordfreq$word)

#color
wss <- round(wordscore$avg_sc*2) 
wordscore$color <- diverge_hcl(max(abs(wss))*2+1, h = c(0, 120), c = 260)[wss+max(abs(wss))+1] 
  
#data frame of word sentiments
wordsent_df <- merge(wordfreq, wordPOS, by = "word") %>% merge(., wordscore, by = "word") %>% arrange(-freq)

wordsentiment <- sentiment(wordPOS$word) %>% cbind(wordPOS, .)


#which ones are negation of adjective/verb?

reviewPOS <- get_sentences(reviews_clean) %>% tagPOS()


#for any given word
wc_df <- subset(wordsent_df, POStags %in% c("CC", "NNS", "NN", "JJ"))

with(wc_df, wordcloud(word, freq, colors = color, ordered.colors = T, max.words = 50))


token = "battery"
featrevs = grep(token, reviews_clean) 
reviews_clean[featrevs] %>% sentiment_by() %>% highlight(., original.text = removePunctuation(reviews_lines[featrevs]))

subset(wordsent_df, word == token)

