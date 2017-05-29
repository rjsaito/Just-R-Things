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
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  data.frame(word = s[a3w], POStags = POStags, stringsAsFactors = F)
}



###########################################################################################################
reviews = read.csv("Z:/AMR_Intern Riki/amazon reviews/amazon_reviews_jaybird_x2.csv", stringsAsFactors = F)

prod = unique(reviews$prod)
reviews_lines = get_sentences2(reviews$comments) %>% unlist()




# table 1: review/sentence sentiment
revs <- reviews$comments %>% str_clean()
sentences <-  get_sentences(gsub("(\\s*)([;:,]+)", " \\2", evs)) %>% unlist()
revs_sentiment <- sentiment(revs)



# table 2: word frequency and average sentiment
#term document matrix of reviews
reviews_tm <- VectorSource(revs) %>% Corpus()  %>% TermDocumentMatrix()

#word scores
wordscore <- with(reviews_tm, data.frame(doc = j, word_ind = i)) %>%
  merge(data.frame(revs_sentiment), by.x = "doc", by.y = "element_id") %>%
  group_by(word_ind) %>%
  summarise(avg_sent = mean(sentiment)) %>%
  merge(data.frame(word = reviews_tm$dimnames$Terms, stringsAsFactors = F), by.x = "word_ind", by.y = "row.names")

#word frequency
wordfreq <-  row_sums(reviews_tm) %>% data.frame(freq = ., word = names(.), stringsAsFactors = F)
row.names(wordfreq) = NULL

#part of speech
wordPOS <- tagPOS(wordfreq$word)

#data frame of word sentiments
wordsent_df <- merge(wordfreq, wordPOS, by = "word") %>% merge(., wordscore, by = "word") %>% arrange(-freq)









#sentiment scores
#change "loud"
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

#data frame of word sentiments
wordsent_df <- merge(wordfreq, wordPOS, by = "word") %>% merge(., wordscore, by = "word") %>% arrange(-freq)






#for any given word
wc_df <- subset(wordsent_df, POStags %in% c("CC", "NNS", "NN", "JJ"))

#color
wss <- round(wordscore$avg_sc*2) 
wordscore$color <- diverge_hcl(max(abs(wss))*2+1, h = c(0, 120), c = 260)[wss+max(abs(wss))+1] 
  
#data
with(wc_df, wordcloud(word, freq, colors = color, ordered.colors = T, max.words = 50))


token = "battery"
featrevs = grep(token, reviews_clean) 
reviews_clean[featrevs] %>% sentiment_by() %>% highlight(., original.text = removePunctuation(reviews_lines[featrevs]))

subset(wordsent_df, word == token)




