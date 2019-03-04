## Rerelease

# Fix scraper
# Add better sentiment analysis
# Add word embedding analysis
# Add topic modeling analysis


# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, stringr)

# Venom product code
prod_code <- "B07HSKPDBV"

url <- paste0("https://www.amazon.com/dp/", prod_code)
doc <- read_html(url)

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  trimws()

prod


# Function to scrape elements from Amazon reviews
scrape_amazon <- function(url, throttle = 0){
  
  # Install / Load relevant packages
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr)
  
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  # obtain HTML of URL
  doc <- read_html(url)
  
  # Parse relevant elements from HTML
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  review_format <- doc %>% 
    html_nodes(".review-format-strip") %>% 
    html_text() 
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric() 
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  suppressWarnings(n_helpful <- doc %>%
    html_nodes(".a-expander-inline-container") %>%
    html_text() %>%
    gsub("\n\n \\s*|found this helpful.*", "", .) %>%
    gsub("One", "1", .) %>%
    map_chr(~ str_split(string = .x, pattern = " ")[[1]][1]) %>%
    as.numeric())
  
  # Combine attributes into a single data frame
  df <- data.frame(title, author, date, review_format, stars, comments, n_helpful, stringsAsFactors = F)
  
  return(df)
}

# Set # of pages to scrape. Note: each page contains 8 reviews.
pages <- 200

# create empty object to write data into
reviews_all <- NULL

# loop over pages
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

write.csv(reviews_all, "NLP/sample_reviews_venom.csv")
reviews_all = read.csv("NLP/sample_reviews_venom.csv", stringsAsFactors = F)




## Sentiment Analysis

# create ID for reviews
review_df <- reviews_all %>%
  mutate(id = row_number())

# using sentimentr, get sentiment
# lexicon-based sentiment analysis
pacman::p_load(sentimentr, ggplot2)

sent_df <- review_df %>%
  get_sentences() %>%
  sentiment_by(by = c('id', 'author', 'date', 'stars', 'review_format'))

# stars vs sentiment
boxplot(ave_sentiment ~ stars,data = sent_df, main="Avg. Sentiment by Star Rating", 
        xlab="Star Rating", ylab="Average Sentiment")

p <- ggplot(sent_df, aes(x = stars, y = ave_sentiment, color = factor(stars), group = stars)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(5.2, -0.05, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +
  guides(color = guide_legend(title="Star Rating")) +
  ylab("Avg. Sentiment") +
  xlab("Review Star Rating") +
  ggtitle("Sentiment of Venom Amazon Reviews, by Star Rating") 
  
p

# sentiment over time
sent_ts <- sent_df %>%
  mutate(
    date = as.Date(date, format = "%B %d, %Y"),
    dir = sign(ave_sentiment)
  ) %>%
  group_by(date) %>%
  summarise(
    avg_sent = mean(ave_sentiment)
  ) %>%
  ungroup()

# plot
ggplot(sent_ts, aes(x = date, y = avg_sent)) +
  geom_smooth(method="loess", size=1, se=T, span = .6) +
  geom_vline(xintercept=as.Date("2018-10-05"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2018-10-05") + 1, .35, label = "Theatrical Release", hjust = "left"), size = 3, color = "black") +
  
  geom_vline(xintercept=as.Date("2018-12-11"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2018-12-11") + 1, .35, label = "Digital Release", hjust = "left"), size = 3, color = "black") +
  
  geom_vline(xintercept=as.Date("2018-12-18"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2018-12-18") + 1, .33, label = "DVD / Blu-Ray Release", hjust = "left"), size = 3, color = "black") +
  
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(max(sent_ts$date) - 5, -0.02, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +
  ylab("Avg. Sentiment") +
  xlab("Review Date") +
  ggtitle("Sentiment of Venom Amazon Reviews, Over Time")





## Word Embeddings

# Since deep learning based word embedding is computationally expensive, use glove instead

pacman::p_load(text2vec, tm, ggplot2, ggrepel, plotly)

#In the next step we will create a vocabulary, a set of words for which we want to learn word vectors. Note, that all of text2vec's functions which operate on raw text data (create_vocabulary, create_corpus, create_dtm, create_tcm) have a streaming API and you should iterate over tokens as the first argument for these functions.

# Create iterator over tokens
tokens <- space_tokenizer(reviews_all$comments %>%
                            tolower() %>%
                            removePunctuation())

# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

#These words should not be too uncommon. Fot example we cannot calculate a meaningful word vector for a word which we saw only once in the entire corpus. Here we will take only words which appear at least five times. text2vec provides additional options to filter vocabulary (see ?prune_vocabulary).

vocab <- prune_vocabulary(vocab, term_count_min = 25L)

#Now we have 71,290 terms in the vocabulary and are ready to construct term-co-occurence matrix (TCM).

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)


# use window of 50 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

#Now we have a TCM matrix and can factorize it via the GloVe algorithm.
#text2vec uses a parallel stochastic gradient descent algorithm. By default it will use all cores on your machine, but you can specify the number of cores if you wish. For example, to use 4 threads call RcppParallel::setThreadOptions(numThreads = 4).

#Let's fit our model. (It can take several minutes to fit!)

glove = GloVe$new(word_vectors_size = 100, vocabulary = vocab, x_max = 20)
glove$fit_transform(tcm, n_iter = 100)

#And now we get the word vectors:
  
word_vectors <- glove$components
#We can find the closest word vectors for our paris - france + germany example:
  
# symbiote <- word_vectors[, "awesome", drop = F] 
# 
# cos_sim = sim2(x = t(word_vectors), y = t(symbiote), method = "cosine", norm = "l2")
# head(sort(cos_sim[,1], decreasing = TRUE), 10)
# 
# # berlin     paris    munich    leipzig   germany 
# # 0.8015347 0.7623165 0.7013252 0.6616945 0.6540700 
# #You can achieve much better results by experimenting with skip_grams_window and the parameters of the GloVe class (including word vectors size and the number of iterations). For more details and large-scale experiments on wikipedia data see this old post on my blog.


# plot

# remove some words

pacman::p_load(tm, Rtsne, tibble, tidytext, scales)

context_words <- data.frame(
  word = c("movie", "venom", "film", "story", "it's", "movies"),
  stringsAsFactors = F
)

keep_words <- setdiff(colnames(word_vectors), union(stopwords(), context_words$word) )

word_vec <- word_vectors[, keep_words]

# distance matrix
cos_sim = sim2(x = t(word_vectors), y = t(word_vectors), method = "cosine", norm = "l2")


# use tsne for visualization
train_df <- data.frame(t(word_vectors)) %>%
  rownames_to_column("word")

tsne <- Rtsne(train_df[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)


# visualizing
colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=train_df$word, col=colors[train_df$word])

plot_df = data.frame(tsne$Y) %>%
  mutate(
    word = train_df$word,
    col=colors[train_df$word]
  )

p <- ggplot(plot_df, aes(X1, X2)) +
  geom_text(aes(X1, X2, label = word, color = col), size = 3) +
  xlab("") + ylab("") +
  theme(legend.position = "none") 
ggplotly(p)



## add word-level sentiment?
word_sent <- review_df %>%
  left_join(sent_df, by = "id") %>%
  select(id, comments, ave_sentiment) %>%
  unnest_tokens(word, comments) %>%
  group_by(word) %>%
  summarise(
    count = n(),
    avg_sentiment = mean(ave_sentiment),
    sum_sentiment = sum(ave_sentiment),
    sd_sentiment = sd(ave_sentiment)
  ) %>%
  anti_join(stop_words) 


pd_sent <- plot_df %>%
  left_join(word_sent, by = "word") %>%
  drop_na() %>%
  filter(count >= 20)


p <- ggplot(pd_sent, aes(X1, X2)) +
  geom_point(aes(X1, X2, size = count, alpha = .01, color = avg_sentiment)) +
  geom_text(aes(X1, X2, label = word), size = 2) +
  scale_colour_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"), midpoint = 0) +
  scale_size(range = c(5, 20))
  
ggplotly(p)




## Topic Modeling

# https://www.tidytextmining.com/topicmodeling.html


pacman::p_load(tm, topicmodels, tidytext, ldatuning)

# other words to remove
context_words <- data.frame(
  word = c("movie", "venom", "film", "story", "it's", "movies"),
  stringsAsFactors = F
)

# split into words
by_review_word <- reviews_all %>%
  mutate(id = 1:nrow(.)) %>%
  unnest_tokens(word, comments)

# find document-word counts
word_counts <- by_review_word %>%
  anti_join(stop_words) %>%
  anti_join(context_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

dtm <- word_counts %>%
  cast_dtm(id, word, n)


# create document term matrix
#my_corpus <- Corpus(VectorSource(reviews_all$comments))
#dtm <- DocumentTermMatrix(my_corpus)

# find topics
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)
# 5 or 14?


# set a seed so that the output of the model is predictable
my_lda <- topicmodels::LDA(dtm, k = 8, control = list(seed = 1234))
my_lda

# from tidy text, run
ap_topics <- tidy(my_lda, matrix = "beta")
ap_topics

pd <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(order = row_number()) 

  #mutate(term = reorder(term, beta)) %>%
ggplot(pd, aes(order, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  # Add categories to axis
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$term,
    expand = c(0,0)
  ) +
  coord_flip()


pd_sent_topic <- pd_sent %>%
  left_join(ap_topics, by = c("word" = "term")) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

p <- ggplot() +
  geom_point(data = pd_sent_topic, aes(X1, X2, size = count, alpha = .01, color = factor(topic))) +
  geom_text(data = pd_sent_topic, aes(X1, X2, label = word), size = 2) +
  scale_size(range = c(5, 20))

ggplotly(p)

