

# grab reviews
reviews_all = read.csv("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/NLP/sample_reviews_venom.csv", stringsAsFactors = F)

## Sentiment Analysis

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, data.table, sentimentr, ggplot2)

# create ID for reviews
review_df <- reviews_all %>%
  mutate(id = row_number())


# example lexicon
head(lexicon::hash_sentiment_jockers_rinker)

# words to replace
replace_in_lexicon <- tribble(
  ~x, ~y,
  "marvel", 0,  # original score: .75
  "venom", 0,   # original score: -.75
  "alien", 0,   # original score: -.6
  "dark", 0,    # original score: -.6
  "bad@$$", .4, # not in dictionary
  "loser", 0,   # original score: -.5
  "carnage", 0, # original score: 0.75
  "villain", 0, # original score: -1
  "avenger", 0, # original score: .25
  "riot", 0     # original score: -.5
)

#  need to change sentiment
venom_lexicon <- lexicon::hash_sentiment_jockers_rinker %>%
  filter(!x %in% replace_in_lexicon$x) %>%
  bind_rows(replace_in_lexicon) %>%
  setDT() %>%
  setkey("x")


# using sentimentr, get sentiment
# lexicon-based sentiment analysis
sent_df <- review_df %>%
  get_sentences() %>%
  sentiment_by(by = c('id', 'author', 'date', 'stars', 'review_format'), polarity_dt = venom_lexicon)

# stars vs sentiment
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

# two familes ofr learning  word vectors

# 1) global matrix factrorization (latent semantic analysis)
# 2) skip gram model

# https://nlp.stanford.edu/pubs/glove.pdf


pacman::p_load(text2vec, tm, ggrepel)

# create lists of reviews split into individual words (iterator over tokens)
tokens <- space_tokenizer(reviews_all$comments %>%
                            tolower() %>%
                            removePunctuation())

# Create vocabulary. Terms will be unigrams (simple words).
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

#These words should not be too uncommon. Fot example we cannot calculate a meaningful word vector for a word which we saw only once in the entire corpus. Here we will take only words which appear at least five times. text2vec provides additional options to filter vocabulary (see ?prune_vocabulary).

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

str(vocab)
#Now we have 598 terms in the vocabulary and are ready to construct term-co-occurence matrix (TCM).

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)

# use window of 50 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 50L)

#Now we have a TCM matrix and can factorize it via the GloVe algorithm.
#text2vec uses a parallel stochastic gradient descent algorithm.

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

keep_words <- setdiff(colnames(word_vectors), stopwords())

word_vec <- word_vectors[, keep_words]

# use tsne for visualization
train_df <- data.frame(t(word_vec)) %>%
  rownames_to_column("word")

tsne <- Rtsne(train_df[,-1], dims = 2, perplexity = 50, verbose=TRUE, max_iter = 500)


# visualizing
colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
  mutate(
    word = train_df$word,
    col = colors[train_df$word]
  ) 

p <- ggplot(plot_df, aes(X1, X2)) +
  geom_text(aes(X1, X2, label = word, color = col), size = 3) +
  xlab("") + ylab("") +
  theme(legend.position = "none") 
p



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
  anti_join(stop_words, by = "word") 


pd_sent <- plot_df %>%
  left_join(word_sent, by = "word") %>%
  drop_na() %>%
  filter(count >= 5)


p <- ggplot(pd_sent, aes(X1, X2)) +
  geom_point(aes(X1, X2, size = count, alpha = .1, color = avg_sentiment)) +
  geom_text(aes(X1, X2, label = word), size = 2) +
  scale_colour_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"), midpoint = 0) +
  scale_size(range = c(5, 20)) +
  xlab("") + ylab("") +
  ggtitle("2-dimensional t-SNE Mapping of Word Vectors") +
  guides(color = guide_legend(title="Avg. Sentiment"), size = guide_legend(title = "Frequency"), alpha = NULL) +
  scale_alpha(range = c(1, 1), guide = "none")
p





## Topic Modeling

# https://www.tidytextmining.com/topicmodeling.html


pacman::p_load(tm, topicmodels, tidytext, ldatuning)

# remove words that show up in more than 5% of documents
frequent_words <- vocab %>%
  filter(doc_count >= nrow(review_df) * .01) %>%
  rename(word = term) %>%
  select(word)

# split into words
by_review_word <- reviews_all %>%
  mutate(id = 1:nrow(.)) %>%
  unnest_tokens(word, comments)

# find document-word counts
word_counts <- by_review_word %>%
  anti_join(stop_words, by = "word") %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

dtm <- word_counts %>%
  cast_dtm(id, word, n)

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


# http://rpubs.com/siri/ldatuning
FindTopicsNumber_plot(result)
# 6


# set a seed so that the output of the model is predictable
my_lda <- topicmodels::LDA(dtm, k = 6, method = "Gibbs")
my_lda

# from tidy text, run
ap_topics <- tidy(my_lda, matrix = "beta")
ap_topics

pd <- ap_topics %>%
  # categorize words into single topic by beta
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(order = row_number()) 

# plot
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

# label topics
topics <- tribble(
  ~topic,  ~desc,
  1, "Symbrock, Symbiotes",
  2, "Story,  Comic",
  3, "Positive Reception",
  4, "Negative Reception",
  5, "Venom",
  6, "Tom Hardy"
)

pd_sent_topic <- pd_sent %>%
  left_join(ap_topics, by = c("word" = "term")) %>%
  filter(beta > .005) %>%
  left_join(topics, by = "topic") %>%
  ungroup()


p <- ggplot() +
  geom_point(data = pd_sent_topic, aes(X1, X2, size = count, alpha = .01, color = avg_sentiment)) +
  geom_text(data = pd_sent_topic, aes(X1, X2, label = word), size = 2) +
  scale_colour_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"), midpoint = 0) +
  scale_size(range = c(5, 20)) +
  xlab("") + ylab("") +
  ggtitle("2-dimensional t-SNE Mapping of Word Vectors") +
  guides(color = guide_legend(title="Sentiment"), size = guide_legend(title = "Frequency"), alpha = NULL) +
  scale_alpha(range = c(1, 1), guide = "none") +
  facet_wrap(~desc, scales=  "free")
p



