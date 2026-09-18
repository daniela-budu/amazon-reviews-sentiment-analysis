#ASSIGNMENT of Text Mining and Sentiment Analysis – 17711-ENG
#Boroni Lucrezia, Budu Daniela, Vigani Nicole 

# SECURITY NOTE
# Do not store credentials directly in this script.
# Before running the scraping section, define the following environment variables:
#   AMAZON_EMAIL
#   AMAZON_PASSWORD
#
# Example (temporary, current R session only):
# Sys.setenv(AMAZON_EMAIL = "your_email")
# Sys.setenv(AMAZON_PASSWORD = "your_password")
#
# For a public GitHub repository, never commit real credentials.

# LIBRARIES
library(tidyverse)
library(rvest)
library(RSelenium)  
library(netstat)
library(ggplot2)
library(stringr)
library(cld2) 
library(tidytext)
library(udpipe)
library(quanteda)
library(quanteda.textmodels)
library(caret)


#1. DATA COLLECTION
# Scraping amazon reviews  
rD <- rsDriver(browser = "firefox",
               verbose = FALSE,
               port = free_port(),
               chromever = NULL,
               phantomver = NULL)

remDr <- rD[["client"]] 
url <- "https://www.amazon.co.uk/product-reviews/B0GJF22LYF/"
remDr$navigate(url)

# identify email field
field <- remDr$findElement(using = "css", "#ap_email")
email <- Sys.getenv("AMAZON_EMAIL")

field$sendKeysToElement(list(email))

# click on the tab 
click <- remDr$findElement(using = "css", ".a-button-input")
click$clickElement()

# identify pwd field
field <- remDr$findElement(using = "css", "#ap_password")
pwd <- Sys.getenv("AMAZON_PASSWORD")

field$sendKeysToElement(list(pwd))

# click on the tab 
click <- remDr$findElement(using = "css", "#signInSubmit")
click$clickElement()

folder <- "Amazon/"
dir.create(folder)

# set the number of pages
pages <- 10

# press all the buttons
for (i in 2:pages) {
  button <- remDr$findElement(using = "css", value = "[data-hook='show-more-button']")
  button$clickElement()
  Sys.sleep(3)
}

output <- remDr$getPageSource(header = TRUE)
write(output[[1]], file = str_c(folder, "Amazon", pages, ".html"))

# close the connection
remDr$close()
rD$server$stop()


# Scrape
html <- read_html(str_c(folder, "Amazon", pages, ".html"), encoding = "utf-8")
# Review title (UK and not-UK)
title=html |> 
  html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") |>
  html_elements("span:nth-child(3)")|>
  html_text2()

title=title |> c(html |> 
                   html_elements("[class = 'a-size-base review-title a-color-base review-title-content a-text-bold']")|>
                   html_text(trim = T))


# Review text (the same for UK and not-UK)
text=html |> 
  html_elements("[class='a-size-base review-text review-text-content']") |>
  html_text(trim = T)

# Review stars (UK and not-UK)
star=html |>
  html_elements("[data-hook='review-star-rating']") |>
  html_text2()

star=star |> c(
  html |> 
    html_elements("[data-hook='cmps-review-star-rating']") |>
    html_text2())

data <- tibble(title,
               text,
               star)
View(data)
# saving dataset and creating data tibble
data = data |> 
  mutate(id = seq_along(text))
saveRDS(data, file = "data.rds")

#2. DATA CLEANING and PREPROCESSING
# language detection
data$title_lang=detect_language(data$title)
data$text_lang=detect_language(data$text) 
# checking
table(Text=data$text_lang,
      Title=data$title_lang,
      useNA="always")
#filtering only english reviews
data <- data %>% 
  filter(text_lang =="en"|is.na(text_lang),
         title_lang =="en"|is.na(title_lang))

# initial cleaning with regex
clean_text <- function(text) {
  text <- str_replace_all(text, '([!?]){2,}', '\\1') 
  text <- str_replace_all(text, '([\\.,!?=])(\\S)', '\\1 \\2') 
  text <- str_replace_all(text, '(\\.\\s?){2,}', '... ') 
  text <- str_replace_all(text, '([a-z])([A-Z])', '\\1 \\2') 
  trimws(text) 
}
data <- data %>%
  mutate(text_clean = clean_text(text))

# tidy transformation and stop words removal
data_tidy <- data %>%
  unnest_tokens(word, text_clean) %>%  
  anti_join(stop_words, by = "word")  %>%
  filter(!str_detect(word, '^[0-9]+$'))
#checking
class(data_tidy)
dim(data_tidy)
head(data_tidy)


#3. DESCRIPTIVE ANALYSIS and VISUALIZATION
data = data %>% 
  mutate(score=as.numeric(str_extract(star,"\\d+\\.\\d+")))
# analyse the score
data %>% 
  summarise(
    mean = mean(score),
    min = min(score),
    median = median(score),
    max = max(score)
  )
data %>% 
  count(score) %>% 
  mutate(p = round(n/sum(n), 2))

# VISUALIZATION ONE: 
# ratings distribution
data %>%
  ggplot(aes(x = factor(score))) +
  geom_bar(fill = "pink2") +
  labs(
    title = "Amazon Reviews' Star Ratings",
    subtitle = "SodaStream Fizz & Go Blueberry",
    x = "Rating",
    y = "Number of Reviews"
  ) +
  theme_bw() +
  theme(plot.title = element_text(color = "pink2",
                                    size = 12,
                                    face = "bold"),
          plot.subtitle = element_text(color = "pink4"))

# VISUALIZATION TWO: 
# 20 most frequently used words
freq.df <- data_tidy %>%
  count(word, sort = TRUE)

freq.df %>%
  slice_max(n, n = 20) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 20 most frequent words",
    x = NULL,
    y = "Frequency"
  ) +
  theme_bw()


#4. DICTIONARY-BASED SENTIMENT ANALYSIS: TIDYTEXT APPROACH
# Bing lexicon
bing <- get_sentiments("bing")

# Match review words with Bing sentiment dictionary
data_bing_words <- data_tidy %>%
  inner_join(bing, by = "word")

head(data_bing_words)

# sentiment score base analysis
data_bing_sentiment <- data_bing_words %>%
  count(id, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(sentiment_score_tidy = positive - negative)

#Join sentiment scores with original review-level dataset
data_sentiment_tidy <- data %>%
  left_join(data_bing_sentiment, by = "id") %>%
  mutate(
    positive = replace_na(positive, 0),
    negative = replace_na(negative, 0),
    sentiment_score_tidy = replace_na(sentiment_score_tidy, 0)
  )



# Tidy summary
summary_tidy <- data_sentiment_tidy %>%
  group_by(score) %>%
  summarise(
    mean_sentiment_tidy = mean(sentiment_score_tidy, na.rm = TRUE),
    mean_positive = mean(positive, na.rm = TRUE),
    mean_negative = mean(negative, na.rm = TRUE),
    n = n()
  )

summary_tidy



# VISUALIZATION tidytext approach:
# most frequent positive and negative words
data_bing_words %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Most frequent positive and negative words",
    x = NULL,
    y = "Frequency"
  ) +
  theme_bw()


#5. DICTIONARY-BASED SENTIMENT ANALYSIS: UDPIPE APPROACH
# Prepare data for UDPipe
data_udpipe_input <- data %>%
  select(id, text_clean) %>%
  rename(
    doc_id = id,
    text = text_clean
  )

data_udpipe_input$text <- iconv(data_udpipe_input$text, to = "UTF-8", sub = "byte")
# UDPipe annotation
output_udpipe <- udpipe(data_udpipe_input, "english-gum")
# Bing dictionary for UDPipe sentiment analysis
bing_dict <- bing %>%
  mutate(polarity = ifelse(sentiment == "positive", 1, -1)) %>%
  select(term = word, polarity)

# UDPIPE basic
sent_udpipe_basic <- txt_sentiment(
  x = output_udpipe,
  term = "token",
  polarity_terms = bing_dict,
  polarity_negators = NULL,
  polarity_amplifiers = NULL,
  polarity_deamplifiers = NULL,
  amplifier_weight = 0,
  n_before = 0,
  n_after = 0,
  constrain = FALSE
)

# UDPIPE with context
sent_udpipe_context <- txt_sentiment(
  x = output_udpipe,
  term = "token",
  polarity_terms = bing_dict,
  polarity_negators = c("not", "no", "neither", "without"),
  polarity_amplifiers = c("really", "very", "definitely", "super"),
  polarity_deamplifiers = c("barely", "hardly"),
  amplifier_weight = 0.8,
  n_before = 2,
  n_after = 0,
  constrain = FALSE
)

# Store both scores
data_sentiment_udpipe <- data %>%
  mutate(
    sentiment_score_udpipe_basic =
      sent_udpipe_basic$overall$sentiment_polarity,
    sentiment_score_udpipe_context =
      sent_udpipe_context$overall$sentiment_polarity
  )


# UDPipe summary
summary_udpipe <- data_sentiment_udpipe %>%
  group_by(score) %>%
  summarise(
    avg_udpipe_basic = mean(sentiment_score_udpipe_basic, na.rm = TRUE),
    avg_udpipe_context = mean(sentiment_score_udpipe_context, na.rm = TRUE),
    n_reviews = n()
  )

summary_udpipe

# COMPARISON TIDY - UDPIPE
comparison_sentiment <- data_sentiment_tidy %>%
  select(id, score, sentiment_score_tidy) %>%
  left_join(
    data_sentiment_udpipe %>%
      select(id, sentiment_score_udpipe_basic, sentiment_score_udpipe_context),
    by = "id"
  )
comparison_sentiment %>%
  summarise(
    mean_tidy = mean(sentiment_score_tidy, na.rm = TRUE),
    mean_udpipe_basic = mean(sentiment_score_udpipe_basic, na.rm = TRUE),
    mean_udpipe_context = mean(sentiment_score_udpipe_context, na.rm = TRUE)
  )

# comparison dataset
comparison_sentiment <- data_sentiment_tidy %>%
  select(id, sentiment_score_tidy) %>%
  left_join(
    data_sentiment_udpipe %>%
      select(id, sentiment_score_udpipe_context),
    by = "id"
  )

# Categorise tidy sentiment
comparison_sentiment <- comparison_sentiment %>%
  mutate(
    tidy_pol = case_when(
      sentiment_score_tidy > 0 ~ "positive",
      sentiment_score_tidy < 0 ~ "negative",
      TRUE ~ "neutral"
    ),
    udpipe_pol = case_when(
      sentiment_score_udpipe_context > 0 ~ "positive",
      sentiment_score_udpipe_context < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )

# Comparison table 
table(
  TIDY = comparison_sentiment$tidy_pol,
  UDPIPE = comparison_sentiment$udpipe_pol
)


#6)  NAIVE BAYES
data %>%
  count(score)
table(data$score)
# since the number of 3-star reviews is limited (6) we can delete it to create a binary score
# such as: 
# 1-2 stars -> negative
# 4-5 stars -> positive
# 3 stars   -> excluded
data_nb <- data %>%
  mutate(
    star_sent = case_when(
      score <= 2 ~ "negative",
      score >= 4 ~ "positive",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(star_sent))
# results: 
table(data_nb$star_sent)
# dataset unbalanced - more positive than negative

# select only useful data
glimpse(data_nb)
data_q <- data_nb %>% 
  select(id, text_clean, star_sent)

corp_nb <- corpus(data_q, text_field = "text_clean")
# remove punctuation,  numbers, stopwords and apply stemming + tokenization
tokens_nb <- tokens(
  corp_nb,
  remove_punct = TRUE,
  remove_numbers = TRUE
) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem()
# document feature matrix
dfm_nb <- dfm(tokens_nb)

set.seed(6272)

# 70% of reviews for the training set
id_train <- sample(
  1:nrow(data_q),
  size = round(0.7 * nrow(data_q)),
  replace = FALSE
)
# split dfm into training and test sets
dfm_train <- dfm_nb[id_train, ]
dfm_test <- dfm_nb[-id_train, ]

# train the naive bayes classifier
tmod_nb <- textmodel_nb(dfm_train, dfm_train$star_sent)

# model summary
summary(tmod_nb)
# matching
dfm_matched <- dfm_match(
  dfm_test,
  features = featnames(dfm_train)
)
# prediction 
predicted_class <- predict(
  tmod_nb,
  newdata = dfm_matched
)
# extracting labels
actual_class <- dfm_matched$star_sent
# compare predicted - actual
tab_class <- table(predicted_class, actual_class)
tab_class

# model performance matrix
cm_nb <- confusionMatrix(
  tab_class,
  positive = "positive",
  mode = "everything"
)
cm_nb

#7) COMPARISON WITH DICTIONARY-BASED METHODS AND NAIVE BAYES

# results
table(data_nb$star_sent)

# Same documents used in the NB test set
data_test <- data_nb[-id_train, ]

nrow(data_test)
length(predicted_class)

comparison_test <- data_test %>%
  select(id, star_sent) %>%
  left_join(
    data_sentiment_tidy %>%
      select(id, sentiment_score_tidy),
    by = "id"
  ) %>%
  left_join(
    data_sentiment_udpipe %>%
      select(id, sentiment_score_udpipe_basic, sentiment_score_udpipe_context),
    by = "id"
  ) %>%
  mutate(
    true_sent = factor(star_sent),
    pred_NB = factor(predicted_class),
    tidy_lab = factor(ifelse(sentiment_score_tidy >= 0, "positive", "negative")),
    udpipe_basic_lab = factor(ifelse(sentiment_score_udpipe_basic >= 0, "positive", "negative")),
    udpipe_context_lab = factor(ifelse(sentiment_score_udpipe_context >= 0, "positive", "negative"))
  )

# Confusion matrices
cm_test_tidy <- confusionMatrix(
  comparison_test$tidy_lab,
  comparison_test$true_sent,
  positive = "positive",
  mode = "everything"
)

cm_test_udpipe_basic <- confusionMatrix(
  comparison_test$udpipe_basic_lab,
  comparison_test$true_sent,
  positive = "positive",
  mode = "everything"
)

cm_test_udpipe_context <- confusionMatrix(
  comparison_test$udpipe_context_lab,
  comparison_test$true_sent,
  positive = "positive",
  mode = "everything"
)

cm_test_nb <- confusionMatrix(
  comparison_test$pred_NB,
  comparison_test$true_sent,
  positive = "positive",
  mode = "everything"
)

cm_test_tidy
cm_test_udpipe_basic
cm_test_udpipe_context
cm_test_nb

# Accuracy comparison
accuracy_comparison <- tibble(
  method = c("Tidy Bing", "UDPipe Basic", "UDPipe Context", "Naive Bayes"),
  accuracy = c(
    cm_test_tidy$overall["Accuracy"],
    cm_test_udpipe_basic$overall["Accuracy"],
    cm_test_udpipe_context$overall["Accuracy"],
    cm_test_nb$overall["Accuracy"]
  )
)

accuracy_comparison

