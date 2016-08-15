library(dplyr)
library(purrr)
library(twitteR)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)
library(tidytext)
library(broom)

theme_set(theme_bw())
options(tibble.width = Inf)

consumerKey = "0RgrnAZ9wsdQaSbngpzsNvUwh"
consumerSecret = "UHUZXy43SoE7y8COSLm8k9fkd6AV13QKSWXEoYlAFmlGESLZIm"
accessToken = "2732354256-xCXzZeczCMX32427KU9pV7mhPAul1wIZlVfAF1A"
accessTokenSecret = "zR7j7yKN0rps5DJAxIh4zSUi2bMvu91iIWoPSV0Hs2X6K"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

trump_tweets = userTimeline("realDonaldTrump", n=3200)
trump_tweets_df = tbl_df(map_df(trump_tweets, as.data.frame))

tweets = trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

tweets %>% 
  count(source, hour=hour(with_tz(created, "EST"))) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(hour, percent, colour=source)) +
  geom_line() +
  scale_y_continuous(labels=percent_format()) +
  scale_fill_hue() +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

tweet_picture_counts = tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source, picture = ifelse(str_detect(text, "t.co"),
                                 "Picture/link", "No Picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

reg = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words = tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern=reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

ggplot(tweet_words, aes(word)) + geom_histogram(stat="count")

android_iphone_ratios = tweet_words %>%
  count(word, source) %>%
  filter(sum(n)>=5) %>%
  spread(source, n, fill=0) %>%
  ungroup() %>%
  mutate_each(funs((.+1)/sum(.+1)), -word) %>%
  mutate(logratio = log2(Android/iPhone)) %>%
  arrange(desc(logratio))

topbottom = rbind(head(android_iphone_ratios, 10))

tweet_words %>%
  count(word, sort=TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

android_iphone_ratios %>%
  group_by(logratio>0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

#sentiment analysis
nrc = sentiments %>% 
  filter(lexicon =="nrc") %>%
  select(word, sentiment)

sources = tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)

by_source_sentiment = tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill=list(n=0)) %>%
  inner_join(sources, by = "id") %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

sentiment_differences = by_source_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, estimate)) %>%
  mutate_each(funs(.-1), estimate, conf.low, conf.high) %>%
  ggplot(aes(estimate, sentiment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(labels = percent_format()) +
  labs(x = "% increase in Android relative to iPhone", 
       y = "Sentiment")

android_iphone_ratios %>%
  inner_join(nrc, by="word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio), 
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  ggplot(aes(word, logratio, fill = logratio<0)) +
  facet_wrap(~sentiment, scales = "free", nrow = 2) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android/iPhone log ratio") + 
  scale_fill_manual(name = "", labels = c("Android", "iPhone"), 
                     values = c("red", "lightblue"))
  