# replicated from http://varianceexplained.org/r/yelp-sentiment/

library(dplyr)
library(readr)
library(jsonlite)
library(stringr)
library(tidytext)
library(ggplot2)
theme_set(theme_bw())

options(tibble.width = Inf)

infile = "Data/yelp_academic_dataset_review.json"
review_lines = read_lines(infile, n_max=200000, progress=FALSE)

reviews_combined = str_c("[", str_c(review_lines, collapse = ", "), "]")
reviews = fromJSON(reviews_combined) %>% flatten() %>% tbl_df()

review_words = reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

AFINN = sentiments %>% 
  filter(lexicon=="AFINN") %>%
  select(word, afinn_score=score)

reviews_sentiment = review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))

ggplot(reviews_sentiment, aes(stars, sentiment, group=stars)) +
  geom_boxplot() + ylab("Average sentiment score")

review_words_counted = review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

word_summaries = review_words_counted %>% 
  group_by(word) %>%
  summarise(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup()

word_summaries_filtered = word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

word_summaries_filtered %>%
  arrange(average_stars)

ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1, hjust=1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color="red", lty=2) +
  xlab("# of reviews") +
  ylab("Average stars")

words_afinn = word_summaries_filtered %>%
  inner_join(AFINN, by="word")

ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of the word") +
  ylab("Average stars of reviews with this word")

words_afinn %>%
  arrange(desc(reviews)) %>%
  ggplot(aes(afinn_score, average_stars)) +
  geom_point(aes(size=reviews)) +
  geom_text(aes(label=word), vjust=1, hjust=1, check_overlap = TRUE) +
  geom_smooth(method="lm", se=FALSE) +
  xlab("AFINN Sentiment Score") + 
  ylab("Average Yelp Stars") + 
  expand_limits(x=-6)

word_summaries_filtered %>%
  inner_join(AFINN, by="word") %>%
  ggplot(aes(reviews, average_stars, color=afinn_score)) +
  geom_point() +
  geom_text(aes(label=word), check_overlap = TRUE, hjust=1, vjust=1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color="red", lty=2) +
  scale_color_gradient2(low="red", high="blue", midpoint = 0, mid="gray") +
  labs(x="# of reviews", y="Average stars", color="AFINN")

