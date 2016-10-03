simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labels)/sum(labels), 
             FPR = cumsum(!labels)/sum(!labels),
             labels)
}

set.seed(1)

sim_widget_data <- function(N, noise = 100) {
  x <- runif(N, min = 0, max = 100)
  y <- 122 - x/2 + rnorm(N, sd=noise)
  bad_widget <- factor(y > 100)
  data.frame(x, y, bad_widget)
}

widget_data <- sim_widget_data(500, 10)

test_set_idx <- sample(1:nrow(widget_data), size=floor(nrow(widget_data)/4))

test_set <- widget_data[test_set_idx,]
training_set <- widget_data[-test_set_idx,]

library(ggplot2)
library(dplyr)

ggplot(test_set, aes(x, y, col = bad_widget)) +
  scale_color_manual(values = c("black", "red")) +
  geom_point() +
  ggtitle("Bad widgets related to x")


fit_glm <- glm(bad_widget ~ x, training_set, family=binomial(link="logit"))
glm_link_scores <- predict(fit_glm, test_set, type="link")
glm_response_scores <- predict(fit_glm, test_set, type="response")

score_data <- data.frame(link = glm_link_scores,
                         response = glm_response_scores,
                         bad_widget = test_set$bad_widget,
                         stringsAsFactors = FALSE)

score_data %>%
  ggplot(aes(x = link, y = response, col = bad_widget)) +
  scale_color_manual(values = c("black", "red")) +
  geom_point() +
  geom_rug() +
  ggtitle("Both link and response scores put cases in the same order")

library(pROC)
r <- roc(test_set$bad_widget, glm_response_scores, direction = "<")
plot(r, col = "yellow", lwd = 3, main = "ROC curve")

glm_simple_roc <- simple_roc(test_set$bad_widget=="TRUE", glm_link_scores)
