library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(purrr)
library(keras)
library(Metrics)

df <- read_csv("Data/creditcardfraud.csv", col_types = list(Time = col_number()))

df %>%
  gather(variable, value, -Class) %>%
  ggplot(aes(y = as.factor(variable), fill = as.factor(Class), x = percent_rank(value))) +
  geom_density_ridges()

#split into train test
df_train <- df %>% filter(row_number(Time) <= 200000) %>% select(-Time)
df_test <- df %>% filter(row_number(Time) > 200000) %>% select(-Time)

get_desc <- function(x) {
  map(x, ~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
}

normalization_minmax <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
}

desc <- df_train %>% 
  select(-Class) %>% 
  get_desc()

x_train <- df_train %>%
  select(-Class) %>%
  normalization_minmax(desc) %>%
  as.matrix()

x_test <- df_test %>%
  select(-Class) %>%
  normalization_minmax(desc) %>%
  as.matrix()
y_train <- df_train$Class
y_test <- df_test$Class

# model

model <- keras_model_sequential()
model %>%
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = 15, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))
summary(model)


model %>% compile(loss = "mean_squared_error", optimizer = "adam")

checkpoint <- callback_model_checkpoint(
  filepath = "temp/model.hdf5",
  save_best_only = TRUE,
  period = 1,
  verbose = 1
)

early_stopping <- callback_early_stopping(patience = 5)

model %>% fit(
  x = x_train[y_train == 0,],
  y = x_train[y_train == 0,],
  epochs = 100,
  batch_size = 32,
  validation_data = list(x_test[y_test == 0,], x_test[y_test == 0,]),
  callbacks = list(checkpoint, early_stopping)
)


pred_train <- predict(model, x_train)
mse_train <- apply((x_train - pred_train)^2, 1, sum)
pred_test <- predict(model, x_test)
mse_test <- apply((x_test - pred_test)^2, 1, sum)

auc(y_train, mse_train)
auc(y_test, mse_test)

d <- data.frame(mse = mse_test, y = y_test)

ggplot(d, aes(as.factor(y), mse)) + geom_boxplot() + scale_y_log10()

# hunt for threshold k
possible_k <- seq(0, 0.5, length.out = 100)

precision <- sapply(possible_k, function(k) {
  predicted_class <- as.numeric(mse_test > k)
  sum(predicted_class == 1 & y_test == 1)/sum(predicted_class)
})
qplot(possible_k, precision, geom = "line") + labs(x = "Threshold", y = "Precision")

recall <- sapply(possible_k, function(k) {
  predicted_class <- as.numeric(mse_test > k)
  sum(predicted_class == 1 & y_test == 1)/sum(y_test)
})
qplot(possible_k, recall, geom = "line") + labs(x = "Threshold", y = "Recall")

cost_per_verification <- 1

lost_money <- sapply(possible_k, function(k) {
  predicted_class <- as.numeric(mse_test > k)
  sum(cost_per_verification * predicted_class + (predicted_class == 0) * y_test * df_test$Amount)
})


qplot(possible_k, lost_money, geom = "line") + labs(x = "Threshold", y = "Lost Money")
possible_k[which.min(lost_money)]
