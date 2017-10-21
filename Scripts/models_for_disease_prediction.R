# https://shiring.github.io/machine_learning/2017/03/31/webinar_code

bc_data <- read.table("data/breast-cancer-wisconsin.txt", 
                      header = FALSE, 
                      sep = ",")
colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes")

bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))

# missing data

bc_data[bc_data == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data)))

# impute missing data
library(mice)
bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))
bc_data$classes <- as.factor(bc_data$classes)
summary(bc_data$classes)

# data exploration
library(ggplot2)

ggplot(bc_data, aes(x = classes, fill = classes)) +
  geom_bar()
ggplot(bc_data, aes(x = clump_thickness)) +
  geom_histogram(bins = 10)

# PCA

library(pcaGoPromoter)
library(ellipse)

pca_output <- pca(t(bc_data[, -1]), printDropped = FALSE, scale = TRUE, center = TRUE)
pca_output2 <- as.data.frame(pca_output$scores)
pca_output2$groups <- bc_data$classes

centroids <- aggregate(cbind(PC1, PC2) ~ groups, pca_output2, mean)

conf_rgn <- do.call(rbind, lapply(unique(pca_output2$groups), function(t)
  data.frame(groups = as.character(t),
             ellipse(cov(pca_output2[pca_output2$groups == t, 1:2]),
                     centre = as.matrix(centroids[centroids$groups==t, 2:3]),
                     level = 0.95),
             stringsAsFactors = FALSE)))

ggplot(data = pca_output2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
  geom_polygon(data = conf_rgn, aes(fill = groups), alpha = 0.2) +
  geom_point(size = 2, alpha = 0.6) + 
  scale_color_brewer(palette = "Set1") +
  labs(color = "",
       fill = "",
       x = paste0("PC1: ", round(pca_output$pov[1], digits = 2) * 100, "% variance"),
       y = paste0("PC2: ", round(pca_output$pov[2], digits = 2) * 100, "% variance")) 

library(tidyr)

gather(bc_data, x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x=y, color=classes, fill=classes)) +
  geom_density(alpha=0.3) + 
  facet_wrap(~x, scales="free", ncol=3)

# load caret
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
library(caret)

# partition data
set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]

library(tidyverse)
rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

# glm
set.seed(42)
model_glm <- caret::train(clump_thickness~., data=train_data, method="glm",
                          preProcess=c("scale", "center"),
                          trControl = trainControl(method="repeatedcv",
                                                   number=10, repeats=10,
                                                   savePredictions = TRUE,
                                                   verboseIter = FALSE))
predictions <- predict(model_glm, test_data)

data.frame(residuals = resid(model_glm),
           y = model_glm$finalModel$y) %>%
  ggplot(aes(x = y, y = residuals)) +
  geom_jitter() +
  geom_smooth(method = "lm")
data.frame(actual = test_data$clump_thickness,
           predicted = predictions) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# decision trees
library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(classes ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)

# random forest

set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))
model_rf$finalModel$confusion

imp <- model_rf$finalModel$importance
imp[order(imp, decreasing = TRUE), ]
importance <- varImp(model_rf, scale = TRUE)
plot(importance)

confusionMatrix(predict(model_rf, test_data), test_data$classes)

results <- data.frame(actual = test_data$classes,
                      predict(model_rf, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
  geom_jitter(size = 3, alpha = 0.6)

# xgboost

set.seed(42)
model_xgb <- caret::train(classes ~ .,
                          data = train_data,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))

confusionMatrix(predict(model_xgb, test_data), test_data$classes)

results <- data.frame(actual = test_data$classes,
                      predict(model_xgb, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
  geom_jitter(size = 3, alpha = 0.6)

# feature selection
library(corrplot)

# calculate correlation matrix
corMatMy <- cor(train_data[, -1])
corrplot(corMatMy, order = "hclust")

findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)
highlyCor <- colnames(train_data[, -1])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]
train_data_cor <- train_data[, which(!colnames(train_data) %in% highlyCor)]

# using rfe
set.seed(7)
results_rfe <- rfe(x = train_data[, -1], y = train_data$classes, sizes = c(1:9),
                   rfeControl = rfeControl(functions=rfFuncs, method="cv", number=10))

predictors(results_rfe)

# genetic algorithm for feature selection
set.seed(27)
model_ga <- gafs(x = train_data[, -1], 
                 y = train_data$classes,
                 iters = 10, # generations of algorithm
                 popSize = 10, # population size for each generation
                 levels = c("malignant", "benign"),
                 gafsControl = gafsControl(functions = rfGA, # Assess fitness with RF
                                           method = "cv",    # 10 fold cross validation
                                           genParallel = TRUE, # Use parallel programming
                                           allowParallel = TRUE))
model_ga$ga$final

# dealing with unbalanced dataset

set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]
# base model
set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))

final <- data.frame(actual = test_data$classes,
                    predict(model_rf, newdata = test_data, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
cm_original <- confusionMatrix(final$predict, test_data$classes)

# under-sampling
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                     verboseIter = FALSE, sampling = "down")
set.seed(42)
model_rf_under <- caret::train(classes ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_under <- data.frame(actual = test_data$classes,
                          predict(model_rf_under, newdata = test_data, type = "prob"))
final_under$predict <- ifelse(final_under$benign > 0.5, "benign", "malignant")
cm_under <- confusionMatrix(final_under$predict, test_data$classes)

# over sampling
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(42)
model_rf_over <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_over <- data.frame(actual = test_data$classes,
                         predict(model_rf_over, newdata = test_data, type = "prob"))
final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")
cm_over <- confusionMatrix(final_over$predict, test_data$classes)

# ROSE
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_rose <- data.frame(actual = test_data$classes,
                         predict(model_rf_rose, newdata = test_data, type = "prob"))
final_rose$predict <- ifelse(final_rose$benign > 0.5, "benign", "malignant")
cm_rose <- confusionMatrix(final_rose$predict, test_data$classes)

# SMOTE
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_smote <- data.frame(actual = test_data$classes,
                         predict(model_rf_smote, newdata = test_data, type = "prob"))
final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")
cm_smote <- confusionMatrix(final_smote$predict, test_data$classes)

# predictions
models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)
resampling <- resamples(models)
bwplot(resampling)

library(dplyr)
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("cm_", name))
  
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(Sensitivity = model$byClass["Sensitivity"],
           Specificity = model$byClass["Specificity"],
           Precision = model$byClass["Precision"],
           Recall = model$byClass["Recall"],
           F1 = model$byClass["F1"])
}

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)