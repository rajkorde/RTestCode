#https://shiring.github.io/machine_learning/2016/12/02/flu_outcome_ML_2_post

library(outbreaks)
library(tidyr)
library(plyr)
library(ggplot2)

fluH7N9.china.2013_backup <- fluH7N9.china.2013

## cleaning and EDA
fluH7N9.china.2013$age[which(fluH7N9.china.2013$age == "?")] <- NA
fluH7N9.china.2013$case.ID <- paste("case", fluH7N9.china.2013$case.ID, sep = "_")


fluH7N9.china.2013_gather <- fluH7N9.china.2013 %>%
  gather(Group, Date, date.of.onset:date.of.outcome)
fluH7N9.china.2013_gather$Group <- factor(fluH7N9.china.2013_gather$Group, 
  levels = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"))

fluH7N9.china.2013_gather$Group <- mapvalues(fluH7N9.china.2013_gather$Group, 
  from = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"), 
  to = c("Date of onset", "Date of hospitalisation", "Date of outcome"))

# renaming provinces
fluH7N9.china.2013_gather$province <- mapvalues(fluH7N9.china.2013_gather$province, 
  from = c("Anhui", "Beijing", "Fujian", "Guangdong", "Hebei", "Henan", "Hunan", 
           "Jiangxi", "Shandong", "Taiwan"), 
  to = rep("Other", 10))

# add a level for unknown gender
levels(fluH7N9.china.2013_gather$gender) <- 
  c(levels(fluH7N9.china.2013_gather$gender), "unknown")
fluH7N9.china.2013_gather$gender[is.na(fluH7N9.china.2013_gather$gender)] <- 
  "unknown"

# rearrange province order so that Other is the last
fluH7N9.china.2013_gather$province <- factor(fluH7N9.china.2013_gather$province, 
  levels = c("Jiangsu",  "Shanghai", "Zhejiang", "Other"))

my_theme <- function(base_size = 12, base_family = "Roboto"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "bottom",
      legend.justification = "top", 
      legend.box = "horizontal",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

ggplot(data = fluH7N9.china.2013_gather, aes(x = Date, y = as.numeric(age), fill = outcome)) +
  stat_density2d(aes(alpha = ..level..), geom = "polygon") +
  geom_jitter(aes(color = outcome, shape = gender), size = 1.5) +
  geom_rug(aes(color = outcome)) +
  labs(
    fill = "Outcome",
    color = "Outcome",
    alpha = "Level",
    shape = "Gender",
    x = "Date in 2013",
    y = "Age",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Dataset from 'outbreaks' package (Kucharski et al. 2014)",
    caption = ""
  ) +
  facet_grid(Group ~ province) +
  my_theme() +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1")

fluH7N9.china.2013_gather_2 <- fluH7N9.china.2013_gather[, -4] %>%
  gather(group_2, value, gender:province)

fluH7N9.china.2013_gather_2$value <- mapvalues(fluH7N9.china.2013_gather_2$value, from = c("m", "f", "unknown", "Other"), 
                                               to = c("Male", "Female", "Unknown gender", "Other province"))

fluH7N9.china.2013_gather_2$value <- factor(fluH7N9.china.2013_gather_2$value, 
                                            levels = c("Female", "Male", "Unknown gender", "Jiangsu", "Shanghai", "Zhejiang", "Other province"))

p1 <- ggplot(data = fluH7N9.china.2013_gather_2, aes(x = value, fill = outcome, color = outcome)) +
  geom_bar(position = "dodge", alpha = 0.7, size = 1) +
  my_theme() +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  labs(
    color = "",
    fill = "",
    x = "",
    y = "Count",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Gender and Province numbers of flu cases",
    caption = ""
  )

p2 <- ggplot(data = fluH7N9.china.2013_gather, aes(x = as.numeric(age), fill = outcome, color = outcome)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_rug() +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  my_theme() +
  labs(
    color = "",
    fill = "",
    x = "Age",
    y = "Density",
    title = "",
    subtitle = "Age distribution of flu cases",
    caption = ""
  )

library(gridExtra)
library(grid)

grid.arrange(p1, p2, ncol = 2)

ggplot(data = fluH7N9.china.2013_gather, aes(x = Date, y = as.numeric(age), color = outcome)) +
  geom_point(aes(shape = gender), size = 1.5, alpha = 0.6) +
  geom_path(aes(group = case.ID)) +
  facet_wrap( ~ province, ncol = 2) +
  my_theme() +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1") +
  labs(
    color = "Outcome",
    shape = "Gender",
    x = "Date in 2013",
    y = "Age",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Dataset from 'outbreaks' package (Kucharski et al. 2014)",
    caption = "\nTime from onset of flu to outcome."
  )

##Feature Engineering
library(dplyr)

dataset <- fluH7N9.china.2013 %>%
  mutate(hospital = as.factor(ifelse(is.na(date.of.hospitalisation), 0, 1)),
         gender_f = as.factor(ifelse(gender == "f", 1, 0)),
         province_Jiangsu = as.factor(ifelse(province == "Jiangsu", 1, 0)),
         province_Shanghai = as.factor(ifelse(province == "Shanghai", 1, 0)),
         province_Zhejiang = as.factor(ifelse(province == "Zhejiang", 1, 0)),
         province_other = as.factor(ifelse(province == "Zhejiang" | province == "Jiangsu" | province == "Shanghai", 0, 1)),
         days_onset_to_outcome = as.numeric(as.character(gsub(" days", "",
                                                              as.Date(as.character(date.of.outcome), format = "%Y-%m-%d") - 
                                                                as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         days_onset_to_hospital = as.numeric(as.character(gsub(" days", "",
                                                               as.Date(as.character(date.of.hospitalisation), format = "%Y-%m-%d") - 
                                                                 as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         age = as.numeric(as.character(age)),
         early_onset = as.factor(ifelse(date.of.onset < summary(fluH7N9.china.2013$date.of.onset)[[3]], 1, 0)),
         early_outcome = as.factor(ifelse(date.of.outcome < summary(fluH7N9.china.2013$date.of.outcome)[[3]], 1, 0))) %>%
  subset(select = -c(2:4, 6, 8))
rownames(dataset) <- dataset$case.ID
dataset <- dataset[, -1]

#impute missing rows
library(mice)
dataset_impute <- mice(dataset[, -1],  print = FALSE)
dataset_complete <- merge(dataset[, 1, drop = FALSE], mice::complete(dataset_impute, 1), by = "row.names", all = TRUE)
rownames(dataset_complete) <- dataset_complete$Row.names
dataset_complete <- dataset_complete[, -1]


#partition data into test and train sets
train_index <- which(is.na(dataset_complete$outcome))
train_data <- dataset_complete[-train_index, ]
test_data  <- dataset_complete[train_index, -1]

library(caret)
set.seed(27)
val_index <- createDataPartition(train_data$outcome, p = 0.7, list=FALSE)
val_train_data <- train_data[val_index, ]
val_test_data  <- train_data[-val_index, ]
val_train_X <- val_train_data[,-1]
val_test_X <- val_test_data[,-1]


#DecisionTrees

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

set.seed(27)
fit <- rpart(outcome ~ .,
             data = train_data[, 2:ncol(train_data)],
             method = "class",
             control = rpart.control(xval = 10, minbucket = 2, cp = 0), parms = list(split = "information"))

fancyRpartPlot(fit)

#Feature Importance using random forest
library(e1071)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train the model
set.seed(27)
model <- train(outcome ~ ., data = train_data[, 2:ncol(train_data)], method = "rf", preProcess = NULL, trControl = control)

# estimate variable importance
importance <- varImp(model, scale=TRUE)

# prepare for plotting
importance_df_1 <- importance$importance
importance_df_1$group <- rownames(importance_df_1)

importance_df_1$group <- mapvalues(importance_df_1$group, 
                                   from = c("age", "hospital2", "gender_f2", "province_Jiangsu2", "province_Shanghai2", "province_Zhejiang2",
                                            "province_other2", "days_onset_to_outcome", "days_onset_to_hospital", "early_onset2", "early_outcome2" ), 
                                   to = c("Age", "Hospital", "Female", "Jiangsu", "Shanghai", "Zhejiang",
                                          "Other province", "Days onset to outcome", "Days onset to hospital", "Early onset", "Early outcome" ))
f = importance_df_1[order(importance_df_1$Overall, decreasing = FALSE), "group"]

importance_df_2 <- importance_df_1
importance_df_2$Overall <- 0

importance_df <- rbind(importance_df_1, importance_df_2)

# setting factor levels
importance_df <- within(importance_df, group <- factor(group, levels = f))
importance_df_1 <- within(importance_df_1, group <- factor(group, levels = f))

ggplot() +
  geom_point(data = importance_df_1, aes(x = Overall, y = group, color = group), size = 2) +
  geom_path(data = importance_df, aes(x = Overall, y = group, color = group, group = group), size = 1) +
  scale_color_manual(values = rep(brewer.pal(1, "Set1")[1], 11)) +
  my_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Importance",
    y = "",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Scaled feature importance",
    caption = "\nDetermined with Random Forest and
    repeated cross validation (10 repeats, 10 times)"
  )


##Check distributions
# prepare for plotting
dataset_complete_gather <- dataset_complete %>%
  mutate(set = ifelse(rownames(dataset_complete) %in% rownames(test_data), "Test Data", 
                      ifelse(rownames(dataset_complete) %in% rownames(val_train_data), "Validation Train Data",
                             ifelse(rownames(dataset_complete) %in% rownames(val_test_data), "Validation Test Data", "NA"))),
         case_ID = rownames(.)) %>%
  gather(group, value, age:early_outcome)

dataset_complete_gather$group <- mapvalues(dataset_complete_gather$group, 
                                           from = c("age", "hospital", "gender_f", "province_Jiangsu", "province_Shanghai", "province_Zhejiang",
                                                    "province_other", "days_onset_to_outcome", "days_onset_to_hospital", "early_onset", "early_outcome" ), 
                                           to = c("Age", "Hospital", "Female", "Jiangsu", "Shanghai", "Zhejiang",
                                                  "Other province", "Days onset to outcome", "Days onset to hospital", "Early onset", "Early outcome" ))
ggplot(data = dataset_complete_gather, aes(x = as.numeric(value), fill = outcome, color = outcome)) +
  geom_density(alpha = 0.2) +
  geom_rug() +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  my_theme() +
  facet_wrap(set ~ group, ncol = 11, scales = "free") +
  labs(
    x = "Value",
    y = "Density",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Features for classifying outcome",
    caption = "\nDensity distribution of all features used for classification of flu outcome."
  )

ggplot(subset(dataset_complete_gather, group == "Age" | group == "Days onset to hospital" | group == "Days onset to outcome"), 
       aes(x=outcome, y=as.numeric(value), fill=set)) + geom_boxplot() +
  my_theme() +
  scale_fill_brewer(palette="Set1", type = "div ") +
  facet_wrap( ~ group, ncol = 3, scales = "free") +
  labs(
    fill = "",
    x = "Outcome",
    y = "Value",
    title = "2013 Influenza A H7N9 cases in China",
    subtitle = "Features for classifying outcome",
    caption = "\nBoxplot of the features age, days from onset to hospitalisation and days from onset to outcome."
  )


#model building
#random forest

set.seed(27)
model_rf <- caret::train(outcome ~ .,
                         data = val_train_data[, -1],
                         method = "rf",
                         preProcess = NULL,
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_rf, val_test_data[, -1]), val_test_data$outcome)

#glmnet
set.seed(27)
model_glmnet <- caret::train(outcome ~ .,
                             data = val_train_data[, -1],
                             method = "glmnet",
                             preProcess = NULL,
                             trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_glmnet, val_test_data[, -1]), val_test_data$outcome)


#knn
set.seed(27)
model_kknn <- caret::train(outcome ~ .,
                           data = val_train_data[, -1],
                           method = "kknn",
                           preProcess = NULL,
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_kknn, val_test_data[, -1]), val_test_data$outcome)

#penalized discriminant analysis
set.seed(27)
model_pda <- caret::train(outcome ~ .,
                          data = val_train_data[, -1],
                          method = "pda",
                          preProcess = NULL,
                          trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_pda
confusionMatrix(predict(model_pda, val_test_data[, -1]), val_test_data$outcome)

#xgboost
library(xgboost)
matrix_train <- apply(val_train_X, 2, function(x) as.numeric(as.character(x)))
outcome_death_train <- ifelse(val_train_data$outcome == "Death", 1, 0)
matrix_test <- apply(val_test_X, 2, function(x) as.numeric(as.character(x)))
outcome_death_test <- ifelse(val_test_data$outcome == "Death", 1, 0)

xgb_train_matrix <- xgb.DMatrix(data = as.matrix(matrix_train), label = outcome_death_train)
xgb_test_matrix <- xgb.DMatrix(data = as.matrix(matrix_test), label = outcome_death_test)
watchlist <- list(train = xgb_train_matrix, test = xgb_test_matrix)
label <- getinfo(xgb_test_matrix, "label")

param <- list("objective" = "binary:logistic")

xgb.cv(param = param, 
       data = xgb_train_matrix, 
       nfold = 3,
       label = getinfo(xgb_train_matrix, "label"),
       nrounds = 5)

bst_1 <- xgb.train(data = xgb_train_matrix, 
                   label = getinfo(xgb_train_matrix, "label"),
                   max.depth = 2, 
                   eta = 1, 
                   nthread = 4, 
                   nround = 50, # number of trees used for model building
                   watchlist = watchlist, 
                   objective = "binary:logistic")

#Feature importance
features = colnames(matrix_train)
importance_matrix_1 <- xgb.importance(features, model = bst_1)
print(importance_matrix_1)
xgb.ggplot.importance(importance_matrix_1) +
  theme_minimal()

pred_1 <- predict(bst_1, xgb_test_matrix)

result_1 <- data.frame(case_ID = rownames(val_test_data),
                       outcome = val_test_data$outcome, 
                       label = label, 
                       prediction_p_death = round(pred_1, digits = 2),
                       prediction = as.integer(pred_1 > 0.5),
                       prediction_eval = ifelse(as.integer(pred_1 > 0.5) != label, "wrong", "correct"))
result_1
err <- as.numeric(sum(as.integer(pred_1 > 0.5) != label))/length(label)
print(paste("test-error =", round(err, digits = 2)))

#training with gblinear
bst_2 <- xgb.train(data = xgb_train_matrix, 
                   booster = "gblinear", 
                   label = getinfo(xgb_train_matrix, "label"),
                   max.depth = 2, 
                   eta = 1, 
                   nthread = 4, 
                   nround = 50, # number of trees used for model building
                   watchlist = watchlist, 
                   objective = "binary:logistic")

features = colnames(matrix_train)
importance_matrix_2 <- xgb.importance(features, model = bst_2)
print(importance_matrix_2)

xgb.ggplot.importance(importance_matrix_2) +
  theme_minimal()

pred_2 <- predict(bst_2, xgb_test_matrix)

result_2 <- data.frame(case_ID = rownames(val_test_data),
                       outcome = val_test_data$outcome, 
                       label = label, 
                       prediction_p_death = round(pred_2, digits = 2),
                       prediction = as.integer(pred_2 > 0.5),
                       prediction_eval = ifelse(as.integer(pred_2 > 0.5) != label, "wrong", "correct"))
result_2
err <- as.numeric(sum(as.integer(pred_2 > 0.5) != label))/length(label)
print(paste("test-error =", round(err, digits = 2)))

#xgboost using caret - 17 correct
set.seed(27)
model_xgb_null <-train(outcome ~ .,
                       data=val_train_data[, -1],
                       method="xgbTree",
                       preProcess = NULL,
                       trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_xgb_null, val_test_data[, -1]), val_test_data$outcome)

#scaling and centering - 17
set.seed(27)
model_xgb_sc_cen <-train(outcome ~ .,
                         data=val_train_data[, -1],
                         method="xgbTree",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_xgb_sc_cen, val_test_data[, -1]), val_test_data$outcome)

#boxcox 
set.seed(27)
model_xgb_BoxCox <-train(outcome ~ .,
                         data=val_train_data[, -1],
                         method="xgbTree",
                         preProcess = "BoxCox",
                         trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_xgb_BoxCox, val_test_data[, -1]), val_test_data$outcome)

#PCA
set.seed(27)
model_xgb_pca <-train(outcome ~ .,
                      data=val_train_data[, -1],
                      method="xgbTree",
                      preProcess = "pca",
                      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
confusionMatrix(predict(model_xgb_pca, val_test_data[, -1]), val_test_data$outcome)