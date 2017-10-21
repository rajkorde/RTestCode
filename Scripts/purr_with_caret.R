# http://rsangole.netlify.com/post/pur-r-ify-your-carets/

library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(caret)
library(mlbench)
data("BostonHousing")

response <- 'medv'
preds.original <- colnames(BostonHousing[,1:13])

# boxcox
prepTrain <- preProcess(x = BostonHousing[, preds.original], method = c("BoxCox"))
boxcoxed <- predict(prepTrain, newdata = BostonHousing[, preds.original])
colnames(boxcoxed) <- paste0(colnames(boxcoxed),'.boxed')
preds.boxcoxed <- colnames(boxcoxed)

# squaring
squared <- (BostonHousing[,c(1:3,5:13)])^2
colnames(squared) <- paste0(colnames(squared),'.sq')
preds.sq <- colnames(squared)

BostonHousing %<>% 
  cbind(boxcoxed,squared)

BostonHousing %<>%
  map_df(.f = ~as.numeric(.x)) %>% as.data.frame()

pred_varsets <- ls(pattern = "preds")

# create a starter data frame
num_var_select <- length(pred_varsets)

list(BostonHousing) %>%
  rep(num_var_select) %>%
  enframe(name = "id", value = "rawdata") %>%
  mutate(pred_varsets = pred_varsets) -> starter_df

filterColumns <- function(x,y){
  x[,(colnames(x) %in% eval(parse(text=y)))]
}

# Create X and Y columns
starter_df %<>% 
  transmute(
    id,
    pred_varsets,
    train.X = map2(rawdata, pred_varsets,  ~ filterColumns(.x, .y)),
    train.Y = map(rawdata, ~ .x$medv)
  )

rpartModel <- function(X, Y) {
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv",
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'rpart2',
    trControl = ctrl,
    tuneGrid = data.frame(maxdepth=c(2,3,4,5)),
    preProc = c('center', 'scale')
  )
}
xgbTreeModel <- function(X,Y){
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv",
    number = 5
  )
  train(
    x=X,
    y=Y,
    method = 'xgbTree',
    trControl = ctrl,
    tuneGrid = expand.grid(nrounds = c(100,300,500), 
                           max_depth = c(2,4,6) ,
                           eta = 0.1,
                           gamma = 1, 
                           colsample_bytree = 1, 
                           min_child_weight = 1, 
                           subsample = 1),
    preProc = c('center', 'scale')
  )
}

model_list <- list(rpartModel=rpartModel,
                   xgbModel=xgbTreeModel) %>%
  enframe(name = 'modelName',value = 'model')

# create data model combinations

train_df <- starter_df[rep(1:nrow(starter_df),nrow(model_list)),]

train_df %<>% bind_cols(
  model_list[rep(1:nrow(model_list), nrow(starter_df)),] %>% arrange(modelName)
  ) %>% mutate(id = 1:nrow(.))

# solve the models
train_df %<>%
  mutate(params = map2(train.X, train.Y, ~list(X = .x, Y = .y)),
         modelFits = invoke_map(model, params))

# extract results
train_df %<>% 
  mutate(
    RMSE=map_dbl(modelFits,~max(.x$results$RMSE)),
    RMSESD=map_dbl(modelFits,~max(.x$results$RMSESD)),
    Rsq=map_dbl(modelFits,~max(.x$results$Rsquared)),
    bestTune=map(modelFits,~.x$bestTune)
  )

library(lattice)
dotplot(Rsq~pred_varsets|modelName, train_df)

train_df %>% 
  ggplot(aes(x=pred_varsets,color=modelName))+
  geom_point(aes(y=RMSE),size=2)+
  geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.15)

plot(train_df$modelFits[train_df$modelName=='xgbModel' & train_df$pred_varsets=='preds.original'][[1]])