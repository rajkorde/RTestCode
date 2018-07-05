library(DALEX)
library(caret)
data("apartments")

set.seed(123)

regr_rf <- train(m2.price~., data = apartments, method = "rf", ntree = 100)
regr_gbm <- train(m2.price~., data = apartments, method = "gbm")
regr_nn <- train(m2.price~., data = apartments, method = "nnet", 
                 linout = TRUE, preProcess = c("center", "scale"),
                 maxit = 500,
                 tuneGrid = expand.grid(size = 2, decay = 0),
                 trControl = trainControl(method = "none", seeds = 1))

data("apartmentsTest")
explainer_regr_rf <- explain(regr_rf, label = "rf", 
                             data = apartmentsTest,
                             y = apartmentsTest$m2.price)

explainer_regr_gbm <- explain(regr_gbm, label = "gbm", 
                             data = apartmentsTest,
                             y = apartmentsTest$m2.price)

explainer_regr_nn <- explain(regr_nn, label = "nn", 
                             data = apartmentsTest,
                             y = apartmentsTest$m2.price)

# Model performance

mp_regr_rf <- model_performance(explainer_regr_rf)
mp_regr_gbm <- model_performance(explainer_regr_gbm)
mp_regr_nn <- model_performance(explainer_regr_nn)

plot(mp_regr_rf, mp_regr_nn, mp_regr_gbm)
plot(mp_regr_rf, mp_regr_nn, mp_regr_gbm, geom = "boxplot")

# variable importance
vi_regr_rf <- variable_importance(explainer_regr_rf, loss_function = loss_root_mean_square)
vi_regr_gbm <- variable_importance(explainer_regr_gbm, loss_function = loss_root_mean_square)
vi_regr_nn <- variable_importance(explainer_regr_nn, loss_function = loss_root_mean_square)

plot(vi_regr_rf, vi_regr_gbm, vi_regr_nn)

pdp_regr_rf  <- variable_response(explainer_regr_rf, variable =  "construction.year", type = "pdp")
pdp_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "construction.year", type = "pdp")
pdp_regr_nn  <- variable_response(explainer_regr_nn, variable =  "construction.year", type = "pdp")

plot(pdp_regr_rf, pdp_regr_gbm, pdp_regr_nn)

ale_regr_rf  <- variable_response(explainer_regr_rf, variable =  "construction.year", type = "ale")
ale_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "construction.year", type = "ale")
ale_regr_nn  <- variable_response(explainer_regr_nn, variable =  "construction.year", type = "ale")

plot(ale_regr_rf, ale_regr_gbm, ale_regr_nn)

mpp_regr_rf  <- variable_response(explainer_regr_rf, variable =  "district", type = "factor")
mpp_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "district", type = "factor")
mpp_regr_nn  <- variable_response(explainer_regr_nn, variable =  "district", type = "factor")

plot(mpp_regr_rf, mpp_regr_gbm, mpp_regr_nn)

library(imlplots)
print(summarizeColumns(b)[, -c(5, 6, 7)], digits = 4)

boston.task = makeRegrTask(data = b, target = "medv")
rf.mod = train("regr.randomForest", boston.task)
glm.mod = train("regr.glm", boston.task)
mod.list = list(rf.mod, glm.mod)
imlplots(data = boston, task = boston.task, models = mod.list)
