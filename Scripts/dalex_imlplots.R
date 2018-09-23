library(DALEX)
library(caret)

data("apartments")
data("apartmentsTest")

set.seed(123)

regr_rf <- train(m2.price~., data = apartments, method = "rf", ntree = 100)
regr_gbm <- train(m2.price~., data = apartments, method = "gbm")
regr_nn <- train(m2.price~., data = apartments, method = "nnet", 
                 linout = TRUE, preProcess = c("center", "scale"),
                 maxit = 500,
                 tuneGrid = expand.grid(size = 2, decay = 0),
                 trControl = trainControl(method = "none", seeds = 1))


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

# ceteris paribus plot
# ceteris paribus - with other conditions remaining the same.
# explore local behavior of a model

library("ceterisParibus")

explainer_rf <- explain(regr_rf,
                        data = apartmentsTest, 
                        y = apartmentsTest$m2.price)
apartments_small_1 <- apartmentsTest[1,]
apartments_small_2 <- select_sample(apartmentsTest, n = 20)

cp_rf_y1 <- ceteris_paribus(explainer_rf, apartments_small_1, 
                            y = apartments_small_1$m2.price)
cp_rf_y2 <- ceteris_paribus(explainer_rf, apartments_small_2, 
                            y = apartments_small_2$m2.price)

plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
     show_residuals = TRUE, alpha = 0.5, size_points = 3,
     alpha_points = 1, size_rugs = 0.5)
plot(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
     alpha = 0.2, alpha_points = 1, size_rugs = 0.5)
#plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#     show_residuals = TRUE, alpha = 0.5, 
#     color = "construction.year", size_points = 3)
#plot(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#     size = 0.5, alpha = 0.5, color = "surface")

# wangkardu plots
# explore local goodness of fit

cr_rf <- local_fit(explainer_rf, observation = apartments_small_1,
                   select_points = 0.002, selected_variable = "surface")
plot(cr_rf)

# breakdown plots - distribution changes
br_rf <- prediction_breakdown(explainer_rf, observation = apartments_small_1)
plot(br_rf)


#modelDown
#devtools::install_github("MI2DataLab/modelDown")
library(modelDown)
modelDown::modelDown(explainer_regr_rf, explainer_regr_gbm)


#devtools::install_github('compstat-lmu/imlplots')
library(imlplots)

apartments_task <- makeRegrTask(data = apartments, target = "m2.price")
rf_model <- train("regr.randomForest", apartments_task)
glm_model <- train("regr.glm", apartments_task)
model_list <- list(rf_model, glm_model)
imlplots(data = apartments, task = apartments_task, models = model_list)
