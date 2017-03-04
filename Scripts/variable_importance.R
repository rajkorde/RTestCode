#http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/

library(mlbench)
data(Ozone, package="mlbench")

inputData <- Ozone
names(inputData) <- c("Month", "Day_of_month", "Day_of_week", "ozone_reading", 
                      "pressure_height", "Wind_speed", "Humidity", 
                      "Temperature_Sandburg", "Temperature_ElMonte", 
                      "Inversion_base_height", "Pressure_gradient", 
                      "Inversion_temperature", "Visibility")

#impute using knn
library(DMwR)

inputData <- knnImputation(inputData)


### Segregate all continuous and categorical variables

# Place all continuous vars in inputData_cont
inputData_cont <- inputData[, c("pressure_height", "Wind_speed", "Humidity", 
    "Temperature_Sandburg", "Temperature_ElMonte", "Inversion_base_height", 
    "Pressure_gradient", "Inversion_temperature", "Visibility")]

# Place all categorical variables in inputData_cat
inputData_cat <- inputData[, c("Month", "Day_of_month", "Day_of_week")]

# create the response data frame
inputData_response <- data.frame(ozone_reading=inputData[, "ozone_reading"])  # response variable as a dataframe
response_name <- "ozone_reading"  # name of response variable
response <- inputData[, response_name]  # response variable as a vector

#randomforest
library(party)
cf1 <- cforest(ozone_reading ~ ., data = inputData, control = cforest_unbiased(mtry = 2, ntree = 50))
varimp(cf1, conditional = TRUE)
varimpAUC(cf1)


#relaimpo
library(relaimpo)
lmMod <- lm(ozone_reading ~ ., data = inputData)
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)
sort(relImportance$lmg, decreasing= TRUE)


#MARS (Earth packages)
library(earth)
marsModel <- earth(ozone_reading ~ ., data=inputData) # build model
ev <- evimp (marsModel) # estimate variable importance
plot(ev)


#stepwise regression
base.mod <- lm(ozone_reading ~ 1, data = inputData)
all.mod <- lm(ozone_reading ~ . , data= inputData)
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod),
                direction = "both", trace = 1, steps = 1000)
shortlistedVars <- names(unlist(stepMod[[1]]))
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]

#Boruta
library(Boruta)
boruta_output <- Boruta(response ~ ., data = na.omit(inputData), doTrace = 2)

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])


#information value and weight of evidence
library(devtools)
install_github("tomasgreif/woe")
install_github("tomasgreif/riv", force = TRUE)

library(woe)
library(riv)

iv_df <- iv.mult(german_data, y="gb", summary=TRUE, verbose=TRUE)
iv <- iv.mult(german_data, y="gb", summary=FALSE, verbose=TRUE)

iv.plot.summary(iv_df)

german_data_iv <- iv.replace.woe(german_data, iv, verbose=TRUE)  # add woe variables to original data frame.
plot(german_data_iv)
