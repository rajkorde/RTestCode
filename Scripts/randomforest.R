library(readr)
library(lubridate)
library(dplyr)
library(randomForest)
library(rpart)
library(caret)
library(xgboost)

train <- read_csv("Data/SL training.csv")
test <- read_csv("Data/SL validation.csv")

train$Type <- "Train"
test$Type <- "Test"

train <- bind_rows(train, test)

train <- mutate(train, PresentTime = mdy_hms(PresentTime),
            FirstUpdatedDate = mdy_hms(FirstUpdatedDate),
            OSOOBEDateTime = mdy_hms(OSOOBEDateTime),
            ProductActivationTime = mdy_hms(ProductActivationTime))
train <- mutate(train, PresentTimeDay = weekdays(PresentTime),
                FirstUpdatedDateDay = weekdays(FirstUpdatedDate),
                OSOOBEDateTimeDay = weekdays(OSOOBEDateTime),
                ProductActivationTimeDay = weekdays(ProductActivationTime)) %>%
  select(-c(AnonymizedCampaign, AnonymizedDeviceID))
train$HasClicked <- ifelse(train$HasClicked =="True", TRUE, FALSE)
train <- mutate(train, DefaultInternetBrowser = as.factor(DefaultInternetBrowser),
                ISOCountryShortName = as.factor(ISOCountryShortName),
                IsVirtualDevice = as.factor(IsVirtualDevice),
                IsDomainJoined = as.factor(IsDomainJoined),
                IsCommercial = as.factor(IsCommercial),
                IsEducation = as.factor(IsEducation),
                IsGamingDevice = as.factor(IsGamingDevice),
                IsTouchEnabled = as.factor(IsTouchEnabled),
                IsPenCapable = as.factor(IsPenCapable),
                HasFrontFacingCamera = as.factor(HasFrontFacingCamera),
                HasRearFacingCamera = as.factor(HasRearFacingCamera),
                DeviceTimezone = as.factor(DeviceTimezone),
                GamerPCClassification = as.factor(GamerPCClassification),
                GamerSegment = as.factor(GamerSegment),
                PresentTimeDay = as.factor(PresentTimeDay),
                FirstUpdatedDateDay = as.factor(FirstUpdatedDateDay),
                DeviceTimezone = as.factor(DeviceTimezone),
                OSOOBEDateTimeDay = as.factor(OSOOBEDateTimeDay),
                ProductActivationTimeDay = as.factor(ProductActivationTimeDay)) %>%
  select(-c(NumberOfExternalDrives, NumberOfInternalDrives,
            NumberofInternalDisplays, NumberofExternalDisplays, 
            IsAlwaysOnAlwaysConnectedCapable, DisplayLanguage))
train$IsGamingDevice <- NULL 

train <- mutate(train, PAge = as.numeric(difftime(mdy("2/3/2017"), FirstUpdatedDate, unit = "days")),
                DAge = as.numeric(difftime(mdy("2/3/2017"), OSOOBEDateTime, unit = "days")),
                PHour = hour(PresentTime)) %>%
  select(-c(PresentTime, ProductActivationTime, FirstUpdatedDate, OSOOBEDateTime))
#nzv(train)

t <- filter(train, Type == "Train") %>% select(-c(Type, ISOCountryShortName, DeviceTimezone))
test <- filter(train, Type == "Test") %>% select(-c(Type, ISOCountryShortName, DeviceTimezone))

save(train, t, test, file = "Objects/CTR.Rdata")

fit <- glm(HasClicked ~ ., data = t)
dat <- predict(fit, newdata = test)

rpfit <- rpart(HasClicked ~ ., data=t)
dat <- predict(rpfit, newdata = test)

cv.res <- xgb.cv(data = select(t, -HasClicked), label = t$HasClicked, nfold = 5,
                 nrounds = 2, objective = "binary:logistic")

xgb <- xgboost(data = data.matrix(select(t, -HasClicked)), 
               label = t$HasClicked, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               objective = "binary:logistic",
               nthread = 3
)

x1 <- predict(xgb, newdata = data.matrix(select(test, -HasClicked)))

rpv <- predict(rpfit, newdata = test, type = "prob")

save(x1, xgb, file = "Objects/xgb.Rdata")

x2 <- (x1 > 0.095)

confusionMatrix(x2, test$HasClicked)

library(ROCR)
g <- prediction(x1, test$HasClicked)
g1 <- performance(g, "auc")
auc <- unlist(slot(auc, "y.values"))


