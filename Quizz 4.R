library(ElemStatLearn)
library(caret)
library(doMC)
registerDoMC(cores = 2)

data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

# gbm and rf models are both trained on vowel.train. Their accuracy is evaluated
# on vowel.test. Then predicts on train from both models are used to generate
# the confusion matrix.

set.seed(33833)
mod11 <- train(y ~.,method="gbm", data=vowel.train, verbose = F)
set.seed(33833)
mod22 <- train(y ~.,method="rf", data=vowel.train, trControl = trainControl(method="cv"), number=3)

pred11 <- predict(mod11,vowel.test[,-1])
confusionMatrix(vowel.test$y, pred11) # 0.5238
pred22 <- predict(mod22,vowel.test[,-1])
confusionMatrix(vowel.test$y, pred22) # 0.5823

confusionMatrix(pred11, pred22) # 0.7056

# RF Accuracy = 0.6082
# 
# GBM Accuracy = 0.5152
# 
# Agreement Accuracy = 0.6361

# Question 2
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
ctrl <- trainControl(allowParallel = T)
mod2_1 <- train(diagnosis ~.,method="rf", data=training, trControl = trainControl(method="cv", allowParallel = T), number=3)
mod2_2 <- train(diagnosis ~.,method="gbm", data=training, verbose = F, trControl = ctrl)
mod2_3 <- train(diagnosis ~.,method="lda", data=training, verbose = F, trControl = ctrl)

pred2_1 <- predict(mod2_1, testing)
pred2_2 <- predict(mod2_2, testing)
pred2_3 <- predict(mod2_3, testing)

confusionMatrix(testing$diagnosis, pred2_1)$overall
confusionMatrix(testing$diagnosis, pred2_2)$overall
confusionMatrix(testing$diagnosis, pred2_3)$overall

predDF <- data.frame(pred2_1, pred2_2, pred2_3, diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~ ., method="rf", data=predDF, trControl = trainControl(method="cv", allowParallel = T), number=3)
combPred <- predict(combModFit, predDF)
# combPred <- predict(combModFit, testing)
confusionMatrix(testing$diagnosis, combPred)$overall # 0.8170732. In the answer there is 0.80

# Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

# Question 3
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
mod3_1 <- train(CompressiveStrength ~ ., method = "lasso", data = training, trControl = ctrl)
# mod3_1 <- train(CompressiveStrength ~ ., method = "enet", data = training, trControl = ctrl)
plot(mod3_1$finalModel, xvar = "penalty", use.color = T) # Cement

# Question 4

setwd("~/Documents/GitHub/Practical Machine Learning/")
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
fname <- "gaData.csv"

if(!file.exists(fname)) {
    download.file(url, fname, method = "curl")
}

library(lubridate) # For year() function below
library(forecast)
dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)

mod4_1 <- bats(tstrain)
fc <- forecast(mod4_1, level = 95, h = length(tstest))
plot(fc)
pred <- data.frame(lower = fc$lower, upper = fc$upper, mean = fc$mean, actual = tstest)
mean(pred$actual < pred$upper) # 0.9617021

# Question 5

library(e1071)

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
mod5_1 <- svm(CompressiveStrength ~ ., data = training)
# pred5_1 <- predict(mod5_1, subset(testing, select = -CompressiveStrength))
pred5_1 <- predict(mod5_1, testing)
sqrt(sum((pred5_1 - testing$CompressiveStrength)^2)/length(pred5_1)) # 6.715009
sqrt(var(pred5_1 - testing$CompressiveStrength)) # 6.72605
sd(pred5_1 - testing$CompressiveStrength) # 6.72605

