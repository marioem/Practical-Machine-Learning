library(caret)
library(randomForest)
library(doMC)

#registerDoMC(cores = 4)

trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv")) {
    download.file(trainurl, destfile = "pml-training.csv", method = "curl")
}

if(!file.exists("pml-testing.csv")) {
    download.file(testurl, destfile = "pml-testing.csv", method = "curl")
}

train <- read.csv("pml-training.csv",stringsAsFactors = F)
test <- read.csv("pml-testing.csv",stringsAsFactors = F)
# some variables which seem to be numerical are read in as character
cv <- names(train[,sapply(train, class) == "character"])
cv <- cv[-c(1,2,3,37)]

for(i in cv){
    train[,i] <- as.numeric(train[,i])
}

cmtr <- colMeans(apply(train,2, is.na))
cmtst <- colMeans(apply(test,2, is.na))

missing <- rbind(cmtr, cmtst)
# Check if the missing data affects the same column (if yes, the result is 0)
difcol <- sum(xor(cmtr, cmtst))

if(difcol == 0){
    train <- train[,missing[1,]*missing[2,] == 0]
    test <- test[,missing[1,]*missing[2,] == 0]
} else
    cat("Variables with missing data don't coincide between train and quizz set")

predtrain <- train[,-c(1:7)]
predtrain[,which(names(predtrain) == "classe")] <- as.factor(predtrain[,which(names(predtrain) == "classe")])
predtest <- test[,-c(1:7)]

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]

ctrl <- trainControl(method = "cv", allowParallel = F)
set.seed(728665723)
modelFit3 <- train(ptrTraining$classe ~ .,method="rf", preProcess="pca", 
                   trControl = ctrl, data=ptrTraining)
