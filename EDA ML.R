library(VIM)
library(ggplot2)
library(caret)
library(dplyr)
library(doMC)
library(GGally)
registerDoMC(cores = 4)

score <- function(pred) {
    correct <- c("B", "A", "B", "A", "A", "E", "D", "B", "A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")
    score <- sum(pred == correct)
    score
}

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

dim(train)
dim(test)

# some variables which seem to be numerical are read in as character
cv <- names(train[,sapply(train, class) == "character"])
cv <- cv[-c(1,2,3,37)]

for(i in cv){
    print(i)
    train[,i] <- as.numeric(train[,i])
}

cmtr <- colMeans(apply(train,2, is.na))
cmtst <- colMeans(apply(test,2, is.na))
# There is a pattern in missing data. 
unique(cmtr) # data is either complete or between 98% and 100% is missing
unique(cmtst)  # data is either complete or 100% is missing

sum(cmtr > 0) # 100 variables have missing data
sum(cmtst > 0) # 100 variables have missing data

# As in the test data there are 100 variables which miss 100% data we remove them
# as they will not provide any value

missing <- rbind(cmtr, cmtst)
# Check if the missing data affects the same column (if yes, the result is 0)
difcol <- sum(xor(cmtr, cmtst))

if(difcol == 0){
    train <- train[,missing[1,]*missing[2,] == 0]
    test <- test[,missing[1,]*missing[2,] == 0]
} else
    cat("Variables with missing data don't coincide between train and test set")

# First approach - we assume that names, time stamps, etc. are not necessary for the model
# Note: name could be useful for quizz prediction, but not for general prediction
# predtrain <- train
predtrain <- train[,-c(1:7)]
predtrain[,which(names(predtrain) == "classe")] <- as.factor(predtrain[,which(names(predtrain) == "classe")])
# predtest <- test
predtest <- test[,-c(1:7)]

dim(predtrain)
dim(predtest)

# Optional experimenting with some derivative variables - magnitudes of the vector for which cartesian coordinates were recorded
predtrain <- predtrain %>% mutate(gyros_belt_mag = sqrt(gyros_belt_x^2 + gyros_belt_y^2 + gyros_belt_z^2)) %>% 
                                  mutate(accel_belt_mag = sqrt(accel_belt_x^2 + accel_belt_y^2 + accel_belt_z^2)) %>% 
                                  mutate(magnet_belt_mag = sqrt(magnet_belt_x^2 + magnet_belt_y^2 + magnet_belt_z^2)) %>% 
                                  mutate(gyros_arm_mag = sqrt(gyros_arm_x^2 + gyros_arm_y^2 + gyros_arm_z^2)) %>% 
                                  mutate(accel_arm_mag = sqrt(accel_arm_x^2 + accel_arm_y^2 + accel_arm_z^2)) %>% 
                                  mutate(magnet_arm_mag = sqrt(magnet_arm_x^2 + magnet_arm_y^2 + magnet_arm_z^2)) %>% 
                                  mutate(gyros_dumbbell_mag = sqrt(gyros_dumbbell_x^2 + gyros_dumbbell_y^2 + gyros_dumbbell_z^2)) %>% 
                                  mutate(accel_dumbbell_mag = sqrt(accel_dumbbell_x^2 + accel_dumbbell_y^2 + accel_dumbbell_z^2)) %>% 
                                  mutate(magnet_dumbbell_mag = sqrt(magnet_dumbbell_x^2 + magnet_dumbbell_y^2 + magnet_dumbbell_z^2)) %>% 
                                  mutate(gyros_forearm_mag = sqrt(gyros_forearm_x^2 + gyros_forearm_y^2 + gyros_forearm_z^2)) %>% 
                                  mutate(accel_forearm_mag = sqrt(accel_forearm_x^2 + accel_forearm_y^2 + accel_forearm_z^2)) %>% 
                                  mutate(magnet_forearm_mag = sqrt(magnet_forearm_x^2 + magnet_forearm_y^2 + magnet_forearm_z^2))

predtest <- predtest %>% mutate(gyros_belt_mag = sqrt(gyros_belt_x^2 + gyros_belt_y^2 + gyros_belt_z^2)) %>% 
    mutate(accel_belt_mag = sqrt(accel_belt_x^2 + accel_belt_y^2 + accel_belt_z^2)) %>% 
    mutate(magnet_belt_mag = sqrt(magnet_belt_x^2 + magnet_belt_y^2 + magnet_belt_z^2)) %>% 
    mutate(gyros_arm_mag = sqrt(gyros_arm_x^2 + gyros_arm_y^2 + gyros_arm_z^2)) %>% 
    mutate(accel_arm_mag = sqrt(accel_arm_x^2 + accel_arm_y^2 + accel_arm_z^2)) %>% 
    mutate(magnet_arm_mag = sqrt(magnet_arm_x^2 + magnet_arm_y^2 + magnet_arm_z^2)) %>% 
    mutate(gyros_dumbbell_mag = sqrt(gyros_dumbbell_x^2 + gyros_dumbbell_y^2 + gyros_dumbbell_z^2)) %>% 
    mutate(accel_dumbbell_mag = sqrt(accel_dumbbell_x^2 + accel_dumbbell_y^2 + accel_dumbbell_z^2)) %>% 
    mutate(magnet_dumbbell_mag = sqrt(magnet_dumbbell_x^2 + magnet_dumbbell_y^2 + magnet_dumbbell_z^2)) %>% 
    mutate(gyros_forearm_mag = sqrt(gyros_forearm_x^2 + gyros_forearm_y^2 + gyros_forearm_z^2)) %>% 
    mutate(accel_forearm_mag = sqrt(accel_forearm_x^2 + accel_forearm_y^2 + accel_forearm_z^2)) %>% 
    mutate(magnet_forearm_mag = sqrt(magnet_forearm_x^2 + magnet_forearm_y^2 + magnet_forearm_z^2))

# Check for near-zero variance
nzv <- nearZeroVar(predtrain, saveMetrics= TRUE)
nzv # none is nzv

# Check for correlated predictors
descrCor <- cor(predtrain[,-which(names(predtrain) == "classe")])
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)

predtrain2 <- predtrain[,-highlyCorDescr]
predtest2 <- predtest[,-highlyCorDescr]
descrCor <- cor(predtrain2[,-which(names(predtrain2) == "classe")])
summary(descrCor[upper.tri(descrCor)])

comboInfo <- findLinearCombos(predtrain2[,-which(names(predtrain2) == "classe")])
comboInfo # list to remove - NULL

# Check the PCA decomposition of the reduced data set

preProc <- preProcess(predtrain2[,-which(names(predtrain2) == "classe")], method = "pca")
preProc # 23 components to capture 95% of the variance

# predtest probably is only for the quizz, so we partition the data into train and test
# set out of predtrain

#####################################
# - predtrain split into training 75% and test 25%
#   bootstrapped, 25 repetitions
#   PCA preprocessing

inTrain <- createDataPartition(y=predtrain$classe, p=0.75, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

M <- abs(cor(ptrTraining[,-53]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

corrplot(M,method = "ellipse", order="AOE",type="upper",tl.pos="d")
corrplot(M, order="AOE", tl.srt=60)
corrplot(M, order="AOE", type="upper", diag=FALSE)

set.seed(728665723)
modelFit1 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",data=ptrTraining)
# Random Forest 
# 
# 14718 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 14718, 14718, 14718, 14718, 14718, 14718, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9634265  0.9537124  0.003187379  0.004041935
#     27    0.9460841  0.9317554  0.007608189  0.009660524
#     52    0.9467776  0.9326376  0.007878605  0.009991989
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat1 <- confusionMatrix(ptrTesting$classe,predict(modelFit1,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#           A 1388    4    0    1    2
#           B   14  916   15    2    2
#           C    0    8  842    4    1
#           D    0    0   34  770    0
#           E    1    4    2    2  892
# 
# Overall Statistics
# 
#            Accuracy : 0.9804          
#              95% CI : (0.9761, 0.9841)
# No Information Rate : 0.2861          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9752          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9893   0.9828   0.9429   0.9884   0.9944
# Specificity            0.9980   0.9917   0.9968   0.9918   0.9978
# Pos Pred Value         0.9950   0.9652   0.9848   0.9577   0.9900
# Neg Pred Value         0.9957   0.9960   0.9874   0.9978   0.9988
# Prevalence             0.2861   0.1900   0.1821   0.1588   0.1829
# Detection Rate         0.2830   0.1868   0.1717   0.1570   0.1819
# Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
# Balanced Accuracy      0.9937   0.9873   0.9698   0.9901   0.9961

t(round(prop.table(confmat1$table, 2),2))

quizztest <- predict(modelFit1, predtest)

# [1] B A B A A E D B A A B C B A E E A B B B
# Levels: A B C D E
# 20/20

#####################################
# - predtrain entirely used for training
#   bootstrapped, 25 repetitions
#   PCA preprocessing

set.seed(728665723)
modelFit2 <- train(predtrain$classe ~ .,method="rf",preProcess="pca",data=predtrain)
# Random Forest 
# 
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 19622, 19622, 19622, 19622, 19622, 19622, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD  
#      2    0.9646774  0.9545002  0.02634822   0.03325086
#     27    0.9542450  0.9408494  0.01931727   0.02434642
#     52    0.9540000  0.9405308  0.01911967   0.02409737
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 
quizztest <- predict(modelFit2, predtest)

# [1] B A B A A E D B A A B B B A E E A B B B
# Levels: A B C D E
# 19/20

###########################
# - rf z 10% danych na test
#   10-fold cv
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit3 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17663 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 14237, 14237, 14237, 14236, 14237, 14237, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9786331  0.9709535  0.003357730  0.004574301
#     27    0.9702262  0.9595435  0.004087566  0.005566700
#     52    0.9702261  0.9595437  0.004452272  0.006067148
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat3 <- confusionMatrix(ptrTesting$classe,predict(modelFit3,ptrTesting))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   A   B   C   D   E
# A 555   1   1   0   1
# B   5 373   0   0   1
# C   0   3 132   0   2
# D   0   0   1  96  19
# E   0   0   0   1 564
# 
# Overall Statistics
# 
#            Accuracy : 0.9801          
#              95% CI : (0.9724, 0.9861)
# No Information Rate : 0.3345          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9729          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9911   0.9894  0.98507  0.98969   0.9608
# Specificity            0.9975   0.9956  0.99692  0.98794   0.9991
# Pos Pred Value         0.9946   0.9842  0.96350  0.82759   0.9982
# Neg Pred Value         0.9958   0.9971  0.99876  0.99939   0.9807
# Prevalence             0.3191   0.2148  0.07635  0.05527   0.3345
# Detection Rate         0.3162   0.2125  0.07521  0.05470   0.3214
# Detection Prevalence   0.3179   0.2160  0.07806  0.06610   0.3219
# Balanced Accuracy      0.9943   0.9925  0.99100  0.98881   0.9800


t(round(prop.table(confmat3$table, 2),2))

quizztest <- predict(modelFit3, predtest)
# [1] B A A A A E E B A A A A B A E E A B B B
# Levels: A B C D E
# 16/20



# Second approach - we assume that time stamps, are necessary for the model
# Note: name could be useful for quizz prediction, but not for general prediction
predtrain3 <- train[,-c(1,2,5:7)]
predtest3 <- test[,-c(1,2,5:7)]

###########################
# - rf z 10% danych na test
#   10-fold cv
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain3$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain3[inTrain,]
ptrTesting <- predtrain3[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit4 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17662 samples
# 54 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (54), centered (54), scaled (54) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 15897, 15895, 15895, 15896, 15895, 15895, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9806935  0.9755762  0.004507328  0.005705205
#     28    0.9679539  0.9594593  0.004327552  0.005482963
#     54    0.9680107  0.9595295  0.004368051  0.005534833
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat4 <- confusionMatrix(ptrTesting$classe,predict(modelFit4,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 555   0   2   1   0
#          B   5 373   1   0   0
#          C   2   4 334   2   0
#          D   0   0   5 316   0
#          E   0   1   0   3 356
# 
# Overall Statistics
# 
#            Accuracy : 0.9867          
#              95% CI : (0.9806, 0.9913)
# No Information Rate : 0.2867          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9832          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9875   0.9868   0.9766   0.9814   1.0000
# Specificity            0.9979   0.9962   0.9951   0.9969   0.9975
# Pos Pred Value         0.9946   0.9842   0.9766   0.9844   0.9889
# Neg Pred Value         0.9950   0.9968   0.9951   0.9963   1.0000
# Prevalence             0.2867   0.1929   0.1745   0.1643   0.1816
# Detection Rate         0.2832   0.1903   0.1704   0.1612   0.1816
# Detection Prevalence   0.2847   0.1934   0.1745   0.1638   0.1837
# Balanced Accuracy      0.9927   0.9915   0.9858   0.9892   0.9988

t(round(prop.table(confmat4$table, 2),2))

quizztest <- predict(modelFit4, predtest3)
quizztest

# [1] B A B A A E D B A A B C B A E E A B B B
# Levels: A B C D E
# 20/20

# - rf z x% danych na test z uwzg. name
# Third approach - we assume that time stamps and name are necessary for the model
# Note: name could be useful for quizz prediction, but not for general prediction
predtrain4 <- train[,-c(1,5:7)]
predtest4 <- test[,-c(1,5:7)]

###########################
# - rf z 10% danych na test
#   10-fold cv
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain4$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain4[inTrain,]
ptrTesting <- predtrain4[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit5 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17662 samples
# 55 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (59), centered (59), scaled (59) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 15897, 15895, 15895, 15896, 15895, 15895, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9822795  0.9775808  0.003285328  0.004157695
#     30    0.9681813  0.9597433  0.004352589  0.005515859
#     59    0.9686905  0.9603901  0.003381427  0.004283118
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat5 <- confusionMatrix(ptrTesting$classe,predict(modelFit5,ptrTesting))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   A   B   C   D   E
# A 555   0   1   2   0
# B   6 365   5   0   3
# C   1   4 337   0   0
# D   0   0   9 311   1
# E   0   0   3   2 355
# 
# Overall Statistics
# 
#            Accuracy : 0.9811          
#              95% CI : (0.9741, 0.9867)
# No Information Rate : 0.2867          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9761          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9875   0.9892   0.9493   0.9873   0.9889
# Specificity            0.9979   0.9912   0.9969   0.9939   0.9969
# Pos Pred Value         0.9946   0.9631   0.9854   0.9688   0.9861
# Neg Pred Value         0.9950   0.9975   0.9889   0.9976   0.9975
# Prevalence             0.2867   0.1883   0.1811   0.1607   0.1832
# Detection Rate         0.2832   0.1862   0.1719   0.1587   0.1811
# Detection Prevalence   0.2847   0.1934   0.1745   0.1638   0.1837
# Balanced Accuracy      0.9927   0.9902   0.9731   0.9906   0.9929

t(round(prop.table(confmat5$table, 2),2))

quizztest <- predict(modelFit5, predtest4)
quizztest

# [1] B A A A A E D B A A B C B A E E A B B B
# Levels: A B C D E
# 19/20

# Revert to first approach, bigger reduction of predictors by means of PCA

###########################
# - rf z 10% danych na test
#   10-fold cv
#   Removed highly correlated predictors
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain2$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain2[inTrain,]
ptrTesting <- predtrain2[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

# ctrl <- trainControl(method = "cv", preProcOptions = list(thresh = 0.8), allowParallel = T)
ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit6 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17663 samples
# 31 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (31), centered (31), scaled (31) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 14237, 14237, 14237, 14236, 14237, 14237, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9771791  0.9689667  0.003271564  0.004461770
#     16    0.9745870  0.9654604  0.003539737  0.004822746
#     31    0.9685818  0.9572896  0.003676371  0.005014160
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat6 <- confusionMatrix(ptrTesting$classe,predict(modelFit6,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 555   3   0   0   0
#          B   7 369   0   0   3
#          C   1   0 132   0   4
#          D   0   0   1 104  11
#          E   0   1   0   3 561
# 
# Overall Statistics
# 
#            Accuracy : 0.9806         
#              95% CI : (0.973, 0.9865)
# No Information Rate : 0.3299         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
#                  Kappa : 0.9737         
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9858   0.9893  0.99248  0.97196   0.9689
# Specificity            0.9975   0.9928  0.99692  0.99272   0.9966
# Pos Pred Value         0.9946   0.9736  0.96350  0.89655   0.9929
# Neg Pred Value         0.9933   0.9971  0.99938  0.99817   0.9849
# Prevalence             0.3208   0.2125  0.07578  0.06097   0.3299
# Detection Rate         0.3162   0.2103  0.07521  0.05926   0.3197
# Detection Prevalence   0.3179   0.2160  0.07806  0.06610   0.3219
# Balanced Accuracy      0.9916   0.9910  0.99470  0.98234   0.9828

t(round(prop.table(confmat6$table, 2),2))

quizztest <- predict(modelFit6, predtest2)
quizztest
#
# [1] B A A A A E E B A A B B B A E E A B B B
# Levels: A B C D E
# 17/20

##################################################
# - gbm, 90/10
#   10-fold cv
#   PCA pre-processing
#   basic cleaned dataset - predtrain

# Check the PCA decomposition of the reduced data set

preProc <- preProcess(predtrain[,-which(names(predtrain) == "classe")], method = "pca")
preProc # 25 components to capture 95% of the variance

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

# ctrl <- trainControl(method = "cv", preProcOptions = list(thresh = 0.8), allowParallel = T)
ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit7 <- train(ptrTraining$classe ~ ., method="gbm",preProcess="pca",trControl = ctrl, data=ptrTraining, verbose = F))
# Stochastic Gradient Boosting 
# 
# 17663 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 14237, 14237, 14237, 14236, 14237, 14237, ... 
# Resampling results across tuning parameters:
#     
#     interaction.depth  n.trees  Accuracy   Kappa      Accuracy SD  Kappa SD  
# 1                   50      0.5816410  0.4026119  0.01939622   0.02825764
# 1                  100      0.6329722  0.4835892  0.01711039   0.02392909
# 1                  150      0.6607264  0.5266206  0.01695815   0.02366133
# 2                   50      0.6762760  0.5469224  0.01595314   0.02251569
# 2                  100      0.7405030  0.6410480  0.01425470   0.01972983
# 2                  150      0.7765350  0.6926189  0.01339315   0.01838168
# 3                   50      0.7284299  0.6224570  0.01490118   0.02047002
# 3                  100      0.7938571  0.7165721  0.01197070   0.01621948
# 3                  150      0.8345040  0.7732848  0.01136973   0.01556835
# 
# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning
# parameter 'n.minobsinnode' was held constant at a value of 10
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage =
#     0.1 and n.minobsinnode = 10. 

confmat7 <- confusionMatrix(ptrTesting$classe,predict(modelFit7,ptrTesting))
t(round(prop.table(confmat7$table, 2),2))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 518  11   6   1  22
#          B  58 281   8   3  29
#          C   8   6 114   0   9
#          D  10   2  10  49  45
#          E  22  31   6  10 496
# 
# Overall Statistics
# 
#            Accuracy : 0.8308         
#              95% CI : (0.8124, 0.848)
# No Information Rate : 0.351          
# P-Value [Acc > NIR] : < 2.2e-16      
# 
#                  Kappa : 0.7677         
# Mcnemar's Test P-Value : 1.119e-11      
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.8409   0.8489  0.79167  0.77778   0.8253
# Specificity            0.9649   0.9312  0.98572  0.96040   0.9402
# Pos Pred Value         0.9283   0.7414  0.83212  0.42241   0.8779
# Neg Pred Value         0.9181   0.9637  0.98146  0.99146   0.9118
# Prevalence             0.3510   0.1886  0.08205  0.03590   0.3425
# Detection Rate         0.2952   0.1601  0.06496  0.02792   0.2826
# Detection Prevalence   0.3179   0.2160  0.07806  0.06610   0.3219
# Balanced Accuracy      0.9029   0.8901  0.88869  0.86909   0.8827

quizztest <- predict(modelFit7, predtest)
quizztest
# [1] B A A A A E E B A A A A B A E E A B B B
# Levels: A B C D E
# 15/20
 
###########################
# - rf z 10% danych na test
#   10-fold 10 times repeated cv
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

# ctrl <- trainControl(method = "cv", preProcOptions = list(thresh = 0.8), allowParallel = T)
ctrl <- trainControl(method = "repeatedcv", repeats = 10, number = 10, allowParallel = T)
set.seed(728665723)
system.time(modelFit8 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca",trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17663 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 14237, 14237, 14237, 14236, 14237, 14237, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9785763  0.9708670  0.003473077  0.004736122
#     27    0.9704850  0.9598840  0.005024031  0.006841305
#     52    0.9705735  0.9600044  0.004910724  0.006690151
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat8 <- confusionMatrix(ptrTesting$classe,predict(modelFit8,ptrTesting))
t(round(prop.table(confmat8$table, 2),2))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 556   1   0   0   1
#          B   7 371   1   0   0
#          C   0   2 132   0   3
#          D   0   0   4 103   9
#          E   0   3   0   5 557
# 
# Overall Statistics
# 
#            Accuracy : 0.9795          
#              95% CI : (0.9717, 0.9856)
# No Information Rate : 0.3248          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9722          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9876   0.9841  0.96350  0.95370   0.9772
# Specificity            0.9983   0.9942  0.99691  0.99211   0.9932
# Pos Pred Value         0.9964   0.9789  0.96350  0.88793   0.9858
# Neg Pred Value         0.9942   0.9956  0.99691  0.99695   0.9891
# Prevalence             0.3208   0.2148  0.07806  0.06154   0.3248
# Detection Rate         0.3168   0.2114  0.07521  0.05869   0.3174
# Detection Prevalence   0.3179   0.2160  0.07806  0.06610   0.3219
# Balanced Accuracy      0.9929   0.9891  0.98021  0.97291   0.9852

quizztest <- predict(modelFit8, predtest)
quizztest
# [1] B A A A A E E B A A B A B A E E A B B B
# Levels: A B C D E
# 17/20
dotPlot(varImp(modelFit))

###########################
# Some playing
# - rf 60/40
#   10-fold cv
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

# ctrl <- trainControl(method = "cv", preProcOptions = list(thresh = 0.8), allowParallel = T)
set.seed(728665723)
system.time(modelFit9 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca", data=ptrTraining))
# Random Forest 
# 
# 11775 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 11775, 11775, 11775, 11775, 11775, 11775, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9393203  0.9144945  0.004512222  0.006247946
#     27    0.9311179  0.9032123  0.006102416  0.008490898
#     52    0.9308302  0.9027974  0.006197442  0.008603478
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat9 <- confusionMatrix(ptrTesting$classe,predict(modelFit9,ptrTesting))
t(round(prop.table(confmat9$table, 2),2))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2206    8    0    0   18
#          B   16 1426    4    0   72
#          C    0    0  500    0   49
#          D    1    0    9  352  105
#          E    0   13    4    4 3060
# 
# Overall Statistics
# 
#            Accuracy : 0.9614          
#              95% CI : (0.9569, 0.9655)
# No Information Rate : 0.4211          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9457          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9924   0.9855  0.96712  0.98876   0.9262
# Specificity            0.9954   0.9856  0.99332  0.98465   0.9954
# Pos Pred Value         0.9884   0.9394  0.91075  0.75375   0.9932
# Neg Pred Value         0.9970   0.9967  0.99767  0.99946   0.9488
# Prevalence             0.2833   0.1844  0.06589  0.04537   0.4211
# Detection Rate         0.2811   0.1817  0.06372  0.04486   0.3900
# Detection Prevalence   0.2844   0.1934  0.06996  0.05951   0.3926
# Balanced Accuracy      0.9939   0.9856  0.98022  0.98671   0.9608

quizztest <- predict(modelFit9, predtest)
quizztest
# [1] B A A A A E D B A A B C B A E E A B B B
# Levels: A B C D E
# 3rd incorrect
dotPlot(varImp(modelFit9))

###########################
# Some playing
# - rf 60/40
#   10-fold CV repeated 10 times
#   PCA pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "repeatedcv", repeats = 10, allowParallel = T)
set.seed(728665723)
system.time(modelFit10 <- train(ptrTraining$classe ~ .,method="rf",preProcess="pca", trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 11775 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52) 
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 10597, 10597, 10599, 10598, 10596, 10598, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9548279  0.9364641  0.005796533  0.008220279
#     27    0.9518301  0.9323701  0.006190985  0.008761623
#     52    0.9516603  0.9321281  0.005692665  0.008055497
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

confmat10 <- confusionMatrix(ptrTesting$classe,predict(modelFit10,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2198   16    1    0   17
#          B   36 1414    7    0   61
#          C    0    3  504    0   42
#          D    2    0   12  359   94
#          E    0   14   13    8 3046
# 
# Overall Statistics
# 
#            Accuracy : 0.9585          
#              95% CI : (0.9538, 0.9628)
# No Information Rate : 0.4154          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9417          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9830   0.9772  0.93855  0.97820   0.9344
# Specificity            0.9939   0.9838  0.99384  0.98556   0.9924
# Pos Pred Value         0.9848   0.9315  0.91803  0.76874   0.9886
# Neg Pred Value         0.9932   0.9948  0.99548  0.99892   0.9551
# Prevalence             0.2849   0.1844  0.06843  0.04677   0.4154
# Detection Rate         0.2801   0.1802  0.06423  0.04575   0.3882
# Detection Prevalence   0.2844   0.1934  0.06996  0.05951   0.3926
# Balanced Accuracy      0.9885   0.9805  0.96620  0.98188   0.9634

t(round(prop.table(confmat10$table, 2),2))

quizztest <- predict(modelFit10, predtest)
quizztest
#
# [1] B A E A A E E B A A B E B A E E A B B B
# Levels: A B C D E
# 17/20

predict(modelFit10, predtest, type = "prob")
dotPlot(varImp(modelFit10))



###########################
# Some playing
# - rf 60/40
#   10-fold CV
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit11 <- train(ptrTraining$classe ~ ., method="rf", trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 11775 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 10597, 10597, 10599, 10598, 10596, 10598, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9762206  0.9666897  0.005125497  0.007250889
#     27    0.9809763  0.9734318  0.003696602  0.005185188
#     52    0.9736720  0.9632169  0.005146063  0.007211867
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 27. 

confmat11 <- confusionMatrix(ptrTesting$classe,predict(modelFit11,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2229    2    0    0    1
#          B   13 1488    0    0   17
#          C    0    6  511    0   32
#          D    0    0    9  408   50
#          E    0   16   10    9 3046
# 
# Overall Statistics
# 
#            Accuracy : 0.979          
#              95% CI : (0.9756, 0.982)
# No Information Rate : 0.4009         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
#                  Kappa : 0.9706         
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9942   0.9841  0.96415  0.97842   0.9682
# Specificity            0.9995   0.9953  0.99481  0.99206   0.9926
# Pos Pred Value         0.9987   0.9802  0.93078  0.87366   0.9886
# Neg Pred Value         0.9977   0.9962  0.99740  0.99878   0.9790
# Prevalence             0.2857   0.1927  0.06754  0.05314   0.4009
# Detection Rate         0.2841   0.1896  0.06512  0.05199   0.3882
# Detection Prevalence   0.2844   0.1934  0.06996  0.05951   0.3926
# Balanced Accuracy      0.9968   0.9897  0.97948  0.98524   0.9804

t(round(prop.table(confmat11$table, 2),2))

quizztest <- predict(modelFit11, predtest)
quizztest
#
# [1] B A B A A E E B A A B E B A E E A B B B
# Levels: A B C D E
# 18/20


###########################
# Some playing
# - rf 90/10
#   10-fold CV
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
system.time(modelFit12 <- train(ptrTraining$classe ~ ., method="rf", trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17662 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 15896, 15895, 15896, 15895, 15895, 15896, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9840342  0.9776941  0.003049367  0.004275935
#     27    0.9870347  0.9819224  0.002853689  0.003987793
#     52    0.9823919  0.9754498  0.003401135  0.004753417
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 27. 

confmat12 <- confusionMatrix(ptrTesting$classe,predict(modelFit12,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 557   1   0   0   0
#          B   3 373   0   0   3
#          C   0   0 134   0   3
#          D   0   0   3 108   5
#          E   0   1   2   1 766
# 
# Overall Statistics
# 
#            Accuracy : 0.9888         
#              95% CI : (0.9831, 0.993)
# No Information Rate : 0.3964         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
#                  Kappa : 0.9844         
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9946   0.9947  0.96403  0.99083   0.9858
# Specificity            0.9993   0.9962  0.99835  0.99568   0.9966
# Pos Pred Value         0.9982   0.9842  0.97810  0.93103   0.9948
# Neg Pred Value         0.9979   0.9987  0.99726  0.99946   0.9908
# Prevalence             0.2857   0.1913  0.07092  0.05561   0.3964
# Detection Rate         0.2842   0.1903  0.06837  0.05510   0.3908
# Detection Prevalence   0.2847   0.1934  0.06990  0.05918   0.3929
# Balanced Accuracy      0.9970   0.9954  0.98119  0.99325   0.9912

t(round(prop.table(confmat12$table, 2),2))

quizztest <- predict(modelFit12, predtest)
quizztest
#
# [1] B A B A A E E B A A B E B A E E A B B B
# Levels: A B C D E
# 18/20

###########################
# Some playing
# - rf 60/40
#   10-fold CV repeated 10 times
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "repeatedcv", repeats = 10, allowParallel = T)
set.seed(728665723)
system.time(modelFit13 <- train(ptrTraining$classe ~ ., method="rf", trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 11775 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 10597, 10597, 10599, 10598, 10596, 10598, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9757624  0.9660519  0.003775879  0.005318856
#     27    0.9809765  0.9734282  0.003382812  0.004745186
#     52    0.9732062  0.9625786  0.004153704  0.005822175
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 27. 

confmat13 <- confusionMatrix(ptrTesting$classe,predict(modelFit13,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2231    1    0    0    0
#          B   11 1502    0    0    5
#          C    0    5  503    2   39
#          D    0    0    7  404 1080
#          E    0    9    5    8 2035
# 
# Overall Statistics
# 
#            Accuracy : 0.8506          
#              95% CI : (0.8426, 0.8585)
# No Information Rate : 0.4026          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.8038          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9951   0.9901  0.97670  0.97585   0.6442
# Specificity            0.9998   0.9975  0.99373  0.85376   0.9953
# Pos Pred Value         0.9996   0.9895  0.91621  0.27096   0.9893
# Neg Pred Value         0.9980   0.9976  0.99836  0.99843   0.8059
# Prevalence             0.2857   0.1933  0.06563  0.05276   0.4026
# Detection Rate         0.2843   0.1914  0.06410  0.05148   0.2593
# Detection Prevalence   0.2844   0.1934  0.06996  0.19001   0.2621
# Balanced Accuracy      0.9975   0.9938  0.98521  0.91480   0.8197

t(round(prop.table(confmat13$table, 2),2))

quizztest <- predict(modelFit13, predtest)
quizztest
#
# [1] B A B A A E E B A A B E B A E E A B B B
# Levels: A B C D E
# 18/20

###########################
# Some playing
# - rf 90/10
#   10-fold CV repeated 10 times
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "repeatedcv", repeats = 10, allowParallel = T)
set.seed(728665723)
system.time(modelFit14 <- train(ptrTraining$classe ~ ., method="rf", trControl = ctrl, data=ptrTraining))
# Random Forest 
# 
# 17662 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 15896, 15895, 15896, 15894, 15896, 15896, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9867626  0.9808772  0.002669880  0.003874354
#     27    0.9889478  0.9840555  0.002620439  0.003792120
#     52    0.9823459  0.9745250  0.003271330  0.004731078
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 27. 

confmat14 <- confusionMatrix(ptrTesting$classe,predict(modelFit14,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 558   0   0   0   0
#          B   1 378   0   0   0
#          C   0   1 135   1   0
#          D   0   1   4 828   0
#          E   0   0   0   7  46
# 
# Overall Statistics
# 
#            Accuracy : 0.9923          
#              95% CI : (0.9874, 0.9957)
# No Information Rate : 0.4265          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.989           
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9982   0.9947  0.97122   0.9904  1.00000
# Specificity            1.0000   0.9994  0.99890   0.9956  0.99634
# Pos Pred Value         1.0000   0.9974  0.98540   0.9940  0.86792
# Neg Pred Value         0.9993   0.9987  0.99781   0.9929  1.00000
# Prevalence             0.2852   0.1939  0.07092   0.4265  0.02347
# Detection Rate         0.2847   0.1929  0.06888   0.4224  0.02347
# Detection Prevalence   0.2847   0.1934  0.06990   0.4250  0.02704
# Balanced Accuracy      0.9991   0.9971  0.98506   0.9930  0.99817

t(round(prop.table(confmat14$table, 2),2))

quizztest <- predict(modelFit14, predtest)
quizztest
#
# [1] B A B A A D D B A A B D B A D D A B B B
# Levels: A B C D E
# 16/20

###########################
# Some playing
# - rf 60/40
#   10-fold CV
#   defined mtry
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
mtry = c(2, floor(sqrt(ncol(ptrTraining) - 1)), floor(ncol(ptrTraining)/2), ncol(ptrTraining) - 1)
system.time(modelFit15 <- train(ptrTraining$classe ~ ., method="rf",
                                trControl = ctrl,
                                data=ptrTraining,
                                tuneGrid = data.frame(mtry = mtry)))
# Random Forest 
# 
# 11775 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 10597, 10597, 10599, 10598, 10597, 10598, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9798733  0.9708696  0.002151023  0.003126633
#      7    0.9834394  0.9760779  0.002694047  0.003896684
#     26    0.9819961  0.9740003  0.003999480  0.005788136
#     52    0.9739293  0.9623451  0.005258005  0.007616862
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 7. 

confmat15 <- confusionMatrix(ptrTesting$classe,predict(modelFit15,ptrTesting))
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2226    6    0    0    0
#          B   13 1483    0   22    0
#          C    0    5  516   28    0
#          D    0    3    9 3317    5
#          E    0    0    0   54  160
# 
# Overall Statistics
# 
#            Accuracy : 0.9815          
#              95% CI : (0.9783, 0.9844)
# No Information Rate : 0.436           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9733          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9942   0.9906  0.98286   0.9696  0.96970
# Specificity            0.9989   0.9945  0.99549   0.9962  0.99297
# Pos Pred Value         0.9973   0.9769  0.93989   0.9949  0.74766
# Neg Pred Value         0.9977   0.9978  0.99877   0.9770  0.99934
# Prevalence             0.2853   0.1908  0.06690   0.4360  0.02103
# Detection Rate         0.2837   0.1890  0.06576   0.4227  0.02039
# Detection Prevalence   0.2844   0.1934  0.06996   0.4249  0.02727
# Balanced Accuracy      0.9966   0.9926  0.98918   0.9829  0.98133

t(round(prop.table(confmat15$table, 2),2))

quizztest <- predict(modelFit15, predtest)
quizztest
#
# [1] B A B A A D D B A A B D B A D D A B B B
# Levels: A B C D E
# 16/20

###########################
# Some playing
# - rf 90/10
#   10-fold CV
#   defined mtry
#   no pre-processing

inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(method = "cv", allowParallel = T)
set.seed(728665723)
mtry = c(2, floor(sqrt(ncol(ptrTraining) - 1)), floor(ncol(ptrTraining)/2), ncol(ptrTraining) - 1)
system.time(modelFit16 <- train(ptrTraining$classe ~ ., method="rf",
                                trControl = ctrl,
                                data=ptrTraining,
                                tuneGrid = data.frame(mtry = mtry)))
# Random Forest 
# 
# 17662 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 15896, 15895, 15896, 15894, 15896, 15896, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9866387  0.9806960  0.002946935  0.004277999
#      7    0.9891862  0.9843924  0.002866757  0.004158509
#     26    0.9891861  0.9843925  0.003105389  0.004505149
#     52    0.9823358  0.9745065  0.003935003  0.005684656
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 7. 

confmat16 <- confusionMatrix(ptrTesting$classe,predict(modelFit16,ptrTesting))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   A   B   C   D   E
# A 558   0   0   0   0
# B   2 377   0   0   0
# C   0   1 129   7   0
# D   0   1   2 830   0
# E   0   0   0   6  47
# 
# Overall Statistics
# 
#            Accuracy : 0.9903          
#              95% CI : (0.9849, 0.9942)
# No Information Rate : 0.4301          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.986           
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9964   0.9947  0.98473   0.9846  1.00000
# Specificity            1.0000   0.9987  0.99563   0.9973  0.99686
# Pos Pred Value         1.0000   0.9947  0.94161   0.9964  0.88679
# Neg Pred Value         0.9986   0.9987  0.99890   0.9885  1.00000
# Prevalence             0.2857   0.1934  0.06684   0.4301  0.02398
# Detection Rate         0.2847   0.1923  0.06582   0.4235  0.02398
# Detection Prevalence   0.2847   0.1934  0.06990   0.4250  0.02704
# Balanced Accuracy      0.9982   0.9967  0.99018   0.9909  0.99843

t(round(prop.table(confmat16$table, 2),2))

quizztest <- predict(modelFit16, predtest)
quizztest
#
# [1] B A B A A D D B A A B D B A D D A B B B
# Levels: A B C D E
# 16/20


###########################
# Some playing
# - rf 60/40
#   bootstrap x25
#   defined mtry
#   no pre-processing

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

ctrl <- trainControl(allowParallel = T)
set.seed(728665723)
mtry = c(2, floor(sqrt(ncol(ptrTraining) - 1)), floor(ncol(ptrTraining)/2), ncol(ptrTraining) - 1)
system.time(modelFit17 <- train(ptrTraining$classe ~ ., method="rf",
                                trControl = ctrl,
                                data=ptrTraining,
                                tuneGrid = data.frame(mtry = mtry)))
modelFit17

# Random Forest 
# 
# 11776 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 11776, 11776, 11776, 11776, 11776, 11776, ... 
# Resampling results across tuning parameters:
#     
#     mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
#      2    0.9857337  0.9819499  0.002251960  0.002851672
#      7    0.9886602  0.9856541  0.002132220  0.002699216
#     26    0.9868533  0.9833683  0.002153314  0.002723871
#     52    0.9748196  0.9681455  0.003321882  0.004188045
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 7. 

confmat17 <- confusionMatrix(ptrTesting$classe,predict(modelFit17,ptrTesting))
confmat17
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2229    3    0    0    0
#          B    5 1506    7    0    0
#          C    0   10 1356    2    0
#          D    0    0   20 1263    3
#          E    0    0    0    1 1441
# 
# Overall Statistics
# 
#            Accuracy : 0.9935          
#              95% CI : (0.9915, 0.9952)
# No Information Rate : 0.2847          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9918          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9978   0.9914   0.9805   0.9976   0.9979
# Specificity            0.9995   0.9981   0.9981   0.9965   0.9998
# Pos Pred Value         0.9987   0.9921   0.9912   0.9821   0.9993
# Neg Pred Value         0.9991   0.9979   0.9958   0.9995   0.9995
# Prevalence             0.2847   0.1936   0.1763   0.1614   0.1840
# Detection Rate         0.2841   0.1919   0.1728   0.1610   0.1837
# Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
# Balanced Accuracy      0.9986   0.9948   0.9893   0.9971   0.9989

t(round(prop.table(confmat17$table, 2),2))

score(predict(modelFit17, predtest))
# 20/20

#######################################################################################
# Class imbalance

prop.table(table(train$classe))
#         A         B         C         D         E 
# 0.2843747 0.1935073 0.1743961 0.1638977 0.1838243 
# 

library(randomForest)
#######################################################################################
# RandomForrest directly
# 60/40
# ntree - 1000
# mtry - default

#set.seed(728665723)
set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

set.seed(728665723)
modelFitRF <- randomForest(classe ~ ., data = ptrTraining, importance = T, ntree = 1000)
modelFitRF
100*(1-sum(diag(modelFitRF$confusion))/nrow(ptrTraining))

# Call:
#     randomForest(formula = classe ~ ., data = ptrTraining, importance = T,      ntree = 1000) 
# Type of random forest: classification
# Number of trees: 1000
# No. of variables tried at each split: 7
# 
#          OOB estimate of  error rate: 0.6%
# Confusion matrix:
#      A    B    C    D    E class.error
# A 3343    2    0    1    2 0.001493429
# B   13 2261    5    0    0 0.007898201
# C    0   11 2042    1    0 0.005842259
# D    0    0   26 1903    1 0.013989637
# E    0    0    3    6 2156 0.004157044

confmatRF <- confusionMatrix(ptrTesting$classe,predict(modelFitRF,ptrTesting))
confmatRF
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 2230    2    0    0    0
# B    6 1504    8    0    0
# C    0    8 1358    2    0
# D    0    0   19 1264    3
# E    0    0    0    1 1441
# 
# Overall Statistics
# 
#            Accuracy : 0.9938          
#              95% CI : (0.9918, 0.9954)
# No Information Rate : 0.285           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9921          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9973   0.9934   0.9805   0.9976   0.9979
# Specificity            0.9996   0.9978   0.9985   0.9967   0.9998
# Pos Pred Value         0.9991   0.9908   0.9927   0.9829   0.9993
# Neg Pred Value         0.9989   0.9984   0.9958   0.9995   0.9995
# Prevalence             0.2850   0.1930   0.1765   0.1615   0.1840
# Detection Rate         0.2842   0.1917   0.1731   0.1611   0.1837
# Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
# Balanced Accuracy      0.9985   0.9956   0.9895   0.9971   0.9989

score(predict(modelFitRF, predtest))
# 20/20

#######################################################################################
# RandomForrest directly
# 60/40
# ntree - 500
# mtry - default

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

set.seed(728665723)
modelFitRF2 <- randomForest(classe ~ ., data = ptrTraining, importance = T, ntree = 500)
modelFitRF2
100*(1-sum(diag(modelFitRF2$confusion))/nrow(ptrTraining))

# Call:
#     randomForest(formula = classe ~ ., data = ptrTraining, importance = T,      ntree = 500) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 7
# 
#          OOB estimate of  error rate: 0.69%
# Confusion matrix:
#      A    B    C    D    E class.error
# A 3343    2    0    0    3 0.001493429
# B   17 2257    5    0    0 0.009653357
# C    0   14 2038    2    0 0.007789679
# D    0    0   27 1902    1 0.014507772
# E    0    0    3    7 2155 0.004618938

confmatRF2 <- confusionMatrix(ptrTesting$classe,predict(modelFitRF2,ptrTesting))
confmatRF2
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2230    2    0    0    0
#          B    7 1505    6    0    0
#          C    0    9 1357    2    0
#          D    0    0   20 1263    3
#          E    0    0    0    2 1440
# 
# Overall Statistics
# 
#            Accuracy : 0.9935          
#              95% CI : (0.9915, 0.9952)
# No Information Rate : 0.2851          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9918          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9969   0.9927   0.9812   0.9968   0.9979
# Specificity            0.9996   0.9979   0.9983   0.9965   0.9997
# Pos Pred Value         0.9991   0.9914   0.9920   0.9821   0.9986
# Neg Pred Value         0.9988   0.9983   0.9960   0.9994   0.9995
# Prevalence             0.2851   0.1932   0.1763   0.1615   0.1839
# Detection Rate         0.2842   0.1918   0.1730   0.1610   0.1835
# Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
# Balanced Accuracy      0.9983   0.9953   0.9897   0.9967   0.9988

score(predict(modelFitRF2, predtest))
# 20/20


#######################################################################################
# RandomForrest directly
# 60/40
# ntree - 500
# mtry - 2

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

set.seed(728665723)
modelFitRF3 <- randomForest(classe ~ ., data = ptrTraining, importance = T, ntree = 500, mtry = 2)
modelFitRF3
100*(1-sum(diag(modelFitRF3$confusion))/nrow(ptrTraining))

# Call:
#     randomForest(formula = classe ~ ., data = ptrTraining, importance = T,      ntree = 500, mtry = 2) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
#          OOB estimate of  error rate: 0.82%
# Confusion matrix:
#      A    B    C    D    E  class.error
# A 3346    1    0    0    1 0.0005973716
# B   22 2251    6    0    0 0.0122860904
# C    0   16 2035    3    0 0.0092502434
# D    0    0   38 1891    1 0.0202072539
# E    0    0    2    7 2156 0.0041570439

confmatRF3 <- confusionMatrix(ptrTesting$classe,predict(modelFitRF3,ptrTesting))
confmatRF3
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2232    0    0    0    0
#          B    7 1505    6    0    0
#          C    0   13 1354    1    0
#          D    0    0   28 1254    4
#          E    0    0    1    2 1439
# 
# Overall Statistics
# 
#            Accuracy : 0.9921          
#              95% CI : (0.9899, 0.9939)
# No Information Rate : 0.2854          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.99            
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9969   0.9914   0.9748   0.9976   0.9972
# Specificity            1.0000   0.9979   0.9978   0.9951   0.9995
# Pos Pred Value         1.0000   0.9914   0.9898   0.9751   0.9979
# Neg Pred Value         0.9988   0.9979   0.9946   0.9995   0.9994
# Prevalence             0.2854   0.1935   0.1770   0.1602   0.1839
# Detection Rate         0.2845   0.1918   0.1726   0.1598   0.1834
# Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
# Balanced Accuracy      0.9984   0.9947   0.9863   0.9964   0.9984

score(predict(modelFitRF3, predtest))
# 20/20


#######################################################################################
# RandomForrest directly
# 100/0
# ntree - 500
# mtry - defualt

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.6, list=FALSE)
ptrTraining <- predtrain
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

set.seed(728665723)
modelFitRF4 <- randomForest(classe ~ ., data = ptrTraining, importance = T, ntree = 500)
modelFitRF4
100*(1-sum(diag(modelFitRF4$confusion))/nrow(ptrTraining))

# Call:
#     randomForest(formula = classe ~ ., data = ptrTraining, importance = T,      ntree = 500) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 7
# 
#     OOB estimate of  error rate: 0.29%
# Confusion matrix:
#      A    B    C    D    E  class.error
# A 5578    1    0    0    1 0.0003584229
# B   11 3783    3    0    0 0.0036871214
# C    0   12 3408    2    0 0.0040911748
# D    0    0   20 3194    2 0.0068407960
# E    0    0    0    4 3603 0.0011089548

# This doesn't have muuch sense, but some people reported these results, probably doing this incorrect approach

confmatRF4 <- confusionMatrix(ptrTesting$classe,predict(modelFitRF4,ptrTesting))
confmatRF4
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    A    B    C    D    E
#          A 2232    0    0    0    0
#          B    0 1518    0    0    0
#          C    0    0 1368    0    0
#          D    0    0    0 1286    0
#          E    0    0    0    0 1442
# 
# Overall Statistics
# 
#            Accuracy : 1          
#              95% CI : (0.9995, 1)
# No Information Rate : 0.2845     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
#                  Kappa : 1          
# Mcnemar's Test P-Value : NA         
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
# Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
# Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
# Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
# Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
# Detection Rate         0.2845   0.1935   0.1744   0.1639   0.1838
# Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
# Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

score(predict(modelFitRF4, predtest))
# 20/20

#######################################################################################
# RandomForrest directly
# 90/10
# ntree - 500
# mtry - defualt

set.seed(987687674)
inTrain <- createDataPartition(y=predtrain$classe, p=0.9, list=FALSE)
ptrTraining <- predtrain[inTrain,]
ptrTesting <- predtrain[-inTrain,]
dim(ptrTraining)
dim(ptrTesting)

set.seed(728665723)
modelFitRF5 <- randomForest(classe ~ ., data = ptrTraining, importance = T, ntree = 500)
modelFitRF5
100*(1-sum(diag(modelFitRF5$confusion))/nrow(ptrTraining))

# Call:
#     randomForest(formula = classe ~ ., data = ptrTraining, importance = T,      ntree = 500) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 7
# 
#     OOB estimate of  error rate: 0.36%
# Confusion matrix:
#      A    B    C    D    E  class.error
# A 5020    1    0    0    1 0.0003982477
# B   10 3406    2    0    0 0.0035108250
# C    0   13 3065    2    0 0.0048701299
# D    0    0   25 2868    2 0.0093264249
# E    0    0    1    6 3240 0.0021558362

confmatRF5 <- confusionMatrix(ptrTesting$classe,predict(modelFitRF5,ptrTesting))
confmatRF5
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   A   B   C   D   E
#          A 558   0   0   0   0
#          B   2 376   1   0   0
#          C   0   1 340   1   0
#          D   0   0   0 321   0
#          E   0   0   0   0 360
# 
# Overall Statistics
# 
#            Accuracy : 0.9974          
#              95% CI : (0.9941, 0.9992)
# No Information Rate : 0.2857          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
#                  Kappa : 0.9968          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9964   0.9973   0.9971   0.9969   1.0000
# Specificity            1.0000   0.9981   0.9988   1.0000   1.0000
# Pos Pred Value         1.0000   0.9921   0.9942   1.0000   1.0000
# Neg Pred Value         0.9986   0.9994   0.9994   0.9994   1.0000
# Prevalence             0.2857   0.1923   0.1740   0.1643   0.1837
# Detection Rate         0.2847   0.1918   0.1735   0.1638   0.1837
# Detection Prevalence   0.2847   0.1934   0.1745   0.1638   0.1837
# Balanced Accuracy      0.9982   0.9977   0.9979   0.9984   1.0000

score(predict(modelFitRF5, predtest))
# 20/20
