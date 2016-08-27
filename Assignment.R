library(caret)
library(data.table)
library(curl)
set.seed(12345)
library(xgboost)
library(randomForest)
library(adabag)
setwd("C:/Work/Coursera/PracticalMachineLearning")

### Load data 
# load data
# training <- read.csv("pml-training.csv",hea=T,row.names=1)
# testing <- read.csv("pml-testing.csv",hea=T,row.names=1)
# save(training,file="training")
# save(testing,file="testing")
load("training")
load("testing")
#
### Preliminary analysis

# remove near zero features using the nearZeroVar function in the caret package
nzv <- nearZeroVar(training)
training <- training[, -c(nzv) ]
testing <- testing[, -c(nzv) ]


na.cut<-0.9
badcols <- colSums(is.na(training))/nrow(training) > na.cut

training<-training[,!badcols]
testing<-testing[,!badcols]
print(dim(training))
# inTrain  <- createDataPartition(training$classe, p=0.7, list=FALSE)
# trainData<-training[inTrain,]
# testData<-training[-inTrain,]

#training1<-head(training,1000)
# randomforest with 10 fold crossvalidation
randFFit <- train(classe ~ ., method = "rf", data = training, importance = T, 
         trControl = trainControl(method = "cv", number = 10))
#save(randFit,file="randFit")

# svm with 10 fold crossvalidation
svmFit <- train(classe ~ ., method = "svmRadial",
                  data = training, verbose = F,  importance = T,
                  trControl = trainControl(method = "cv", number = 10))
save(svmFit,file="svmFit")

# # prediction
 (predictionRF <- as.character(predict(randFFit, testing)))
 (predictionSVM <- as.character(predict(svmFit, testing)))

