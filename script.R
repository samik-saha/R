setwd("D:\\Users\\220222\\SandBox\\DataScience\\BNPParivasCardiff_Claims")
library('caret')
set.seed(12345)
origData<-read.csv('train.csv',header=TRUE)
origData$ID=NULL
origData$v22=NULL
origData=origData[complete.cases(origData),]
origData$target=as.factor(origData$target)
origData$v38=as.factor(origData$v38)
origData$v62=as.factor(origData$v62)
origData$v72=as.factor(origData$v72)
featureCols <- c("target", paste(c("v"),as.character(c(30:40)), sep =""))
dataFiltered<- origData[,featureCols]
inTrainRows<-createDataPartition(dataFiltered$target, p=0.70,list=FALSE)
trainDataFiltered<-dataFiltered[inTrainRows,]
testDataFiltered <- dataFiltered[-inTrainRows,]
logisticRegModel <- train(target ~ ., data=trainDataFiltered, method="glm",family="binomial")
logRegPrediction <- predict(logisticRegModel, testDataFiltered)
logRegConfMat <- confusionMatrix(logRegPrediction, testDataFiltered[,"target"])
print(logRegConfMat)

origData <- origData[c(1:20000),]
inTrainRows<-createDataPartition(origData$target, p=0.70,list=FALSE)
trainData <- origData[inTrainRows,]
testData <- origData[-inTrainRows,]
logisticRegModel <- train(target ~ ., data=trainData, method="glm",family="binomial")
logRegPrediction <- predict(logisticRegModel, testData)
logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"target"])
print(logRegConfMat)
