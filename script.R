setwd("D:\\Users\\220222\\SandBox\\DataScience\\BNPParivasCardiff_Claims")
library('caret')
set.seed(12345)
origData<-read.csv('train.csv',header=TRUE,stringsAsFactors = FALSE)
origData$ID=NULL
origData=origData[complete.cases(origData),]
origData$target=as.factor(origData$target)
origData$v3=as.factor(origData$v3)
origData$v22=as.factor(origData$v22)
origData$v24=as.factor(origData$v24)
origData$v30=as.factor(origData$v30)
origData$v31=as.factor(origData$v31)
origData$v38=as.factor(origData$v38)
origData$v47=as.factor(origData$v47)
origData$v52=as.factor(origData$v52)
origData$v56=as.factor(origData$v56)
origData$v62=as.factor(origData$v62)
origData$v66=as.factor(origData$v66)
origData$v71=as.factor(origData$v71)
origData$v72=as.factor(origData$v72)
origData$v74=as.factor(origData$v74)
origData$v75=as.factor(origData$v75)
origData$v79=as.factor(origData$v79)
origData$v91=as.factor(origData$v91)
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
