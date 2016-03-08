setwd("D:\\Users\\220222\\SandBox\\DataScience\\BNPParivasCardiff_Claims")
require(xgboost)

train <- read.csv('train.csv')
train$ID <- NULL
train$target <- as.factor(train$target)

test <- read.csv('test.csv')

xgboost_params <- list (
  objective <- 'binary:logistic',
  booster <- 'gbtree',
  eval_metric <- 'auc',
  eta <- 0.01,
  subsample <- 0.75,
  colsample_bytree <- 0.68,
  max.depth <- 7)
  

for(i in 1:ncol(train)){
  if ( class(train[,i]) == "numeric" ){
    train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
  }
}

for(i in 1:ncol(test)){
  if ( class(test[,i]) == "numeric" ){
    test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
  }
}

#train <- train[1:10000,]

xgtrain <- xgb.DMatrix(data.matrix(train[,2:132]), label=train[,1])
xgtest <- xgb.DMatrix(data.matrix(test[,2:132]))

boost_round <- 1800
clf <- xgb.train(xgboost_params,xgtrain,num_boost_round=boost_round, verbose_eval<-TRUE, maximize = FALSE)

test_preds = predict(clf,xgtest)
