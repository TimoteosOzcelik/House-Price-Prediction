# load libraries
library(mlbench)
library(caret)

path = "/Users/timoteosonurozcelik/Desktop/Final_Project/"
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))
  return (mape)
}

RMSE = function(m, o){
  rmse=sqrt(mean((m - o)^2))
  return(rmse)
}

rsq <- function (x, y){
  rsq=cor(x, y) ^ 2
  return(rsq)
}

MAE <- function(actual,pred){
  mae<- mean(abs((actual - pred)))
  return (mae)
}

dataset=read.csv(paste(path, "/kc_house_data.csv", sep=""), header=T)

# New columns
dataset$age <- as.numeric(substr(dataset$date, 0, 4)) - dataset$yr_built
dataset$is_renovated <- as.numeric(dataset$yr_renovated != 0)

# Drop some column
dataset$id <- NULL
dataset$date <- NULL
dataset$yr_renovated <- NULL
dataset$yr_built <- NULL
dataset$zipcode <- NULL

# PCA
dataset.pca <- prcomp(dataset, center = TRUE, scale. = TRUE)
summary(dataset.pca)

# ANOVA
fit = aov(df$price ~ df$bedrooms * df$bathrooms * df$sqft_living * df$waterfront * df$grade * df$sqft_above * df$sqft_basement * df$view * df$sqft_living15, data=df)
summary(fit)

ratio = 0.8

set.seed(36)
inTrain <- sample(1:dim(dataset)[1], dim(dataset)[1]*ratio)
trainData <- dataset[inTrain, ]
testData <- dataset[-inTrain, ]

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"

methods = c("lm", "glm","svmRadial","svmLinear","knn","bridge","bayesglm","rpart","treebag","rf","gbm","mlp","mlpML","nnet", "rqnc","ridge")

modeldf = data.frame()
traindf = data.frame()
fitdf = data.frame()
testdf = data.frame()

###1.kolon algoritma adı, 2.kolon RMSE, 3. kolon Rsquared,4.kolon MAE,  5. kolon MAPE, 6. kolon adı MSE ve 7. kolon adı execution time in seconds

k = 1
for (i in methods){
  print(paste("Algorithm:", i))
  set.seed(seed)
  
  ## Model Creation step
  start_time <- Sys.time()
  fit <- train(price~., data=trainData, method=i, metric=metric, preProc=c("center", "scale"), trControl=control)
  end_time <- Sys.time()
  exec_time_model_creation=as.numeric(end_time-start_time,units="secs")
  print(paste("Start time:", start_time, "\nEnd time:", end_time))
  
  ## Estimation from model
  start_time <- Sys.time()
  pred = predict(fit)
  end_time <- Sys.time()
  exec_time_model_perf=as.numeric(end_time-start_time,units="secs")
  
  ## Training Performance step
  start_time <- Sys.time()
  pred_train = predict(fit,trainData)
  end_time <- Sys.time()
  exec_time_prediction_train=as.numeric(end_time-start_time,units="secs")
  
  ## Test Performance
  start_time <- Sys.time()
  pred_test = predict(fit,testData)
  end_time <- Sys.time()
  exec_time_prediction_test=as.numeric(end_time-start_time,units="secs")
  
  modeldf[k,1] = i
  res = fit$results
  ind = which.min(res$RMSE)
  
  #Validation -- Model
  modeldf[k,2] = res[ind,]$RMSE
  modeldf[k,3] = res[ind,]$Rsquared
  modeldf[k,4] = res[ind,]$MAE
  modeldf[k,5] = res[ind,]$RMSESD
  modeldf[k,6] = res[ind,]$RsquaredSD
  modeldf[k,7] = res[ind,]$MAESD
  modeldf[k,8] = mape(trainData$price, pred)
  modeldf[k,9] = mean((trainData$price - pred)^2)
  modeldf[k,10] = exec_time_model_creation
  
  # Fit
  fitdf[k,1] = i
  fitdf[k,2] = RMSE(pred, trainData$price)
  fitdf[k,3] = rsq(pred, trainData$price)
  fitdf[k,4] = MAE(pred, trainData$price)
  fitdf[k,5] = mape(pred, trainData$price)
  fitdf[k,6] = mean((trainData$price - pred)^2)
  fitdf[k,7] = exec_time_model_perf
  
  # Train
  traindf[k,1] = i
  traindf[k,2] = RMSE(pred_train, trainData$price)
  traindf[k,3] = rsq(pred_train, trainData$price)
  traindf[k,4] = MAE(pred_train, trainData$price)
  traindf[k,5] = mape(pred_train, trainData$price)
  traindf[k,6] = mean((trainData$price - pred_train)^2)
  traindf[k,7] = exec_time_prediction_train
  
  # Test
  testdf[k,1] = i
  testdf[k,2] = RMSE(pred_test, testData$price)
  testdf[k,3] = rsq(pred_test, testData$price)
  testdf[k,4] = MAE(pred_test, testData$price)
  testdf[k,5] = mape(pred_test, testData$price)
  testdf[k,6] = mean((testData$price - pred_test)^2)
  testdf[k,7] = exec_time_prediction_test
  
  k = k + 1
}

x <-  c("algorithm","RMSE","RSquared","MAE","RMSEsd","Rsquaredsd","MAEsd","MAPE", "MSE","execTime")
colnames(modeldf) <- x
write.csv(file=paste(path,"modeldf.csv",sep=""),modeldf)

x <-  c("algorithm","RMSE","RSquared","MAE","MAPE", "MSE","execTime")
colnames(fitdf) <- x
write.csv(file=paste(path,"fitdf.csv",sep=""),fitdf)

x <-  c("algorithm","RMSE","RSquared","MAE","MAPE", "MSE","execTime")
colnames(traindf) <- x
write.csv(file=paste(path,"traindf.csv",sep=""),traindf)

x <-  c("algorithm","RMSE","RSquared","MAE","MAPE", "MSE","execTime")
colnames(testdf) <- x
write.csv(file=paste(path,"testdf.csv",sep=""),testdf)
