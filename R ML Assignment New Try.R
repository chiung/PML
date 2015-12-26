#load library
library('caret')
library('randomForest')
library('e1071')
library('RANN')
#read in csv
training_org<-read.csv(file="C:\\Users\\peyter\\Documents\\R ML Assignment\\pml-training.csv",na.strings=c("", "NA", "NULL"))
testing_org<-read.csv(file="C:\\Users\\peyter\\Documents\\R ML Assignment\\pml-testing.csv",na.strings=c("", "NA", "NULL"))
#preprocess training_org by removing NA values
trainingNApp<- training_org[ , colSums(is.na(training_org)) == 0]
filter1 = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
training_filter <- trainingNApp[, -which(names(trainingNApp) %in% filter1)]
#preprocess testing_org by removing NA values
testingNApp<- testing_org[ , colSums(is.na(testing_org)) == 0]
filter1 = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
testing_filter <- testingNApp[, -which(names(testingNApp) %in% filter1)]
#continue preprocess by removing near zero variance for training
zeroVar= nearZeroVar(training_filter[sapply(training_filter, is.numeric)], saveMetrics = TRUE)
training.nonzerovar = training_filter[,zeroVar[, 'nzv']==0]
#continue preprocess by removing near zero variance for testing
zeroVartest= nearZeroVar(testing_filter[sapply(testing_filter, is.numeric)], saveMetrics = TRUE)
testing.nonzerovar = testing_filter[,zeroVar[, 'nzv']==0]
#continue preprocess by removing highly correlated predictor variable for training
corrMatrix <- cor(na.omit(training.nonzerovar[sapply(training.nonzerovar, is.numeric)]))
dim(corrMatrix)
removecor = findCorrelation(corrMatrix, cutoff = .90, verbose = TRUE)
training.decor = training.nonzerovar[,-removecor]
#continue preprocess by removing highly correlated predictor variable for testing
corrMatrixtest <- cor(na.omit(testing.nonzerovar[sapply(testing.nonzerovar, is.numeric)]))
dim(corrMatrixtest)
removecortest = findCorrelation(corrMatrixtest, cutoff = .90, verbose = TRUE)
testing.decor = testing.nonzerovar[,-removecor]
#in sample error checking
inTrain <- createDataPartition(y=training.decor$classe, p=0.7, list=FALSE)
training <- training.decor[inTrain,]
testing <- training.decor[-inTrain,]
#dim(training);dim(testing)
gbm_model1<-train(classe ~ ., method="gbm", data=training)
gbm_predict1 <- predict(gbm_model1,newdata=testing)
confusionMatrix(gbm_predict1, testing$classe)
#rf.training<-randomForest(classe~.,data=training,ntree=100, importance=TRUE)
#tree.pred=predict(rf.training,testing,type="class")
#test on actual data
gbm_predict2<-predict(gbm_model1, newdata=testing.decor)
#function to write labels
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(gbm_predict2)
