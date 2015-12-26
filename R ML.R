#load library
library('caret')
library('randomForest')
library('e1071')
library('RANN')
#read in csv
training<-read.csv(file="C:\\Users\\peyter\\Documents\\R ML Assignment\\pml-training.csv",head=TRUE,sep=",")
testing<-read.csv(file="C:\\Users\\peyter\\Documents\\R ML Assignment\\pml-testing.csv",head=TRUE,sep=",")
#preprocess training and testing set
training <- training[,-c(1:7)]#remove non essential columns
testing <- testing[,-c(1:7)]#remove non essential columns
NZV_columns_train <- nearZeroVar(training, saveMetrics = TRUE)#check for near zero variance(nzv) columns
#NZV_columns_test <- nearZeroVar(testing, saveMetrics = TRUE)#check for near zero variance(nzv) columns
training <- training[, NZV_columns_train$nzv == FALSE]#remove nzv columns from training set
testing <- testing[, NZV_columns_train$nzv == FALSE]#remove nzv columns from testing set
#preprocess training set, scale and replace NA 
pp_training <- preProcess(training[, -94], method = c("center", "scale", "knnImpute"))
clean_training<-predict(pp_training, newdata = training[1:19622, -94])
clean_train_combined<-cbind(clean_training,training$classe)
names(clean_train_combined)[names(clean_train_combined)=="training$classe"] <- "classe"
#preprocess testing set,  scale and replace NA 
testing[is.na(testing)] <- 0
pp_testing <- preProcess(testing[, -94], method = c("center", "scale", "knnImpute"))
clean_testing<-predict(pp_testing, newdata = testing[1:20,-94])
clean_test_combined<-cbind(clean_testing,testing$problem_id)
names(clean_train_combined)[names(clean_train_combined)=="testing$problem_id"] <- "problem_id"
clean_test_combined$problem_id<-0
clean_test_combined<-clean_test_combined[,-c(94)]
#in sample error estimation
inTrain <- createDataPartition(clean_train_combined$classe, p=0.75, list=FALSE)
train_learn <- clean_train_combined[inTrain, ]
train_valid <- clean_train_combined[-inTrain, ]
gbm_model1<-train(classe ~ ., method="gbm", data=train_learn)
gbm_valid <- predict(gbm_model1,newdata=train_valid)
confusionMatrix(gbm_valid, train_valid$classe)
#out of sample error estimatio
gbm_model_oos<-train(classe ~ ., method="gbm", data=clean_train_combined)
gbm_valid_oss<- predict(gbm_model_oos,newdata=clean_test_combined)
confusionMatrix(gbm_valid_oss, clean_test_combined)
#funtion to write output files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(gbm_valid_oss)
