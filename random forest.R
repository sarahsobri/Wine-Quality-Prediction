#Model- No resampling
library(randomForest)
set.seed(225)
rf <- randomForest(quality~., train, trControl = control)
rf

#Prediction- Train set
pred_rf_train <- predict(rf,train)
confusionMatrix(pred_rf_train,train$quality, positive ='1')

#Prediction- Test set
pred_rf_test <- predict(rf,test)
confusionMatrix(pred_rf_test,test$quality, positive ='1')

#ROC
pred_rf <- predict(rf, test, type = 'prob')
ROC <- prediction(pred_rf[,2], labels = test$quality )
plot(performance(ROC,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#Model- Down-sampling
set.seed(225)
rf_downsample <- randomForest(quality~., train_downsample, trControl = control)
rf_downsample

#Prediction- Train set
pred_rf_ds_train <- predict(rf_downsample,train_downsample)
confusionMatrix(pred_rf_ds_train,train_downsample$quality, positive ='1')

#Prediction- Test set
pred_rf_ds_test <- predict(rf_downsample,test_downsample)
confusionMatrix(pred_rf_ds_test,test_downsample$quality, positive ='1')

#ROC
pred_rf_ds <- predict(rf_downsample, test_downsample, type = 'prob')
ROC_ds <- prediction(pred_rf_ds[,2], labels = test_downsample$quality )
plot(performance(ROC_ds,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_ds, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

plot(rf_downsample)
legend("topright", colnames(rf_upsample$err.rate),col=1:6,cex=0.8,fill=1:6)

#Tune for mtry
train <- as.data.frame(train_downsample)
tuneRF(train_downsample[-12], train_downsample[,12],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 750,
       trace = TRUE,
       improve = 0.05)

#tuned model
rf_downsampled_tuned <- randomForest(quality~., train_upsample,
                                     ntree = 750,
                                     mtry = 3,
                                     importance = TRUE,
                                     promixity = TRUE)
rf_downsampled_tuned

#Prediction- Train set tuned
pred_rf_train_tuned_ds <- predict(rf_downsampled_tuned,train_downsample)
confusionMatrix(pred_rf_train_tuned_ds,train_downsample$quality, positive ='1')

#Prediction- Test set tuned
pred_rf_test_tuned_ds <- predict(rf_downsampled_tuned,test_downsample)
confusionMatrix(pred_rf_test_tuned_ds,test_downsample$quality, positive ='1')

#ROC- tuned
pred_rf_tuned_ds <- predict(rf_downsampled_tuned, test_downsample, type = 'prob')
ROC_tuned_ds <- prediction(pred_rf_tuned_ds[,2], labels = test_downsample$quality )
plot(performance(ROC_tuned_ds,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_tuned_ds, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#Important Variables
varImp(rf_downsampled_tuned)
varImpPlot(rf_downsampled_tuned,
           sort = T,
           n.var = 5,
           main = 'Top 5 Variable Importance')

#Model- Up-sampling
set.seed(223)
rf_upsample <- randomForest(quality~., train_upsample, trControl = control)
rf_upsample

#Prediction- Train set
pred_rf_us_train <- predict(rf_upsample,train_upsample)
confusionMatrix(pred_rf_us_train,train_upsample$quality, positive ='1')

#Prediction- Test set
pred_rf_us_test <- predict(rf_upsample,test_upsample)
confusionMatrix(pred_rf_us_test,test_upsample$quality, positive ='1')

#ROC
pred_rf_us <- predict(rf_upsample, test_upsample, type = 'prob')
ROC_us <- prediction(pred_rf_us[,2], labels = test_upsample$quality )
plot(performance(ROC_us,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_us, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

plot(rf_upsample)
legend("topright", colnames(rf_upsample$err.rate),col=1:6,cex=0.8,fill=1:6)

#Tune for mtry
train <- as.data.frame(train_upsample)
tuneRF(train_upsample[-12], train_upsample[,12],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 650,
       trace = TRUE,
       improve = 0.05)

rf_upsampled_tuned <- randomForest(quality~., train_upsample,
                                   ntree = 650,
                                   mtry = 1,
                                   importance = TRUE,
                                   promixity = TRUE)
rf_upsampled_tuned

#Prediction- Train set tunes
pred_rf_train_tuned_us <- predict(rf_upsampled_tuned,train_upsample)
confusionMatrix(pred_rf_train_tuned_us,train_upsample$quality, positive ='1')

#Prediction- Test set tunes
pred_rf_test_tuned_us <- predict(rf_upsampled_tuned,test_upsample)
confusionMatrix(pred_rf_test_tuned_us,test_upsample$quality, positive ='1')

#ROC- tuned
pred_rf_tuned_us <- predict(rf_upsampled_tuned, test_upsample, type = 'prob')
ROC_tuned <- prediction(pred_rf_tuned[,2], labels = test_upsample$quality )
plot(performance(ROC_tuned,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_tuned, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#Important Variables
varImp(rf_upsampled_tuned)
varImpPlot(rf_upsampled_tuned,
           sort = T,
           n.var = 5,
           main = 'Top 5 Variable Importance')

varUsed(rf_upsampled_tuned)
