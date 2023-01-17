# Model- No resampling
naive <- naiveBayes(train[-12],train$quality, laplace = 1,trControl = control)

#Predicting train set
pred_naive_train <- predict(naive, train)
cm_naive_train <- confusionMatrix(pred_naive_train, train$quality, positive = "1")
cm_naive_train

#Predicting test set
pred_naive_test <- predict(naive, test)
cm_naive_test <- confusionMatrix(pred_naive_test, test$quality, positive = "1")
cm_naive_test

#ROC
library(ROCR)
pred_naive <- predict(naive,test, type = 'raw')[,2]
ROC <- prediction(pred_naive, labels = test$quality)
plot(performance(ROC,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#model building - Down-sampling
naive_downsmpled <- naiveBayes(train_downsample[-12],train_downsample$quality, laplace = 1, trControl = control)

#Predicting train set
pred_naive_ds_train <- predict(naive_downsmpled, train_downsample)
cm_naive_ds_train <- confusionMatrix(pred_naive_ds_train, train_downsample$quality, positive = "1")
cm_naive_ds_train

#Predicting test set
pred_naive_ds_test <- predict(naive_downsmpled, test_downsample)
cm_naive_ds_test <- confusionMatrix(pred_naive_ds_test, test_downsample$quality, positive = "1")
cm_naive_ds_test

#ROC
pred_naive_ds <- predict(naive_downsmpled,test_downsample, type = 'raw')[,2]
ROC_ds <- prediction(pred_naive_ds, labels = test_downsample$quality)
plot(performance(ROC_ds,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_ds, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n


#tuning grid set up
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

#Model tuned - downsample
library(caret)
x = train_downsample[,-12]
y = train_downsample$quality

naive_tuned_ds <- train(x = x,y = y,method = "nb",trControl = control,tuneGrid = search_grid)
naive_tuned_ds

#Predicting train set tuned
pred_naive_train_tuned_ds <- predict(naive_tuned_ds , train_downsample)
cm_naive_train_tuned_ds <- confusionMatrix(pred_naive_train_tuned_ds, train_downsample$quality, positive = "1")
cm_naive_train_tuned_ds


#Predicting test set tuned
pred_naive_test_tuned_ds <- predict(naive_tuned_ds, test_downsample)
cm_naive_test_tuned_ds <- confusionMatrix(pred_naive_test_tuned_ds, test_downsample$quality, positive = "1")
cm_naive_test_tuned_ds

#ROC tuned
pred_naive_tuned_ds <- predict(naive_tuned_ds,test_downsample, type = 'prob')[,2]
ROC_naive_tuned_ds <- prediction(pred_naive_tuned_ds, labels = test_downsample$quality)
plot(performance(ROC_naive_tuned_ds,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC tuned
auc_ROCR_n <- performance(ROC_naive_tuned_ds, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#model building- Up-sampling
naive_upsampled <- naiveBayes(train_upsample[-12],train_upsample$quality, laplace = 1, trControl = control)

#Predicting train set
pred_naive_us_train <- predict(naive_upsampled, train_upsample)
cm_naive_us_train <- confusionMatrix(pred_naive_us_train, train_upsample$quality, positive = "1")
cm_naive_us_train

#Predicting test set
pred_naive_us_test <- predict(naive_upsampled, test_upsample)
cm_naive_us_test <- confusionMatrix(pred_naive_us_test, test_upsample$quality, positive = "1")
cm_naive_us_test

#ROC
pred_naive_us <- predict(naive_upsampled,test_upsample, type = 'raw')[,2]
ROC_us <- prediction(pred_naive_us, labels = test_upsample$quality)
plot(performance(ROC_us,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC
auc_ROCR_n <- performance(ROC_us, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#tuning grid set up
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

#Model tuned
x = train_upsample[,-12]
y = train_upsample$quality

naive_tuned_us <- train(x = x,y = y,method = "nb",trControl = control,tuneGrid = search_grid)
naive_tuned_us

#Predicting train set tuned
pred_naive_train_tuned_us <- predict(naive_tuned_us , train_upsample)
cm_naive_train_tuned_us <- confusionMatrix(pred_naive_train_tuned_us, train_upsample$quality, positive = "1")
cm_naive_train_tuned_us

#Predicting test set tuned
pred_naive_test_tuned_us <- predict(naive_tuned_us, test_upsample)
cm_naive_test_tuned_us <- confusionMatrix(pred_naive_test_tuned_ds, test_upsample$quality, positive = "1")
cm_naive_test_tuned_us

#ROC tuned
pred_naive_tuned_us <- predict(naive_tuned_us,test_upsample, type = 'prob')[,2]
ROC_naive_tuned_us <- prediction(pred_naive_tuned_us, labels = test_upsample$quality)
plot(performance(ROC_naive_tuned_us,"tpr", "fpr"),
     main = "ROC")
abline(a=0,b=1)

# AUC tuned
auc_ROCR_n <- performance(ROC_naive_tuned_us, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

