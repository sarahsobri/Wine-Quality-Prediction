#Model - Without resampling
LR <- glm(formula = quality ~ ., data = train, family = binomial())
summary(LR)

LR2 <- glm(formula = quality ~ volatileacidity + residualsugar + chlorides +
             totalsulfurdioxide + density + pH + sulphates + alcohol, 
           data = train, family = binomial())
summary(LR2)

LR3 <- glm(formula = quality ~  volatileacidity + residualsugar +
             totalsulfurdioxide + pH + sulphates + alcohol, 
           data = train, family = binomial())
summary(LR3)

LR4 <- glm(formula = quality ~  volatileacidity +
             totalsulfurdioxide + sulphates + alcohol, 
           data = train, family = binomial())
summary(LR4)

#Prediction on Train Set
pred_lr_train <- predict(LR4, train, type = 'response')
pred_lr_train1 <- ifelse(pred_lr_train>0.5,1,0)
tab_train <- table(Predicted = pred_lr_train1, Actual =  as.factor(train$quality))
confusionMatrix(tab_train, positive = '1')

#Prediction on Test Set
pred_lr_test <- predict(LR4, test, type = 'response')
pred_lr_test1 <- ifelse(pred_lr_test>0.5,1,0)
tab_test <- table(Predicted = pred_lr_test1, Actual =  as.factor(test$quality))
confusionMatrix(tab_test, positive = '1')

#ROC
library(ROCR)
pred_LR <- predict(LR4,test, type = 'response')
ROC <- prediction(pred_LR, labels = test$quality )
plot(performance(ROC,"tpr", "fpr"),
     main = "ROC")

# AUC
auc_ROCR_n <- performance(ROC, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#Model- Down-sampling
LR_ds <- glm(formula = quality ~ ., data = train_downsample, family = binomial())
summary(LR_ds)

LR2_ds <- glm(formula = quality ~ fixedacidity + volatileacidity + residualsugar + chlorides +
                totalsulfurdioxide + density + pH + sulphates + alcohol, 
              data = train_downsample, family = binomial())
summary(LR2_ds)

LR3_ds <- glm(formula = quality ~  volatileacidity + residualsugar + chlorides +
                totalsulfurdioxide + density + sulphates + alcohol, 
              data = train_downsample, family = binomial())
summary(LR3_ds)

LR4_ds <- glm(formula = quality ~  volatileacidity + residualsugar +
                totalsulfurdioxide + sulphates + alcohol, 
              data = train_downsample, family = binomial())
summary(LR4_ds)

#Prediction on Train Set
pred_lr_train_ds <- predict(LR4_ds, train_downsample, type = 'response')
pred_lr_train_ds1 <- ifelse(pred_lr_train_ds>0.5,1,0)
tab_train_ds <- table(Predicted = pred_lr_train_ds1, Actual =  as.factor(train_downsample$quality))
confusionMatrix(tab_train_ds, positive = '1')

#Prediction on Test Set
pred_lr_test_ds <- predict(LR4_ds, test_downsample, type = 'response')
pred_lr_test1_ds <- ifelse(pred_lr_test_ds>0.5,1,0)
tab_test_ds <- table(Predicted = pred_lr_test1_ds, Actual =  as.factor(test_downsample$quality))
confusionMatrix(tab_test_ds, positive = '1')

#ROC
pred_LR_ds <- predict(LR4_ds,test_downsample, type = 'response')
ROC_ds <- prediction(pred_LR_ds, labels = test_downsample$quality )
plot(performance(ROC_ds,"tpr", "fpr"),
     main = "ROC")

# AUC
auc_ROCR_n <- performance(ROC_ds, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n

#Model training- Up-sampling
LR_us <- glm(formula = quality ~ ., data = train_upsample, family = binomial())
summary(LR_us)

LR2_us <- glm(formula = quality ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides +
                totalsulfurdioxide + density + sulphates + alcohol, 
              data = train_upsample, family = binomial())
summary(LR2_us)

LR3_us <- glm(formula = quality ~  fixedacidity + volatileacidity + residualsugar + chlorides +
                totalsulfurdioxide + density + sulphates + alcohol, 
              data = train_upsample, family = binomial())
summary(LR3_us)

#Prediction on Train Set
pred_lr_train_us <- predict(LR3_us, train_upsample, type = 'response')
pred_lr_train_us1 <- ifelse(pred_lr_train_us>0.5,1,0)
tab_train_us <- table(Predicted = pred_lr_train_us1, Actual =  as.factor(train_upsample$quality))
confusionMatrix(tab_train_us, positive = '1')

#Prediction on Test Set
pred_lr_test_us <- predict(LR3_us, test_upsample, type = 'response')
pred_lr_test_us1 <- ifelse(pred_lr_test_us>0.5,1,0)
tab_test_us <- table(Predicted = pred_lr_test_us1, Actual =  as.factor(test_upsample$quality))
confusionMatrix(tab_test_us, positive = '1')


#ROC
pred_LR_us <- predict(LR3_us,test_upsample, type = 'response')
ROC_us <- prediction(pred_LR_us, labels = test_upsample$quality )
plot(performance(ROC_us,"tpr", "fpr"),
     main = "ROC")

# AUC
auc_ROCR_n <- performance(ROC_us, measure = "auc")
auc_ROCR_n <- auc_ROCR_n@y.values[[1]]
auc_ROCR_n



