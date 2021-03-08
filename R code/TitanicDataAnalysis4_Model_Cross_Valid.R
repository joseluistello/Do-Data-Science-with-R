############################################################################
# Copyright 2021 Gangmin Li
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# The base of this code was from Dave Langer "Intoduction to Data Science."
# Credit to him. You can find him on Github: https://github.com/EasyD
# I have also integrated other sources like Titanic Forum, Python code
# and some Chinese R code.
#
# Notably,
# https://www.kaggle.com/startupsci/titanic-data-science-solutions
#
# The whole purpose of this is to teach My students on Data Science.
#
# This R source code file corresponds to video 1 of the YouTube series
# "Introduction to Data Science with R" located at the following URL:
#     http://www.youtube.com/watch?v=32o0DnuRjfg
#
# The task is to build a model based on the train data
# then to predict test data who can survive in Kaggle Titanic competition
##########################################################################

####################################################
# Model Cross Validation and Fine Tune
####################################################
# This file contains Cross Validation
#  1. CV-tree models
#  2. CV-random forest models
#
##### Predictor selection

library(caret)
library(rpart)
library(rpart.plot)

#read Re-engineered dataset
RE_data <- read.csv("RE_data.csv", header = TRUE)

#Factorize response variable
RE_data$Survived <- factor(RE_data$Survived)

#Separate Train and test data.
train <- RE_data[1:891, ]
test <- RE_data[892:1309, ]

# Setup model's train and valid dataset
set.seed(1000)
samp <- sample(nrow(train), 0.8 * nrow(train))
trainData <- train[samp, ]
validData <- train[-samp, ]

# set random for reproduction
set.seed(3214)
# specify parameters for cross validation
control <- trainControl(method = "repeatedcv",
                        number = 10, # number of folds
                        repeats = 5, # repeat times
                        search = "grid")

###############################################
# CV on Tree models
###############################################

### CV on tree model2
set.seed(1010)
# Create model from cross validation train data
# Tree_model2_cv <- train(Survived ~ Sex + Pclass + HasCabinNum + Deck + Fare_pp,
#                         data = trainData,
#                         method = "rpart",
#                         trControl = control)

# Due to the computation cost once a model is trained.
# it is better to save it and load later rather than compute a gain
#save(Tree_model2_cv, file = "Tree_model2_cv.rda")
load("Tree_model2_cv.rda")

# Show CV model's details
print.train(Tree_model2_cv)
# CV model's estimated accuracy
model_accuracy <- Tree_model2_cv$results$Accuracy[1]
paste("Estimated accuracy:", format(model_accuracy, digits = 4))

# Visualize cross validation tree
rpart.plot(Tree_model2_cv$finalModel, extra=4)
plot.train(Tree_model2_cv)

# Record the model's accuracy on *trainData*, *validData*,
# and *test* dataset. Remember *trainData* and *validData*
# are randomly partitioned from the train dataset.
### Access accuracy on different datasets

# prediction's Confusion Matrix on the trainData
predict_train <-predict(Tree_model2_cv, trainData)
conMat <- confusionMatrix(predict_train, trainData$Survived)
conMat$table
# prediction's Accuracy on the trainData
predict_train_accuracy <- conMat$overall["Accuracy"]
predict_train_accuracy

# prediction's Confusion Matrix on the validData
predict_valid <-predict(Tree_model2_cv, validData)
conMat <- confusionMatrix(predict_valid, validData$Survived)
conMat$table
# prediction's Accuracy on the validData
predict_valid_accuracy <- conMat$overall["Accuracy"]
predict_valid_accuracy

# predict on test
predict_test <-predict(Tree_model2_cv, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = as.factor(predict_test))
write.csv(submit, file = "Tree_model2_CV.CSV", row.names = FALSE)
# test accuracy 0.75837
paste("Test Accuracy:", 0.7584)

# accumulate model's accuracy
name <- c("Esti Accu", "Train Accu", "Valid Accu", "Test Accu")
Tree_model2_CV_accuracy <- c(model_accuracy, predict_train_accuracy, predict_valid_accuracy, 0.7584)
names(Tree_model2_CV_accuracy) <- name
Tree_model2_CV_accuracy

### CV on model3
#
set.seed(1234)
# Tree_model3_cv <- train(Survived ~ Sex + Fare_pp + Pclass + Title + Age_group + Group_size + Ticket_class  + Embarked,
#
#                        data = trainData,
#                        method = "rpart",
#                        trControl = control)
#
# save(Tree_model3_cv, file = "Tree_model3_cv.rda")
load("Tree_model3_cv.rda")
# show model details
print.train(Tree_model3_cv)
# CV model's estimated accuracy
model_accuracy <- Tree_model3_cv$results$Accuracy[1]
paste("Estimated accuracy:", format(model_accuracy, digits = 4))

#Visualize cross validation tree
rpart.plot(Tree_model3_cv$finalModel, extra=4)
plot.train(Tree_model3_cv)

### Access accuracy on different datasets
# prediction's Confusion Matrix on the trainData
predict_train <-predict(Tree_model3_cv, trainData)
conMat <- confusionMatrix(predict_train, trainData$Survived)
conMat$table
# prediction's Accuracy on the trainData
predict_train_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("trainData Accuracy:", predict_train_accuracy)

# prediction's Confusion Matrix on the validData
predict_valid <-predict(Tree_model3_cv, validData)
conMat <- confusionMatrix(predict_valid, validData$Survived)
conMat$table
# prediction's Accuracy on the validData
predict_valid_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("validData Accuracy:", predict_valid_accuracy)

#predict on test
predict_test <-predict(Tree_model3_cv, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = as.factor(predict_test))
write.csv(submit, file = "Tree_model3_CV.CSV", row.names = FALSE)

## test accuracy is 0.77751
paste("Test Accuracy:", 0.7775)

# accumulate model's accuracy
Tree_model3_CV_accuracy <- c(model_accuracy, predict_train_accuracy, predict_valid_accuracy, 0.7775)
names(Tree_model3_CV_accuracy) <- name
Tree_model3_CV_accuracy

##############################################
### Cross Validation on Random Forest Models
##############################################

# Random Forest model RF_model1_cv
# set seed for reproduction
set.seed(2307)
# RF_model1_cv <- train(Survived ~ Sex + Pclass + HasCabinNum +      Deck + Fare_pp,
#                        data = trainData,
#                        method = "rf",
#                        trControl = control)
# save(RF_model1_cv, file = "RF_model1_cv.rda")
load("RF_model1_cv.rda")

# Show CV mdoel's details
print(RF_model1_cv)
print(RF_model1_cv$results)

# Record model's accuracy
model_accuracy <- format(RF_model1_cv$results$Accuracy[2], digits = 4)
paste("Estimated accuracy:", model_accuracy)

### Access accuracy on different datasets
# prediction's Confusion Matrix on the trainData
predict_train <-predict(RF_model1_cv, trainData)
conMat <- confusionMatrix(predict_train, trainData$Survived)
conMat$table
# prediction's Accuracy on the trainData
predict_train_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("trainData Accuracy:", predict_train_accuracy)

# prediction's Confusion Matrix on the validData
predict_valid <-predict(RF_model1_cv, validData)
conMat <- confusionMatrix(predict_valid, validData$Survived)
conMat$table
# prediction's Accuracy on the validData
predict_valid_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("validData Accuracy:", predict_valid_accuracy)

# predict on test
predict_test <-predict(RF_model1_cv, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = as.factor(predict_test))
write.csv(submit, file = "RF_model1_CV.CSV", row.names = FALSE)

## test accuracy 0.74641
paste("Test Accuracy:", 0.7464)

# accumulate model's accuracy
RF_model1_cv_accuracy <- c(model_accuracy, predict_train_accuracy, predict_valid_accuracy, 0.7464)
names(RF_model1_cv_accuracy) <- name
RF_model1_cv_accuracy

###
# Random Forest model RF_model2_cv
##
# set seed for reproduction
set.seed(2300)

# RF_model2_cv <- train(Survived ~ Sex + Fare_pp + Pclass + Title + Age_group + Group_size + Ticket_class + Embarked,
#                        data = trainData,
#                        method = "rf",
#                        trControl = control)
# # This model will be used in chapter 12. so it is saved into a file for late to be loaded
# save(RF_model2_cv, file = "RF_model2_cv.rda")

load("RF_model2_cv.rda")

# Show CV mdoel's details
print(RF_model2_cv)
print(RF_model2_cv$results)

# Record model's accuracy
mode2_accuracy <- format(RF_model2_cv$results$Accuracy[2], digits = 4)
paste("Estimated accuracy:", mode2_accuracy)

### Access accuracy on different datasets
# prediction's Confusion Matrix on the trainData
predict_train <-predict(RF_model2_cv, trainData)
conMat <- confusionMatrix(predict_train, trainData$Survived)
conMat$table

# prediction's Accuracy on the trainData
predict_train_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("trainData Accuracy:", predict_train_accuracy)

# prediction's Confusion Matrix on the validData
predict_valid <-predict(RF_model2_cv, validData)
conMat <- confusionMatrix(predict_valid, validData$Survived)
conMat$table
# prediction's Accuracy on the validData
predict_valid_accuracy <- format(conMat$overall["Accuracy"], digits=4)
paste("validData Accuracy:", predict_valid_accuracy)

#predict on test
predict_test <-predict(RF_model2_cv, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = as.factor(predict_test))
write.csv(submit, file = "RF_model2_CV.CSV", row.names = FALSE)

## test accuracy 0.75119
paste("Test Accuracy:", 0.7512)

# accumulate model's accuracy
RF_model2_cv_accuracy <- c(mode2_accuracy, predict_train_accuracy, predict_valid_accuracy, 0.7512)
names(RF_model2_cv_accuracy) <- name
RF_model2_cv_accuracy

### make a comparison among tree and random forest models
library(tidyr)
Model <- c("Tree_M2","Tree_M3","RF_model1","RF_model2")

# Show individual models' accuracy
Tree_model2_CV_accuracy
Tree_model3_CV_accuracy
RF_model1_cv_accuracy
RF_model2_cv_accuracy

#preparee for table construction
Pre <- c("Sex, Pclass, HasCabinNum, Deck, Fare_pp", "Sex, Fare_pp, Pclass, Title, Age_group, Group_size, Ticket_class, Embarked", "Sex, Pclass, HasCabinNum, Deck, Fare_pp", "Sex, Fare_pp, Pclass, Title, Age_group, Group_size, Ticket_class, Embarked")
#
Learn <- c(as.numeric(Tree_model2_CV_accuracy[1])*100, as.numeric(Tree_model3_CV_accuracy[1])*100, as.numeric(RF_model1_cv_accuracy[1])*100, as.numeric(RF_model2_cv_accuracy[1])*100)
#
Train <- c(as.numeric(Tree_model2_CV_accuracy[2])*100, as.numeric(Tree_model3_CV_accuracy[2])*100, as.numeric(RF_model1_cv_accuracy[2])*100, as.numeric(RF_model2_cv_accuracy[2])*100)
#
Valid <- c(as.numeric(Tree_model2_CV_accuracy[3])*100, as.numeric(Tree_model3_CV_accuracy[3])*100, as.numeric(RF_model1_cv_accuracy[3])*100, as.numeric(RF_model2_cv_accuracy[3])*100)
#
Test <- c(as.numeric(Tree_model2_CV_accuracy[4])*100, as.numeric(Tree_model3_CV_accuracy[4])*100, as.numeric(RF_model1_cv_accuracy[4])*100, as.numeric(RF_model2_cv_accuracy[4])*100)

# Construct Dataframe for table and plot
df1 <- data.frame(Model, Pre, Learn, Train, Valid, Test)
df2 <- data.frame(Model, Learn, Train, Valid, Test)

# show in table
knitr::kable(df1, longtable = TRUE, booktabs = TRUE, digits = 2, col.names =c("Models", "Predictors", "Accuracy on Learn", "Accuracy on Train", "Accuracy on Valid",  "Accuracy on Test"),
             caption = 'The Comparision among 4 CV models'
)

# plot results in bar chat
df.long <- gather(df2, Dataset, Accuracy, -Model, factor_key =TRUE)
ggplot(data = df.long, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_col(position = position_dodge())

#############################################
## Multiple Models Comparison
#############################################
#
### Regression Model for Titanic
LR_Model <- glm(formula = Survived ~ Pclass + Title + Sex + Age_group + Group_size + Ticket_class  + Fare_pp + Embarked, family = binomial, data = trainData)

#summary(LR_Model_CV)
### Validate on trainData
Valid_trainData <- predict(LR_Model, newdata = trainData, type = "response") #prediction threshold
Valid_trainData <- ifelse(Valid_trainData > 0.5, 1, 0)  # set binary
#produce confusion matrix
confusion_Mat<- confusionMatrix(as.factor(trainData$Survived),as.factor(Valid_trainData))

# accuracy on traindata
Regression_Acc_Train <- round(confusion_Mat$overall["Accuracy"]*100,2)
paste('Model Train Accuracy =', Regression_Acc_Train)

### Validate on validData
validData_Survived_predicted <- predict(LR_Model, newdata = validData, type = "response")
validData_Survived_predicted  <- ifelse(validData_Survived_predicted  > 0.5, 1, 0)  # set binary prediction threshold
conMat<- confusionMatrix(as.factor(validData$Survived),as.factor(validData_Survived_predicted))

Regression_Acc_Valid <-round(conMat$overall["Accuracy"]*100,2)
paste('Model Valid Accuracy =', Regression_Acc_Valid)

### produce a prediction on test data
library(pROC)
auc(roc(trainData$Survived,Valid_trainData))  # calculate AUROC curve
#predict on test
test$Survived <- predict(LR_Model, newdata = test, type = "response")
test$Survived <- ifelse(test$Survived > 0.5, 1, 0)  # set binary prediction threshold
submit <- data.frame(PassengerId = test$PassengerId, Survived = as.factor(test$Survived))

write.csv(submit, file = "LG_model1_CV.CSV", row.names = FALSE)
# Kaggle test accuracy score:0.76555

# record accuracy
Regr_Acc <- c(Regression_Acc_Train, Regression_Acc_Valid, 0.76555)

acc_names <- c("Train Accu", "Valid Accu", "Test Accu")
names(Regr_Acc) <- acc_names
Regr_Acc

### Support Vector Machine Model for Titanic
#load library
library(e1071)

# fit the model using default parameters
SVM_model<- svm(Survived ~ Pclass + Title + Sex + Age_group + Group_size + Ticket_class + Fare_pp + Deck + HasCabinNum + Embarked, data=trainData, kernel = 'radial', type="C-classification")

#summary(SVM_model)
### Validate on trainData
Valid_trainData <- predict(SVM_model, trainData)
#produce confusion matrix
confusion_Mat<- confusionMatrix(as.factor(trainData$Survived), as.factor(Valid_trainData))

# output accuracy
AVM_Acc_Train <- round(confusion_Mat$overall["Accuracy"]*100,4)
paste('Model Train Accuracy =', AVM_Acc_Train)

### Validate on validData
validData_Survived_predicted <- predict(SVM_model, validData) #produce confusion matrix
conMat<- confusionMatrix(as.factor(validData$Survived), as.factor(validData_Survived_predicted))
# output accuracy
AVM_Acc_Valid <- round(conMat$overall["Accuracy"]*100,4)
paste('Model Valid Accuracy =', AVM_Acc_Valid)

### make prediction on test
# SVM failed to produce a prediction on test because test has Survived col and it has value NA. A work around is assign it with a num like 1.
test$Survived <-1

# predict results on test
Survived <- predict(SVM_model, test)
solution <- data.frame(PassengerId=test$PassengerId, Survived =Survived)
write.csv(solution, file = 'svm_predicton.csv', row.names = F)

# prediction accuracy on test
SVM_Acc <- c(AVM_Acc_Train, AVM_Acc_Valid, 0.78947)
names(SVM_Acc) <- acc_names

# print out
SVM_Acc

### Neural Network Models
# load library
library(nnet)

# train the model
xTrain = train[ , c("Survived", "Pclass","Title", "Sex","Age_group","Group_size", "Ticket_class", "Fare_pp", "Deck", "HasCabinNum", "Embarked")]

NN_model1 <- nnet(Survived ~ ., data = xTrain, size=10, maxit=500, trace=FALSE)

#How do we do on the training data?
nn_pred_train_class = predict(NN_model1, xTrain, type="class" )  # yields "0", "1"
nn_train_pred = as.numeric(nn_pred_train_class ) #transform to 0, 1
confusion_Mat<-confusionMatrix(as.factor(nn_train_pred), train$Survived)
# output accuracy
NN_Acc_Train <- round(confusion_Mat$overall["Accuracy"]*100,4)
paste('Model Train Accuracy =', NN_Acc_Train)

#How do we do on the valid data?
nn_pred_valid_class = predict(NN_model1, validData, type="class" )  # yields "0", "1"
nn_valid_pred = as.numeric(nn_pred_valid_class ) #transform to 0, 1
confusion_Mat<-confusionMatrix(as.factor(nn_valid_pred), validData$Survived)
# output accuracy
NN_Acc_Valid <- round(confusion_Mat$overall["Accuracy"]*100,4)
paste('Model valid Accuracy =', NN_Acc_Valid)

#make a prediction on test data
nn_pred_test_class = predict(NN_model1, test, type="class" )  # yields "0", "1"
nn_pred_test = as.numeric(nn_pred_test_class ) #transform to 0, 1
solution <- data.frame(PassengerId=test$PassengerId, Survived = nn_pred_test)
write.csv(solution, file = 'NN_predicton.csv', row.names = F)

###
# 0.8934,0.8547, 0.71052
NN_Acc <- c(NN_Acc_Train, NN_Acc_Valid, 0.71052)
names(NN_Acc) <- acc_names
NN_Acc

### Comparision among Different Models

library(tidyr)
Model <- c("Regression","SVM","NN", "Decision tree", "Random Forest")
Train <- c(Regression_Acc_Train, AVM_Acc_Train, NN_Acc_Train, 82.72, 83.16)
Valid <- c(Regression_Acc_Valid, AVM_Acc_Valid, NN_Acc_Valid, 81.01, 92)
Test <- c(76.56, 78.95, 71.05, 77.75, 78.95)
df1 <- data.frame(Model, Train, Valid, Test)

# show in table
knitr::kable(df1, longtable = TRUE, booktabs = TRUE, digits = 2, col.names =c("Models", "Accuracy on Train", "Accuracy on Valid","Accuracy on Test"),
             caption = 'The Comparision among 3 Machine Learning Models'
)

# plot in bar chat
df.long <- gather(df1, Dataset, Accuracy, -Model, factor_key =TRUE)
ggplot(data = df.long, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_col(position = position_dodge())

