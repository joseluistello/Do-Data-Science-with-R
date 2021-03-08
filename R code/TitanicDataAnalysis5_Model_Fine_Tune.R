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
###########################################################################
# Model Fine Tune
###########################################################################
#
###############################
### Tuning a model's Predictor
###############################
# load necessary library
library(randomForest)
library(plyr)
library(caret)

# load our re-engineered data set and separate train and test datasets
RE_data <- read.csv("RE_data.csv", header = TRUE)
train <- RE_data[1:891, ]
test <- RE_data[892:1309, ]

# Train a Random Forest with the default parameters using full attributes
# Survived is our response variable and the rest can be predictors except pasengerID.

rf.train <- subset(train, select = -c(PassengerId, Survived))

# we set rf.label for later use but it is the dependednt variable
rf.label <- as.factor(train$Survived)

# RandomForest cannot handle factors with over 53 levels
rf.train$Ticket <- as.numeric(train$Ticket)

set.seed(1234) # for reproduction
rf.1 <- randomForest(x = rf.train, y = rf.label, importance = TRUE)
rf.1

#rf.1 model with full house predictors has error rate: 15.49%

# Check the order of the predictors prediction power.
pre.or <- sort(rf.1$importance[,3], decreasing = TRUE)
pre.or
varImpPlot(rf.1)

## The idea in here is using the predictors order (prediction)
# to build all models each has one predictor less. so we can
# compare models to find which is has the best accuracy
# WE can take that model as the best to bench marking the predictor.

# rf.2 as an example
rf.train.2 <- subset(rf.train, select = -c(Parch))
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE)
rf.2
#The *rf.2*  model's accuracy 84.85%,  error` (15.15%).

### we can repeat the process to get all the models and their accuracy
# List themin a table
library(tidyr)

Model <- c("rf.1","rf.2","rf.3","rf.4","rf.5","rf.6","rf.7","rf.8","rf.9","rf.10","rf.11","rf.12","rf.13","rf.14","rf.15","rf.16")
Pre <- c("Sex", "Title", "Fare_pp", "Ticket_class", "Pclass", "Ticket", "Age", "Friend_size", "Deck", "Age_group", "Group_size", "Family_size", "HasCabinNum", "SibSp", "Embarked", "Parch")
#Produce models predictor list
Pred <- rnorm(16)
tem <- NULL
for (i in 1:length(Pre)) {
  tem  <- paste(tem, Pre[i], sep = " ")
  #Using environment variable setting
  ls  <- paste("Pred[",i,"]", sep="")
  eq  <- paste(paste(ls, "tem", sep="<-"), collapse=";")
  eval(parse(text=eq))
}
Pred <- sort(Pred, decreasing = TRUE)

Error <- c(15.49, 15.15, 14.93, 15.26, 14.7, 14.7, 14.03, 13.58, 14.48, 15.6, 16.27, 16.95, 17.51, 20.31, 20.76, 21.32)
Accuracy <- 100 - Error

df <- data.frame(Model, Pred, Accuracy)

knitr::kable(df, longtable = TRUE, booktabs = TRUE, digits = 2, col.names =c("Models", "Predictors", "Accuracy"),
             caption = 'Model Predictors Comparision'
)

# load the best model and record its predictors
# save(rf.8, file = "rf_model.rda")
load("rf_model.rda")

Predictor <- c("Sex, Title, Fare_pp, Ticket_class, Pclass, Ticket, Age, Friend_size, Deck")
Predictor

#################################
### Tuning Training Data Samples
#################################
# Set Prediction Accuracy Benchmark

# Let's start with a submission of rf.8 to Kaggle
# to find the difference between model's OOB and the accuracy

# Subset our test records and features
test.submit.df <- test[, c("Sex", "Title", "Fare_pp", "Ticket_class", "Pclass", "Ticket", "Age", "Friend_size", "Deck")]
test.submit.df$Ticket <- as.numeric(test.submit.df$Ticket)

# Make predictions
rf.8.preds <- predict(rf.8, test.submit.df)
table(rf.8.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = test$PassengerId, Survived = rf.8.preds)

write.csv(submit.df, file = "RF8_SUB.csv", row.names = FALSE)
# After our submission we have scores 0.75598 from Kaggle,
# but the OOB predicts that we should score 0.8642.

#
### The 1 sampling methods: 10 Folds CV Repeat 10 Times
#
# check to see if the samples are the same or close to the same ratio
library(caret)
library(doSNOW)

set.seed(2348)
# rf.label is the Survived in the train dataset.
# ? createMultiFolds to find out more. train (891)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification: survived ratio in the train dataset
table(rf.label)
342 / 549

# check 10-folds random split each folds ratio (34 is an example)
table(rf.label[cv.10.folds[[34]]])
308 / 494
#confirmed the stratification both have the similer ratio

# Set up caret's trainControl object using 10-folds repeated CV
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# Model construction with "`10-folds repeated CV`" is a very expensive
# R has a package called **"doSNOW"**, that facilities the use of
# multi-core processor and permits parallel computing in
# a pseudo cluster mode

## Set up doSNOW package for multi-core training.
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
# # Set seed for reproducibility and train
# set.seed(34324)
#
# rf.8.cv.1 <- train(x = rf.train.8, y = rf.label, method = "rf", tuneLength = 3, ntree = 500, trControl = ctrl.1)
#
# #Shutdown cluster
# stopCluster(cl)
# save(rf.8.cv.1, file = "rf.8.cv.1.rda")
# Check out results
load("rf.8.cv.1.rda")
# Check out results
rf.8.cv.1

# prediction accuracy reduced from 0.8642 to 0.8511,
# but not pessimistic enough to the test accuracy, it is 0.75598.

#
### The 2 sampling methods: 5 Folds CV Repeat 10 Times
#
set.seed(5983)
# cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)
#
# ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)
#
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
# set.seed(89472)
# rf.8.cv.2 <- train(x = rf.train.8, y = rf.label, method = "rf", tuneLength = 3, ntree = 500, trControl = ctrl.2)
#
# #Shutdown cluster
# stopCluster(cl)
# save(rf.8.cv.2, file = "rf.8.cv.2.rda")
# # Check out results
load("rf.8.cv.2.rda")
# Check out results
rf.8.cv.2

# We can see that 5-fold CV is a little better.
# The accuracy is moved under 85%. The model's training data set
# is moved from 9/10 to 4/5, which is 713 now.

#
### The 3 sampling methods: 3 Folds CV Repeat 10 Times
#
set.seed(37596)
# cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
#
# ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)
#
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
# set.seed(94622)
# rf.8.cv.3 <- train(x = rf.train.8, y = rf.label, method = "rf", tuneLength = 3, ntree = 500, trControl = ctrl.3)
#
# #Shutdown cluster
# stopCluster(cl)
#
# save(rf.8.cv.3, file = "rf.8.cv.3.rda")
# # # Check out results
load("rf.8.cv.3.rda")
# Check out results
rf.8.cv.3

# We can see the accuracy has further decreased (0.8387579).
# Let us also reduced the number of times that the samples
# are repeated used in the training (repeat times).
# Let us see if the sample repeat times reduce to 3,
# if the accuracy can be further reduced.

#
### The 4 sampling methods: 5 Folds CV Repeat 10 Times
#
# set.seed(396)
# cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 3)
#
# ctrl.4 <- trainControl(method = "repeatedcv", number = 3, repeats = 3, index = cv.3.folds)
#
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
# set.seed(9622)
# rf.8.cv.4 <- train(x = rf.train.8, y = rf.label, method = "rf", tuneLength = 3, ntree = 50, trControl = ctrl.4)
#
# #Shutdown cluster
# stopCluster(cl)
#save(rf.8.cv.4, file = "rf.8.cv.4.rda")
# # # Check out results
load("rf.8.cv.4.rda")
# Check out results
rf.8.cv.4

#################################
### Tuning Model’s Parameters
#################################
# random  search to find mtry for RF

#library(caret)
#library(doSNOW)

### Random Search
set.seed(2222)
# #use teh best sampling results that is K=3 ant T=10
# cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
#
# # Set up caret's trainControl object.
# ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds, search="random")
#
# # set up cluster for parallel computing
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
# # Set seed for reproducibility and train
# set.seed(34324)
#
# #use rf.train.8 with 9 predictors
#
# #RF_Random <- train(x = rf.train.8, y = rf.label, method = "rf", tuneLength = 15, ntree = 500, trControl = ctrl.1)
# #save(RF_Random, file = "RF_Random_search.rda")
#
# #Shutdown cluster
# stopCluster(cl)

# Check out results
load("RF_Random_search.rda")
print(RF_Random)

# plot
plot(RF_Random)
### mtry =3 by random search

### Grid Search
# ctrl.2 <- trainControl(method="repeatedcv", number=3, repeats=10, index = cv.3.folds, search="grid")
#
# set.seed(3333)
# # set Grid search with a vector from 1 to 15.
#
# tunegrid <- expand.grid(.mtry=c(1:15))
#
# # set up cluster for parallel computing
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
#
# #RF_grid_search <- train(y = rf.label, x = rf.train.8,  method="rf", metric="Accuracy", trControl = ctrl.2, tuneGrid = tunegrid, tuneLength = 15, ntree = 500)
#
#
# #Shutdown cluster
# stopCluster(cl)
# #save(RF_grid_search, file = "RF_grid_search.rda")

load("RF_grid_search.rda")
#print results
print(RF_grid_search)
#plot
plot(RF_grid_search)

##
### Manual Search we use control 1 random search
##  use n_tree in c(100, 500, 1000, 1500)
model_list <- list()

tunegrid <- expand.grid(.mtry = 3)
control <- trainControl(method="repeatedcv", number=3, repeats=10, search="grid")

# # the following code have been commented out just for produce the markdown file. so it will not wait for ran a long time
# # set up cluster for parallel computing
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
#
# #loop through different settings
#
# for (n_tree in c(100, 500, 1000, 1500)) {
#
#   set.seed(3333)
#   fit <- train(y = rf.label, x = rf.train.8,  method="rf", metric="Accuracy",  tuneGrid=tunegrid, trControl= control, ntree=n_tree)
#
#   key <- toString(n_tree)
#   model_list[[key]] <- fit
# }
#
# #Shutdown cluster
# stopCluster(cl)
# save(model_list, file = "RF_manual_search.rda")
# # the above code comneted out for output book file

load("RF_manual_search.rda")
# compare results
results <- resamples(model_list)
summary(results)
dotplot(results)
# We can see with the default *mtry =3* setting,
# the best *ntree* value is 1500.
# The model can reach 84.31% accuracy

###submit the final result to Kaggle for evaluation
set.seed(1234)

tunegrid <- expand.grid(.mtry = 3)
control <- trainControl(method="repeatedcv", number=3, repeats=10, search="grid")

# # # the following code have been commented out just for produce the markdown file. so it will not wait for ran a long time
# # # set up cluster for parallel computing
# cl <- makeCluster(6, type = "SOCK")
# registerDoSNOW(cl)
#
# Final_model <- train(y = rf.label, x = rf.train.8,  method="rf", metric="Accuracy",  tuneGrid=tunegrid, trControl= control, ntree=1500)
#
# #Shutdown cluster
# stopCluster(cl)
#
# save(Final_model, file = "Final_model.rda")
# # # the above code commented out for output book file

load("Final_model.rda")

# Make predictions
Prediction_Final <- predict(Final_model, test.submit.df)
#table(Prediction_Final)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = test$PassengerId, Survived = Prediction_Final)

write.csv(submit.df, file = "Prediction_Final.csv", row.names = FALSE)

## We have got a score of 0.76076
