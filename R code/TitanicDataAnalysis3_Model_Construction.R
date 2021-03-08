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
# Tutorial One - Understand Data
#
# Understand data involves three steps:
# 1. Load data into memory - RStudio
# 2. Assess Data quantity
# 3. Assess data quality - set goals for next step in data preprocess
##########################################################################
####################################################
# Prediction Model Construction
####################################################
# This file contains prediction model construction
#
#  1. Decision trees
#  2. Random forest
#
##### Predictor selection
# load Library
library(dplyr)# data manipulation
library(tidyverse)
library(caret) # tuning & cross-validation
library(gridExtra) # visualizations


#load re-engineered dataset
RE_data <- read.csv("RE_data.csv", header = TRUE)

# check data
glimpse(RE_data)
summary(RE_data)

### A quick correlation analysis
# and plot of the numeric attributes
# to get an idea of how they might relate to one another.

# convert non numeric types to numeric
RE_data <- RE_data %>% mutate_if(is.factor,  as.numeric)

# plot correlation among numeric attributes
cor <- RE_data %>% select(., -c(Ticket, PassengerId)) %>%
  cor(use="pairwise.complete.obs") %>%
  corrplot::corrplot.mixed(upper = "circle", tl.col = "black")

# show results in a table
library(kableExtra) # markdown tables
lower <- round(cor,2)
lower[lower.tri(cor, diag=TRUE)]<-""
lower <- as.data.frame(lower)
knitr::kable(lower, booktabs = TRUE,
             caption = 'Coorelations among attributes') %>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", font_size = 8))

### PCA Analysis
# Calculate Eigenvalues of the attributes
data.pca <- prcomp(RE_data[1:891,c(-1, -2)], center = TRUE, scale = TRUE)
summary(data.pca)

library(AMR)
#AMR::ggplot_pca(data.pca)
ggplot_pca(data.pca) #default shows PC1 and PC2, the two most imprtance attibutes
#biplot(data.pca)

#plot other components for example PC3 and PC4
ggplot_pca(data.pca, ellipse=TRUE, choices=c(3,4))

################################################
### Decision tree models
################################################
#load rpart the library which support decision tree
library(rpart)
# Build our first model with Rpart, only use Sex attribute
# load our re-engineered data set and separate train and test datasets
RE_data <- read.csv("RE_data.csv", header = TRUE)
train <- RE_data[1:891, ]
test <- RE_data[892:1309, ]

#build a decision tree model use rpart.
model1 <- rpart(Survived ~ Sex, data = train, method="class")

### Assess model's performance
#library caret is a comprehensive library support all sorts of model analysis

library(caret)
options(digits=4) # set decimal points of numbers.
# assess the model's accuracy by make a prediction on the train data.
Predict_model1_train <- predict(model1, train, type = "class")
#build a confusion matrix to make comparison
conMat <- confusionMatrix(as.factor(Predict_model1_train), as.factor(train$Survived))
#show confusion matrix
conMat$table
#show percentage of same values - accuracy
predict_train_accuracy <- conMat$overall["Accuracy"]
predict_train_accuracy

# The firs prediction by the first decision tree
Prediction1 <- predict(model1, test, type = "class")

# produce a submit with Kaggle required format that is only two attributes: PassengerId and Survived
submit1 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction1)
# Write it into a file "Tree_Model1.CSV"
write.csv(submit1, file = "Tree_Model1.CSV", row.names = FALSE)

##Submit to kaggle and get a score: 0.76555

# Inspect prediction
summary(submit1$Survived)
prop.table(table(submit1$Survived, dnn="Test survive percentage"))
#train survive ratio
prop.table(table(as.factor(train$Survived), dnn="Train survive percentage"))

##
## The result shows that among total of 418 passenger in the test dataset,
## 266 passenger predicted perished (with survived value 0), which counts
## as 64% and 152 passenger predicted to be survived (with survived value 1)
## and which count as 36%. This is not too far from the radio on the
## training dataset, which was 62% survived and 38% perished.

# add Sex back to the submit and form a new data frame called compare
compare <- data.frame(submit1[1], Sex = test$Sex, submit1[2])
# Check train sex and Survived ratios
prop.table(table(train$Sex, train$Survived), margin = 1)
# Check predicted sex radio
prop.table(table(compare$Sex, dnn="Gender ratio in Test"))
#check predicted Survive and Sex radio
prop.table(table(compare$Sex, compare$Survived), margin = 1)

## It is clear that our model is too simple: it predicts any male
## will be perished and every female will be survived! This is approved
## by the gender (male and female) ratio in the test dataset is identical
## to the death ratio in our prediction result.

#plot our decision tree
# load some useful libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#
prp(model1)
fancyRpartPlot(model1)

### Tree Model2 with 5 Core Predictors
# A tree model with the top five attributes
set.seed(1234)
model2 <- rpart(Survived ~ Sex + Pclass + HasCabinNum + Deck + Fare_pp, data = train, method="class")

# Assess model's accuracy with train data
Predict_model2_train <- predict(model2, train, type = "class")
conMat <- confusionMatrix(as.factor(Predict_model2_train), as.factor(train$Survived))
conMat$table
#conMat$overall
predict2_train_accuracy <- conMat$overall["Accuracy"]
predict2_train_accuracy

# make a prediction and submit to Kaggle
Prediction2 <- predict(model2, test, type = "class")
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction2)
write.csv(submit2, file = "Tree_model2.CSV", row.names = FALSE)

### kaggle score: **0.76555**
# plot our full house classifier
prp(model2, type = 0, extra = 1, under = TRUE)
# plot our full house classifier
fancyRpartPlot(model2)

# build a comparison data frame to record each prediction results
Tree_compare <- data.frame(test$PassengerId, predict1=Prediction1, predict2=Prediction2)
# Find differences
dif <- Tree_compare[Tree_compare[2]!=Tree_compare[3], ]
#show dif
dif

### Tree model3 construction using more predictors
model3 <- rpart(Survived ~ Sex + Fare_pp + Pclass + Title + Age_group + Group_size + Ticket_class  + Embarked,
                data=train,
                method="class")
# This model will be used in later chapters so save it in to a file for later to be loaded into memory
save(model3, file = "model3.rda")

#Assess prediction accuracy on train data
Predict_model3_train <- predict(model3, train, type = "class")
conMat <- confusionMatrix(as.factor(Predict_model3_train), as.factor(train$Survived))
conMat$table

#conMat$overall
predict3_train_accuracy <- conMat$overall["Accuracy"]
predict3_train_accuracy

# make prediction and submission, Score: 0.77033
Prediction3 <- predict(model3, test, type = "class")
submit3<- data.frame(PassengerId = test$PassengerId, Survived = Prediction3)
write.csv(submit3, file = "tree_model3.CSV", row.names = FALSE)

# plot our full house classifier
prp(model3, type = 0, extra = 1, under = TRUE)
# plot our full house classifier
fancyRpartPlot(model3)

# build a comparison data frame to record each prediction results
compare <- data.frame(test$PassengerId, predict2 = Prediction2 , predict3 = Prediction3)
# Find differences
dif <- compare[compare[2] != compare[3], ]
#show dif
print.data.frame(dif, row.names = FALSE)

### Tree model4, full-house classifier apart from name and ticket
model4 <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Embarked + HasCabinNum + Friend_size + Fare_pp + Title + Deck + Ticket_class + Family_size + Group_size + Age_group,
                #model4 <- rpart(Survived ~ .,
                data=train,
                method="class")
#assess prediction accuracy on train data
Predict_model4_train <- predict(model4, train, type = "class")
conMat <- confusionMatrix(as.factor(Predict_model4_train), as.factor(train$Survived))
conMat$table
#conMat$overall
predict4_train_accuracy <- conMat$overall["Accuracy"]
predict4_train_accuracy

# make prediction and submission. Kaggle score: 0.75119
Prediction4 <- predict(model4, test, type = "class")
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction4)
write.csv(submit4, file = "Tree_model4.CSV", row.names = FALSE)

# plot our full house classifier
prp(model4, type = 0, extra = 1, under = TRUE)
fancyRpartPlot(model4)

# build a comparison data frame to record each prediction results
compare <- data.frame(test$PassengerId, predict3 = Prediction3 , predict4 = Prediction4)
# Find differences
dif2 <- compare[compare[2] != compare[3], ]
#show dif
dif2

library(tidyr)
# Tree models comparison
Model <- c("Model1","Model2","Model3","Model4")
Pre <- c("Sex", "Sex, Pclass, HasCabinNum, Deck, Fare_pp", "Sex, Fare_pp, Pclass, Title, Age_group, Group_size, Ticket_class, Embarked", "All")
Train <- c(78.68, 81.48, 85.19, 85.41)
Test <- c(76.56, 76.56, 77.03, 75.12)
df1 <- data.frame(Model, Pre, Train, Test)
df2 <- data.frame(Model, Train, Test)
knitr::kable(df1, longtable = TRUE, booktabs = TRUE, digits = 2, col.names =c("Models", "Predictors", "Accuracy on Train", "Accuracy on Test"),
             caption = 'The Comparision among 4 decision tree models'
)
# bar plot of comparison
df.long <- gather(df2, Dataset, Accuracy, -Model, factor_key =TRUE)
ggplot(data = df.long, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_col(position = position_dodge())

##############################################
#  Random Forest models
##############################################
# Install the random forest library, if you have not
# install.packages('randomForest')
# load library
library(randomForest)
library(plyr)
# library(caret)

# load data if you have not
RE_data <- read.csv("RE_data.csv", header = TRUE)

# RE_data <- mutate_if(RE_data, is.numeric, as.factor)
#
train <- RE_data[1:891, ]
test <- RE_data[892:1309, ]

# convert variables into factor because RF can be Classification and Regression
train$Survived <- as.factor(train$Survived)
# convert other attributes which really are categorical data but in form of numbers
train$Pclass <- as.factor(train$Pclass)
train$Group_size <- as.factor(train$Group_size)
#confirm types
sapply(train, class)

# Build the random forest model1 uses Pclass, Sex, HasCabinNum, Deck and Fare_pp
set.seed(1234) #for reproduction
RF_model1 <- randomForest(Survived ~ Sex + Pclass + HasCabinNum
                          + Deck + Fare_pp,
                          data=train, importance=TRUE)

# Make your prediction using the validate dataset
RF_prediction1 <- predict(RF_model1, train)

#check up
conMat<- confusionMatrix(RF_prediction1, train$Survived)
conMat$table

# Misclassification error
paste('Accuracy =', round(conMat$overall["Accuracy"],2))
paste('Error =', round(mean(train$Survived != RF_prediction1), 2))

# produce a submit with Kaggle required format that is only two attributes: PassengerId and Survived
test$Pclass <- as.factor(test$Pclass)
test$Group_size <- as.factor(test$Group_size)

# make prediction, Kaggle score is: 0.76555
RF_prediction <- predict(RF_model1, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = RF_prediction)
# Write it into a file "RF_Result.CSV"
write.csv(submit, file = "RF_Result1.CSV", row.names = FALSE)

# Record the results
RF_model1_accuracy <- c(80, 84, 76.555)

### RE_model2 with more predictors
set.seed(2222)
RF_model2 <- randomForest(as.factor(Survived) ~ Sex + Fare_pp + Pclass + Title + Age_group + Group_size + Ticket_class  + Embarked,
                          data = train,
                          importance=TRUE)
# This model will be used in later chapters, so save it in a file and it can be loaded later.
save(RF_model2, file = "RF_model2.rda")

# RF_model2 Prediction
RF_prediction2 <- predict(RF_model2, train)
#check up
conMat<- confusionMatrix(RF_prediction2, train$Survived)
conMat$table
# Misclassification error
paste('Accuracy =', round(conMat$overall["Accuracy"],2))
paste('Error =', round(mean(train$Survived != RF_prediction2), 2))
# produce a submission and submit to Kaggle
test$Pclass <- as.factor(test$Pclass)
test$Group_size <- as.factor(test$Group_size)

# make prediction on test and submit. Score: 0.78947
RF_prediction <- predict(RF_model2, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = RF_prediction)
# Write it into a file "RF_Result.CSV"
write.csv(submit, file = "RF_Result2.CSV", row.names = FALSE)

# Record RF_model2's results
RF_model2_accuracy <- c(83.16, 92, 78.95)

### RF_model3 construction with the maximum predictors
set.seed(2233)
RF_model3 <- randomForest(Survived ~ Sex + Pclass + Age +
                            SibSp + Parch + Embarked +
                            HasCabinNum + Friend_size +
                            Fare_pp + Title + Deck +
                            Ticket_class + Family_size
                          + Group_size + Age_group,
    data = train, importance=TRUE)

# Display RE_model3's details
RF_model3
# Make a prediction on Train
RF_prediction3 <- predict(RF_model3, train)
#check up
conMat<- confusionMatrix(RF_prediction3, train$Survived)
conMat$table
# Misclassification error
paste('Accuracy =', round(conMat$overall["Accuracy"],2))
paste('Error =', round(mean(train$Survived != RF_prediction3), 2))

# produce a submit with Kaggle
test$Pclass <- as.factor(test$Pclass)
test$Group_size <- as.factor(test$Group_size)

# make prediction, Kaggle score: 0.77033
RF_prediction <- predict(RF_model3, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = RF_prediction)
# Write it into a file "RF_Result.CSV"
write.csv(submit, file = "RF_Result3.CSV", row.names = FALSE)

# Record RF_model3's results
RF_model3_accuracy <- c(83, 94, 77)


### RF_models Comparison
library(tidyr)
Model <- c("RF_Model1","RF_Model2","RF_Model3")
Pre <- c("Sex, Pclass, HasCabinNum, Deck, Fare_pp", "Sex, Fare_pp, Pclass, Title, Age_group, Group_size, Ticket_class, Embarked", "Sex, Pclass, Age, SibSp, Parch, Embarked, HasCabinNum, Friend_size, Fare_pp, Title, Deck, Ticket_class, Family_size, Group_size, Age_group")

Learn <- c(80.0, 83.16, 83.0)
Train <- c(84, 92, 78)
Test <- c(76.555, 78.95, 77.03)
df1 <- data.frame(Model, Pre, Learn, Train, Test)
df2 <- data.frame(Model, Learn, Train, Test)
knitr::kable(df1, longtable = TRUE, booktabs = TRUE, digits = 2, col.names =c("Models", "Predictors", "Accuracy on Learn", "Accuracy on Train", "Accuracy on Test"),
             caption = 'The Comparision among 3 Random Forest models'
)
# plot bar comparison
df.long <- gather(df2, Dataset, Accuracy, -Model, factor_key =TRUE)
ggplot(data = df.long, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_col(position = position_dodge())














