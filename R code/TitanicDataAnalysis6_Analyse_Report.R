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
# Report and further improvement
# ####################################################
#prepare data for the code to be run independent of other chapters
data <- read.csv("data.csv", header = TRUE)
RE_data <- read.csv("RE_data.csv", header = TRUE)
train <- RE_data[1:891, ]
test <- RE_data[892:1309, ]

# model need to be loaded in to memory
load("RF_model2.rda")
RF_model2

# The model's estimated accuracy (by the model construction) is **83.16%**.
# The default parameters of the model are: `mtry = 2` and `ntree = 500`

# The Top 10 trees and the summary of OOB of RF_model2
head(RF_model2$err.rate, 10)
summary(RF_model2$err.rate)

#The cross validations on the model `RF_model2`,
load("RF_model2_cv.rda")
RF_model2_cv

# 2D Visualization of Model RE_model2's Prediciton
#install.packages("Rtsne")
# library(Rtsne)
# library(ggplot2)

# Rtsne needs a seed to ensure consistent output between runs.
set.seed(984357)
features <- c("Sex", "Fare_pp", "Pclass", "Title", "Age_group", "Group_size", "Ticket_class", "Embarked")
#generate 2-d coordinate
Model_tsne <- Rtsne(train[, features], check_duplicates = FALSE)
# Plot
ggplot(NULL, aes(x = Model_tsne$Y[, 1], y = Model_tsne$Y[, 2], color = as.factor(train$Survived))) +
  geom_point() +
  labs(color = "Survived")

# The importance of the predictorsRF_model2
# library(randomForest)
# library(caret)
varImpPlot(RF_model2, main = "")

# print out the values
pre.or <- sort(RF_model2$importance[,3], decreasing = TRUE)
print(pre.or)

###############################
## Further Analysis
###############################

# The decision tree of RF_model2
# library(rpart.plot)
load("model3.rda")
prp(model3, type = 0, extra = 1, under = TRUE)


## further re-engineering `Title` attribute
#let us further re-Engineer title check the value
table(RE_data$Title)

# Parse out title from the raw data
data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)
table(data$Title)

# Further bin or bucket them into a more appropriate category
# We can do with the knowledge of nobility, locality (country of origin)
# and other knowledge such as time (at the beginning of the 20 century).
# For example, "`Dona`" and "`the Countess`" are female nobility equivalent
# to "`Lady`", and  "`Ms`" and "`Mlle`" are essentially the same with "`Miss`";
# "`Mme`" is a military title equivalent to "`Madame`", so it can be
# categorised as "`Mrs`"; "`Jonkheer`" is an honorific nobility in the
# Netherlands; and "`Don`" is title of a university lecturer, they can be
# categorises as "`Sir`"; "`Col`", "`Capt`", and "`Major`" are military
# ranks and can be replaced with a more general title "`Officer`".
# With all of these, we can reduce the numbers of title's category.

# Re-map titles to be more exact
data$Title[data$Title %in% c("Dona", "the Countess")] <- "Lady"
data$Title[data$Title %in% c("Ms", "Mlle")] <- "Miss"
data$Title[data$Title == "Mme"] <- "Mrs"
data$Title[data$Title %in% c("Jonkheer", "Don")] <- "Sir"
data$Title[data$Title %in% c("Col", "Capt", "Major")] <- "Officer"
table(data$Title)

# Collapse titles based on visual analysis
indexes <- which(data$new.Title == "Lady" |
                   (data$new.Title == "Dr" &
                      data$Sex == "female") |
                   (data$new.Title == "Officer"&
                      data$Sex == "female")
)
data$new.Title[indexes] <- "Mrs"

indexes <- which(data$new.Title == "Rev" |
                   data$new.Title == "Sir" |
                   (data$new.Title == "Officer" &
                      data$Sex == "male")|
                   (data$new.Title == "Dr" &
                      data$Sex == "male")  )
data$new.Title[indexes] <- "Mr"

table(data$new.Title)

# Check any other gender slip-ups?
length(which(data$sex == "female" &
               (data$new.Title == "Master" |
                  data$new.Title == "Mr")))
length(which(data$sex == "male" &
               (data$new.Title == "Miss" |
                  data$new.Title == "Mrs")))

# Visualize
ggplot(data[1:891,], aes(x = new.Title, fill = as.factor(Survived))) +
  geom_bar()

# check up the impact of the new-title
set.seed(2222)
RE_data$New_Title <- data$new.Title
RF_model2_new <- randomForest(as.factor(Survived) ~ Sex + Fare_pp + Pclass + New_Title + Age_group + Group_size + Ticket_class  + Embarked,
                              data = RE_data[1:891,],
                              importance=TRUE)

RF_model2_new

# The new model has increased the over accuracy with 0.45%.
# It is not a lot but it approves the point that features re-engineer
# is a place to do a model's performance improvement.
