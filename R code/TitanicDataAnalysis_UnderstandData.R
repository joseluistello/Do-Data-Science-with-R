############################################################################
# Copyright 2020 Gangmin Li
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

### Load raw data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# RStudio help function
# what help with any R function just type ?
# for example ?read.csv in Console

# First explore of datasets
# the first R code you can use is str.
# use ?str to find more

help(str)

# use str to explore train and test

str(train)
str(test)

# Add a "Survived" variable to the test set to allow for combining data sets
test <- data.frame(Survived = rep("NA", nrow(test)), test[,])

# Now check test. you can see that it has 12 variable now.
# And Survived is the first variable. because we used test[,]
# if you want add Survived into the second position Do this,
# test <- data.frame(test[1], Survived = rep("NA", nrow(test)), test[,2:ncol(test)])

# Combine datasets together. actually append test to train
data <- rbind(train, test)

#### We have done combined datasest with only a few line of code
# notice that the type of Survived has been changed to Chr.
# This is because we used "NA" as its value
# A bit about R data types
# ?str structure of dataset
# chr, int
# Factor in R is 'category'. it likes a selection from a list.
# for example, Cabin Factor w/187 levels. It means there are 187 selections.
# Sex Factor w/ 2 "female", "male", 2,1 means, two options 2- female, 1- male.
#
# NA : not available (absent value, missing value)

str(data)

# Exam PassengerID, type INT, we can check total number and the number of unique values.
# If they are equal and both equal to the number of records. it means there are
# unique and has no missing value.
length(data$PassengerId)
length(unique(data$PassengerId))

### Exam Survived
data$Survived <- as.factor(data$Survived)
table(data$Survived)

# Calculate the survive rate in train data is 38% and the death rate is 61.61%
prop.table(table(as.factor(train$Survived)))

### Examine Pclass value,
# Look into Kaggle's explanation about Pclass: it is a proxy for social class i.e. rich or poor
# It should be factor and it does not make sense to stay in int.
data$Pclass <- as.factor(data$Pclass)
test$Pclass <- as.factor(test$Pclass)
train$Pclass <- as.factor(train$Pclass)

# Distribution across classes
table(data$Pclass)

# Distribution across classes with survive
table(data$Pclass, data$Survived)

# Calculate the distribution on Pclass
# Overall passenger distribution on classes.
prop.table(table(data$Pclass))

# Train data passenger distribution on classes.
prop.table(table(train$Pclass))

# Test data passenger distribution on classes.
prop.table(table(test$Pclass))

# Calculate death distribution across classes with Train data
SurviveOverClass <- table(train$Pclass, train$Survived)
# Convert SurviveOverClass into data frame
SoC.data.fram <- data.frame(SurviveOverClass)
# Retrieve death distribution in classes
Death.distribution.on.class <- SoC.data.fram$Freq[SoC.data.fram$Var2==0]
prop.table(Death.distribution.on.class)

# Calculate death rate in train data
# Distribution across classes with survive in Train data
SurviveOverClass <- table(train$Pclass, train$Survived)
# Convert SurviveOverClass into data frame
SoC.data.fram <- data.frame(SurviveOverClass)
# Retrieve death distribution in classes
Death.distribution.on.class <- SoC.data.fram$Freq[SoC.data.fram$Var2==0]
prop.table(Death.distribution.on.class)

# Try summary on data
summary.data.frame(data)

## Exploratory data analysis with graph
# Load up ggplot2 package to use for visualizations
# load it into memory
library(ggplot2)

# High class passenger has more chance of survive than passenger with lower class
# Hypothesis - Rich passengers can but expensive ticket. class=1 is more expensive
# survived at a higher rate
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# It sort of proved the survive rate with social class
# more people perished in the third class

### Exam Name attribute
# the original name typed as factor, which we really don't want (shows the uniqueness)
# convert Name type
data$Name <- as.character(data$Name)

# Confirm the name has 1037 unique values
length(unique(data$Name))

# Find the two duplicate names
# First used which function to get the duplicate names and store them as a vector dup.names
# check it up ?which.
dup.names <- data[which(duplicated(data$Name)), "Name"]

# Echo out
dup.names

### Exam Sex attribute
# Retrial male and females. then check their numbers.
summary(data$Sex)

#Plot Sex distribution on entire dataset and get general an impression
ggplot(data[1:891,], aes(x = Sex)) +
  geom_bar(fill="steelblue") +
  xlab("Sex") +
  ylab("Total Count")

# plot Survived over Sex on train. use data[1:891,]
ggplot(data[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

### Examine Age
# Summary over data, train and test.
summary(data$Age)
summary(train$Age)
summary(test$Age)

#It makes sense to change Age type to Factor to see distribution
summary(as.factor(data$Age))

# Plot distribution of age group
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 10, fill="steelblue") +
  xlab("Age") +
  ylab("Total Count")

# Plot Survived on age group on train dataset
ggplot(data[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

### Exam SibSp, Its original type is int
summary((data$SibSp))

# How many possible unique values?
length(unique(data$SibSp))

# Treat it as a factor, so we know the value distribution
data$SibSp <- as.factor(data$SibSp)
summary(data$SibSp)

# Plot entire SibSp distribution among the 7 values
ggplot(data, aes(x = SibSp)) +
  geom_bar() +
  xlab("SibSp") +
  ylab("Total Count")+
  coord_cartesian()

# Plot on the Survived on SibSp
ggplot(data[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  xlab("SibSp") +
  ylab("Total Count")  +
  labs(fill = "Survived")

### Exam Parch
summary(data$Parch)

# How many possible values?
length(unique(data$Parch))

# Treat is as a factor so we know the value distribution
data$Parch <- as.factor(data$Parch)
summary(data$Parch)

# Plot entire Parch distribution among the 8 posibilites
ggplot(data, aes(x = Parch)) +
  geom_bar() +
  xlab("Parch") +
  ylab("Total Count")

# Plot on the Survived on SibSp
ggplot(data[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  xlab("Parch") +
  ylab("Total Count")  +
  labs(fill = "Survived")

### Exam  ticket
summary(data$Ticket)
length(unique(data$Ticket))
str(data$Ticket)
which(is.na(data$Ticket))

# Plot it value
ggplot(data[1:891,], aes(x = Ticket)) +
  geom_bar() +
  xlab("Ticket") +
  ylab("Total Count")

# Plot on the survive on Ticket
ggplot(data[1:891,], aes(x = Ticket, fill = Survived)) +
  geom_bar() +
  xlab("Ticket") +
  ylab("Total Count")  +
  labs(fill = "Survived")

### Examine Fare
summary(data$Fare)
length(unique(data$Fare))

# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

# Let's check to see if fare has predictive power
ggplot(data[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

# Explore Fare distribution between train and test to see if they are overlapped?
ggplot(train, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(test, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

### Cabin
# Examine cabin values
str(data$Cabin)
# Cabin really isn't a factor, make a string and the display first 100
data$Cabin <- as.character(data$Cabin)
data$Cabin[1:100]

# Find number of the missing value
table(train[which(train$Cabin ==""), "Cabin"])

# Analysis of the cabin variable
str(data$Cabin)

# Cabin really isn't a factor, make a string and the display first 100
data$Cabin <- as.character(data$Cabin)
data$Cabin[1:100]

# Find out number of the missing value in the train
train$Cabin <- as.character(train$Cabin)
table(train[which(train$Cabin ==""), "Cabin"])

# Replace empty cabins with a "U"
#data[which(data$Cabin == ""), "Cabin"] <- "U"
data$Cabin[1:100]

# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

# Add to combined data set and plot
data$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

### Examine Embark
str(data$Embarked)
summary(data$Embarked)

# Plot Embarked data distribution and the Survived data over it
ggplot(data, aes(x = Embarked)) +
  geom_bar(width=0.5) +
  xlab("Passenger embarked port") +
  ylab("Total Count")

ggplot(data[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar(width=0.5) +
   xlab("embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")

##Calculate death RATE distribution over Embarked port with Train data
# We use table in R, you can check with ?table. A good example is
# mytable <- table(A,B) # A will be rows, B will be columns
# mytable # print table

# margin.table(mytable, 1) # A frequencies (summed over B)
# margin.table(mytable, 2) # B frequencies (summed over A)

# prop.table(mytable) # cell percentages
# prop.table(mytable, 1) # row percentages
# prop.table(mytable, 2) # column percentages

# We need prop.table to get column percentage which is the survived

# creat Embarked and Survived contingency table
SurviveOverEmbarkedTable <- table(train$Embarked, train$Survived)
# Death-0/survived-1 value distribution (percentage) based on embarked ports
# prop.table(mytable, 2) give us column (Survived) percentages
Deathandsurvivepercentage <- prop.table(SurviveOverEmbarkedTable, 2)
# Plot
M <- c("c-Cherbourg", "Q-Queenstown", "S-Southampton")
barplot(Deathandsurvivepercentage[2:4,1]*100, xlab =(""), ylim=c(0,100), ylab="Death distribution in percentage %",  names.arg = M, col="steelblue", main="Death distribution", border="black", beside=TRUE)
barplot(Deathandsurvivepercentage[2:4,2]*100, xlab =(""), ylim=c(0,100), ylab="Death distribution in percentage %",  names.arg = M, col="blue", main="Death distribution", border="black", beside=TRUE)

## Calculate survived RATE distribution based on embarked ports
# Death-0/survived-1 value distribution (percentage) based on embarked ports
# prop.table(mytable, 1) give us row (Port) percentages
# col-1 (Survived=0, perished) and col-2 (Survived =1, survived)
DeathandsurviveRateforeachport <- prop.table(SurviveOverEmbarkedTable, 1)
#plot
barplot(Deathandsurvivepercentage[2:4,1]*100, xlab =(""), ylim=c(0,100), ylab="Death rate in percentage %",  names.arg = M,  col="red", main="Death rate comparison among mebarked ports", border="black", beside=TRUE)

#End ###########################################################

