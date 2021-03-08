
#########################################
# Data preprocess
#########################################
# assume we had imported both train and test dataset and we have combined them into on data
# train <- read.csv("train.csv", header = TRUE)
# test <- read.csv("test.csv", header = TRUE)
# # integrate into one file for checking save to do the dame for both files.
#data <- bind_rows(train, test) # compare with data <- rbind(train, test)
#
# If we save the file from the previous code we can load it directly

data <- read.csv("data.csv", header = TRUE)

# Check our combined dataset details
glimpse(data) # compare with str(data)

# Define a function to check missing values
missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i])|x[, i] =="" )
    missing_prop[i] <- missing[i] / nrow(x)
  }

  (missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>%
      arrange(desc(missing_prop)))
}
#############################################
##### Dealing with missing values
#############################################
# large number of missing values, itself can be meaningful.
# add newly created attribute and assign it with new values
data$HasCabinNum <- ifelse((data$Cabin != ""), "Has", "HasNo")

#examine the relations between our newly created cabin replacement's
# 'HasCabinNum' with the attribute Survival using plot

# The forst plot is the numbers
# Make sure survived is in factor type
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(HasCabinNum), fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("HasCabinNum") +
  ylab("Total Count") +
  labs(fill = "Survived")+
  ggtitle("Newly created HasCabinNum attribute on Survived")

# The 2nd plot shows survive percentage of HasCabinNum
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(HasCabinNum), fill = factor(Survived))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "HasCabinNum", y = "Percentage of Survived") +
  ggtitle("Newly created HasCabinNum attribute (Proportion Survived)")

##### Dealing with missing values in Cabin #####

### 1. Replace missing value in Age with its average
ageEverage <- summarise(data, Average = mean(Age, na.rm = TRUE))
# create a new attribute Age_RE1 and assign it with new values
data$Age_RE1 <- ifelse(is.na(data$Age), as.numeric(ageEverage), as.numeric(data$Age))

# plot newly altered age attribute
# Make sure survived is in factor type
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE1), fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Age_RE1") +
  ylab("Total Count") +
  labs(fill = "Survived")+
  ggtitle("Survived value on Age_RE1")

# show survive percentage on HasCabinNum
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE1), fill = factor(Survived))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Age_RE1", y = "Percentage of Survived") +
  ggtitle("Survived percentage on Age_RE1")

### 2. Take a random number range between `min` and `max` age,
# and keep the mean and standard deviation unchanged.
# calculate the non-NA mean and std
mean <- mean(data[["Age"]], na.rm = TRUE) # take  mean
std <- sd(data[["Age"]], na.rm = TRUE) # take  std
# replace NA with a list that maintian the mean and std
temp_rnum <- rnorm(sum(is.na(data$Age)), mean=mean, sd=std)
# add new attribute Age_RE2
data$Age_RE2 <- ifelse(is.na(data$Age), as.numeric(temp_rnum), as.numeric(data$Age))
summary(data$Age_RE2)
# There are possible negative values too, replace them with positive values
data$Age_RE2[(data$Age_RE2)<=0] <- sample(data$Age[data$Age>0], length(data$Age_RE2[(data$Age_RE2)<=0]), replace=F)

# check result
summary(data$Age_RE2)

# plot newly altered age attribute
# Make sure survived is in factor type
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE2), fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Age_RE2") +
  ylab("Total Count") +
  labs(fill = "Survived")+
  ggtitle("Survived value on Age_RE2 attribute")

# show survive percentage on HasCabinNum
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE2), fill = factor(Survived))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Age_RE2", y = "Percentage of Survived") +
  ggtitle("Survived percentage on Age_RE2 attribute")

###Using machine generate model
# to produce new values based on other exiting values
# confirm Age missing values
# get the origianl so it can be compared later
data$Age_RE3 <- data$Age
summary(data$Age_RE3)
# Construct a decision tree with selected attributes and ANOVA method
Agefit <- rpart(Age_RE3 ~ Survived + Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=data[!is.na(data$Age_RE3),],
                method="anova")
#Fill AGE missing values with prediction made by decision tree prediction
data$Age_RE3[is.na(data$Age_RE3)] <- predict(Agefit, data[is.na(data$Age_RE3),])
#confirm the missing values have been filled
summary(data$Age_RE3)

p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE3), fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Age_RE3") +
  ylab("Total Count") +
  labs(fill = "Survived")+
  ggtitle("Survived value on Age_RE3 attribute")

# show survive percentage on HasCabinNum
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Age_RE3), fill = factor(Survived))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Age_RE3", y = "Percentage of Survived") +
  ggtitle("Survived percentage on Age_RE3 attribute")

# Choose use one results as Age, we use machine generated
data$Age <- data$Age_RE3
data <- subset(data, select = -c(Age_RE1, Age_RE2, Age_RE3))

### Deal with Fare Attribute
#
# just one missing value so replace it with mean or median value
data[is.na(data$Fare), ]
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm = T)

### Embarked Attribute
# we take two steps:
# 1. find out the passenger has a shared ticket or not.
# If the ticket is shared than find the travel companion's
# embarked port and take that as the passenger's embarked port;
# 2. If the ticket is not shared or shared partner's
# embarked port is also missing, find out the ticket price
# per person and compare with other ticket's price per
# person to allocate the embarked port.

# list info of the missing records to figure out the fare and the ticket?
data[(data$Embarked==""), c("Embarked", "PassengerId",  "Fare", "Ticket")]
# we want find out if the fare is a single ticket or a group ticket.
# we need to find out is there other passenger share the ticket?
data[(data$Ticket=="113572"), c("Ticket", "PassengerId", "Embarked", "Fare")]

# calculate fare_PP per person
fare_pp <- data %>%
  group_by(Ticket, Fare) %>%
  dplyr::summarize(Friend_size = n()) %>%
  mutate(Fare_pp = Fare / Friend_size)
data <- left_join(data, fare_pp, by = c("Ticket", "Fare"))

# Plot Fare per person on Embarked port
data %>%
  filter((Embarked != "")) %>%
  ggplot(aes(x = Embarked, y = Fare_pp)) +
  geom_boxplot() +
  geom_hline(yintercept = 40, col = "deepskyblue4")

#assign `C` to the embarked missing value.
data$Embarked[(data$Embarked)==""] <- "C"

#confirm the missing values have been fulfilled.
missing_vars(data)

#######################################
##### Attribute Re-engineering #####
#######################################
#
### Title from Name attribute
#
# Abstract Title out
data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)
data %>%
  group_by(Title) %>%
  dplyr::count() %>%
  arrange(desc(n))

# group those less common title’s into an ‘Other’ category.
data$Title <- ifelse(data$Title %in% c("Mr", "Miss", "Mrs", "Master"), data$Title, "Other")
# Checking the table of *Title* vs *Sex* shows nothing anomalous
L<- table(data$Title, data$Sex)
knitr::kable(L, digits = 2, booktabs = TRUE, caption = "Title and sex confirmation")

#plot the results
data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Title), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Title", y = "Survival Percentage") +
  ggtitle("Title attribute (Proportion Survived)")

### Deck from Cabin attribute
data$Cabin <- as.character(data$Cabin)
data$Deck <- ifelse((data$Cabin == ""), "U", substr(data$Cabin, 1, 1))
# plot our newly created attribute relation with Survive
p1 <- ggplot(data[1:891,], aes(x = Deck, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  labs(x = "Deck number", y = "Total account") +
  labs(fill = "Survived")

# plot percentage of survive
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Deck), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Deck number", y = "Percentage") +
  ggtitle("Newly created Deck number (Proportion Survived)")

### Extract ticket class from ticket number

data$Ticket <- as.character(data$Ticket)
data$Ticket_class <- ifelse((data$Ticket != " "), substr(data$Ticket, 1, 1), "")
data$Ticket_class <- as.factor(data$Ticket_class)

# plot our newly created attribute relation with Survive
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Ticket_class, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  labs(x = "Ticket_class", y = "Total account") +
  labs(fill = "Survived value over Ticket class")

# plot percentage of survive
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Ticket_class), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Ticket_class", y = "Percentage") +
  ggtitle("Survived percentage over Newly created Ticket_class")

### Travel in Groups
data$Family_size <- data$SibSp + data$Parch + 1
data$Group_size <- pmax(data$Family_size, data$Friend_size)

#plot our newly created attribute's prediction power
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Group_size, fill = factor(Survived))) +
  geom_histogram() +
  scale_y_continuous(breaks = seq(0, 700, 100)) +
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Group Size: max(Family Size, Group Size)", y = "Count") +
  ggtitle("Survived count over groupsize")

# plot percentage of survive
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Group_size, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Group_size", y = "Percentage") +
  ggtitle("Survived percentage over Newly created Group_size")

### Age in Groups
# set bins
Age_labels <- c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79')
#assign labels
data$Age_group <- cut(data$Age, c(0, 10, 20, 30, 40, 50, 60, 70, 80), include.highest=TRUE, labels= Age_labels)

#Plot
p1 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Age_group, y = ..count.., fill = factor(Survived))) +
  geom_bar() +
  ggtitle("Survived value ove newly created Age_group")

# plot percentage of survive
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Age_group, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Age group", y = "Percentage") +
  ggtitle("Survived percentage ove newly created Age_group")

### Fare per passenger
data$Fare_pp <- data$Fare/data$Friend_size

# plot Fare_PP against Survived
p1<- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = Fare_pp, fill = factor(Survived))) +
  geom_histogram(binwidth = 2) +
  scale_y_continuous(breaks = seq(0, 500, 50)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Fare (per person)", y = "Count") +
  ggtitle("Survived value over Fare_pp")
p1
# plot percentage of survive
p2 <- data %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = factor(Fare_pp), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Fare per person", y = "Percentage") +
  ggtitle("Survived rate over newly created Fare_PP")
p2

# plot in box plot
data %>%
  filter(!is.na(Survived)) %>%
  filter(Fare > 0) %>%
  ggplot(aes(factor(Survived), Fare_pp)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) +
  geom_jitter()

## Build Re-engineered Dataset
glimpse(data)
# Remove abundant attributes
RE_data <- subset(data, select = -c(Name, Cabin, Fare))
# write in file
write.csv(RE_data, file = "RE_Data.CSV", row.names = FALSE)
####END 2 ###########################################################




