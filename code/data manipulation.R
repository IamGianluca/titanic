##### LOAD RELEVANT LIBRARIES #####

library(dplyr)
library(sqldf)


options(decimal = 2)


##### LOAD DATASET #####

# set you working directory and load datasets
setwd("/home/gianluca/Dropbox/Data Analysis/titanic/")

train <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors = FALSE)


##### DATA MANIPULATION #####

# extract the form of address from the Name column
train$Title <- gsub(".*, ([^.]*)\\..*", "\\1", train$Name)
test$Title <- gsub(".*, ([^.]*)\\..*", "\\1", test$Name)

# simplify Title schema
for (i in 1:nrow(train)) {
  if (train$Title[i] == "Mlle") {train$Title[i] <- "Miss"}
  else if (train$Title[i] == "Mme") {train$Title[i] <- "Mrs"}
  else if (train$Title[i] %in% c("Capt", "Don", "Major", "Sir", "Col", "Rev")) {train$Title[i] <- "Sir"}
  else if (train$Title[i] %in% c("Lady", "the Countess", "Jonkheer")) {train$Title[i] <- "Lady"}
  else if (train$Title[i] == "Mlle") {train$Title[i] <- "Miss"}
}

# summarise Title and average age
av_ages <- train %.%
  group_by(Title) %.%
  summarise(avAge = round(mean(Age, na.rm = TRUE), digits = 0))

# add avAge column to train dataset
train <- sqldf("select PassengerId, Survived, Pclass, Name, Sex, Age, SibSp, Parch, Ticket,
                Fare, Cabin, Embarked, t.Title, avAge 
                from train as t
                inner join av_ages as aa
                  on aa.Title = t.Title")

train %.%
  select(PassengerId, Title, Age, avAge) %.%
  filter(is.na(Age) == TRUE)

# go ahead using avAge on those observations where age is NA..