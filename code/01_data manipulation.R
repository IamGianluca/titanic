rm(list = ls())

##### LOAD RELEVANT LIBRARIES #####

library(dplyr)
library(sqldf)

options(decimal = 2)


##### LOAD DATASET #####

# set you working directory and load datasets
setwd("~/Dropbox/Data Analysis/kaggle/titanic")

train <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors = FALSE)


##### DATA MANIPULATION #####

# create Title variable and use the average age by title to fill the missing ages
# merge training and test datasets to extract title from name
test$Survived <- rep(NA, length(test))
combi <- rbind(train, test)

# extract the form of address from the Name column
combi$Title <- gsub(".*, ([^.]*)\\..*", "\\1", combi$Name)

# simplify Title schema
for (i in 1:nrow(combi)) {
  if (combi$Title[i] == "Mlle") {combi$Title[i] <- "Miss"}
  else if (combi$Title[i] == "Mme") {combi$Title[i] <- "Mrs"}
  else if (combi$Title[i] %in% c("Capt", "Don", "Major", "Sir", "Col", "Rev")) {combi$Title[i] <- "Sir"}
  else if (combi$Title[i] %in% c("Dona", "Lady", "the Countess", "Jonkheer")) {combi$Title[i] <- "Lady"}
  else if (combi$Title[i] == "Mlle") {combi$Title[i] <- "Miss"}
}

# summarise Title and average age
av_ages <- combi %.%
  group_by(Title) %.%
  summarise(avAge = round(mean(Age, na.rm = TRUE), digits = 0))

# add avAge column to train dataset
combi <- sqldf("select PassengerId, Survived, Pclass, Name, Sex, Age, SibSp, Parch, Ticket,
                  Fare, Cabin, Embarked, c.Title, avAge 
                from combi as c
                inner join av_ages as aa on aa.Title = c.Title")

# go ahead using avAge on those observations where age is NA..
no.age <- which(is.na(combi$Age) == TRUE)
combi[no.age, "Age"] <- combi[no.age, "avAge"]

# get rid of avAge column
combi <- subset(combi, select = - avAge)

# split in three bins variable Age and assess the different likelihood to survive the event 
combi$AgeType <- rep("Elderly", nrow(combi))
for (i in 1:nrow(combi)) {
  if (combi$Age[i] <= 18) {combi$AgeType[i] <- "Young"}
  else if (combi$Age[i] <= 30) {combi$AgeType[i] <- "Adult"}
  else if (combi$Age[i] <= 50) {combi$AgeType[i] <- "Middle Age"}
}

combi$AgeType <- rep("50-99", nrow(combi))
for (i in 1:nrow(combi)) {
  if (combi$Age[i] <= 5) {combi$AgeType[i] <- "00-05"}
  else if (combi$Age[i] <= 10) {combi$AgeType[i] <- "05-10"}
  else if (combi$Age[i] <= 15) {combi$AgeType[i] <- "10-15"}
  else if (combi$Age[i] <= 20) {combi$AgeType[i] <- "15-20"}
  else if (combi$Age[i] <= 25) {combi$AgeType[i] <- "20-25"}
  else if (combi$Age[i] <= 30) {combi$AgeType[i] <- "25-30"}
  else if (combi$Age[i] <= 35) {combi$AgeType[i] <- "30-35"}
  else if (combi$Age[i] <= 40) {combi$AgeType[i] <- "35-40"}
  else if (combi$Age[i] <= 45) {combi$AgeType[i] <- "40-45"}
  else if (combi$Age[i] <= 50) {combi$AgeType[i] <- "45-50"}
}

# check likelihood to survived by Age group
combi[which(is.na(combi$Survived) == FALSE),] %.%
  group_by(AgeType) %.%
  select(AgeType, Survived) %.%
  summarise(Num = n(),
            SurvRatio = sum(Survived) / n())

# create variable family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# now, it will more useful to reduce the variance transform the variable as a factor. To find 
# the best number of bins to adopt I can have a look at the result of this command
combi[which(is.na(combi$Survived) == FALSE),] %.%
  group_by(FamilySize) %.%
  select(FamilySize, Survived) %.%
  summarise(Size = n(),
            SurvRatio = sum(Survived) / n())

combi$FamSize <- rep(NA, nrow(combi))
for(i in 1:nrow(combi)) {
  if (combi$FamilySize[i] == 1) {combi$FamSize[i] <- "1"}
  else if (combi$FamilySize[i] > 1 & combi$FamilySize[i] < 5) {combi$FamSize[i] <- "2-4"}
  else {combi$FamSize[i] <- "5+"}
}

# latest refinements
combi <- transform(combi,
                   Survived = factor(Survived, labels = c("No", "Yes")),   
                   Pclass = factor(Pclass, labels = c("1st", "2nd", "3rd")),
                   Sex = factor(Sex, labels = c("Female", "Male")),
                   Family = factor(FamSize, ordered = TRUE))

# get rid of avAge column
combi <- combi[, - c(7, 8, 9, 11, 14, 15, 16)]

# split training and test datasets
train <- combi[1:891, ]
test <- combi[892:1309, ]

# write new datasets in data folder
save(train, test, file = "./data/tiny dataset.Rdata")
