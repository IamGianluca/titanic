##### LOAD RELEVANT LIBRARIES #####

library(dplyr)
library(ggplot2)


options(decimal = 2)


##### LOAD DATASET #####

setwd("/home/gianluca/Dropbox/Data Analysis/titanic/")

train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")


##### DATA MANIPULATION #####

# create Survived column in test dataset and merge the two dataset
test$Survived <- NA
combi <- rbind(train, test)

# trasform Name column as character to make text manipulation possible
combi$Name <- as.character(combi$Name)

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
combi %.%
  group_by(Title) %.%
  summarise(Age = mean(Age, na.rm = TRUE))

# 