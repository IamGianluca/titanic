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

# CREATE TITLE VARIABLE AND USE THE AVERAGE AGE PER TITLE TO FILL THE AGE WHERE IS MISSING
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
                inner join av_ages as aa
                  on aa.Title = c.Title")

# go ahead using avAge on those observations where age is NA..
no.age <- which(is.na(combi$Age) == TRUE)
combi[no.age, "Age"] <- combi[no.age, "avAge"]


# get rid of avAge column
combi <- subset(combi, select = - avAge)


# sum up sibsp and parch variable to create family (size) variable 
combi <- transform(combi, 
                   Family = SibSp + Parch + 1)

# set Sex as factor
combi$Sex <- as.factor(combi$Sex)

# split training and test datasets
train <- combi[1:891, ]
test <- combi[892:1309, ]

# write new datasets in data folder
write.csv(train, file = "./data/tinytrain.csv", row.names = FALSE)
write.csv(test, file = "./data/tinytest.csv", row.names = FALSE)
