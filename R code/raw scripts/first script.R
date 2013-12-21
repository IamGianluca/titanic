#set working directory
setwd("~/Dropbox//Data Analysis/titanic/")

# load datasets
gender <- read.csv("./data//raw data/gendermodel.csv", stringsAsFactors=F)
train.set <-read.csv("./data//raw data/train.csv", stringsAsFactors=F)

# set some variables as factors..

# exploratory analysis
head(gender)
head(train.set)
str(train.set)
summary(train.set)
