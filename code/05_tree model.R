rm(list = ls())


##### LOAD RELEVANT LIBRARIES #####

library(dplyr)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(fancyRpartPlot)

options(decimal = 2)


##### LOAD DATASET #####

setwd("~/Dropbox/Data Analysis/kaggle/titanic")
load("./data/tiny dataset.Rdata")


##### TREE MODEL #####

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + factor(Embarked) + 
               factor(Title) + Family, data = train, method = "class")

fancyRpartPlot(fit)
printcp(fit) # display cross-validation results
plotcp(fit) # visualize cross-validation results

# assess prediction on training dataset
predictions <- predict(fit, train, type = "class")
table(train$Survived, predictions)


##### PRUNED TREE #####

# prune back the tree to avoid overfitting the data. Typically, we will want to select a
# tree size that minimizes the cross-validated error, the xerror column printed by printcp()
# As we can see from cross-validation results plot models of size from 2 to 9 lead to similar
# cross-validated error (which is a good estimate of the test error). We like to use the 
# one-standard-error rule, so choose a model of size 2. Unfortunately this lead to a slightly
# worst score in the leaderboard
# new.fit <- prune(fit, cp = fit$cptable[2])
# fancyRpartPlot(new.fit)

# alternatively I could use the size which minimise the cross-validated error. This isn't
# perhaps the best prectice because that could lead to worst results when using the model on
# new data.
# Practice can give you a better idea if the one-standard-error rule make sense if applied 
# along with cross-validation
new.fit <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]) 

predictions2 <- predict(new.fit, newdata = test, method = "class")

test$Survived <- NA
for (i in 1:nrow(test)) {
  if (predictions2[i, 1] > 0.5) {test$Survived[i] <- 0}
  else test$Survived[i] <- 1
}


##### SUBMIT RESULTS TO KAGGLE #####

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "prunedtree_submission.csv", row.names = FALSE)
