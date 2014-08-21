rm(list = ls())

##### LOAD RELEVANT LIBRARIES AND OPTIONS #####

library(ggplot2)
library(plyr)

options(digits=2)


##### LOAD AND CLEANING DATASET #####

setwd("~/Dropbox/Data Analysis/kaggle/titanic")
load("./data/tiny dataset.Rdata")


##### LOGISTIC REGRESSION #####

# full model
train.fit <- glm(Survived ~ . - PassengerId - Name, family = binomial, data = train)
summary(train.fit)

# simplified model
train.fit.simpl <- glm(Survived ~ Pclass + Sex + Age + Family,
                 data = train, family = binomial)
summary(train.fit.simpl)

# because the two models are nested, we can use the anova() function to compare them.
# For generalized linear models we'll want a chi-square version of this test. The 
# non-significant chi-square value (p-value = 0.12) suggests that the reduced model with 
# four predictors fits as well as the full model with eight predictors
anova(train.fit, train.fit.simpl, test = "Chisq")

# cross-validation
predict(train.fit.simpl, type = "response")

# predictions on test set
predictions <- predict(train.fit.simpl, newdata = test, type = "response")

results <- as.vector(NA)

# randomly predict those observations where I don't have enough data (not really the best
# way to run a prediction but I was in a rush to submit something on Kaggle). I must create
# an substitute value for those observations coded as NAs to get around this issue.
for (i in 1:length(predictions)) {
  results[i] <- ifelse(predictions[i] >= 0.5, 1, 0)
  if(is.na(predictions[i]) == TRUE) results[i] <- rbinom(n = 1, size = 1, prob = 0.5)
}


##### SUBMIT RESULTS TO KAGGLE #####

kaggle.sub <- cbind(test$PassengerId, results)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "logistic regression_submission.csv", row.names = FALSE)
