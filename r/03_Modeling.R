#Regression Models#

#splitting the data#

set.seed(12345678)

index <- createDataPartition(bank_clean$subscribed, p = 0.8, list = FALSE)

train <- bank_clean[index,]

test <- bank_clean[-index,]

#Hypotheses for Modeling#

#H1:previous campaign outcome if positive, increases the probability of subscription
#H2:having a current account, increases the probability of subscription
#H3:having a mortgage, decreases the probability of subscription
#H4:a higher interest rate, reduces the probability of subscription
#H5:a higher consumer confidence index, increases the probability of subscription


#baseline model#

formula1 <- subscribed ~ prev_campaign_outcome + has_current_account + has_mortgage + euribor_three_mth + cons_conf_index

model1 <- glm(formula = formula1, data= train, family = binomial)

summary(model1)

#models 2-3 adding different possible predictors in sets of 2 and then a set of 3 in the 4th model#

formula2 <- subscribed ~ prev_campaign_outcome + has_current_account + has_mortgage + euribor_three_mth + cons_conf_index + salary_level + has_personal_loan

model2 <- glm(formula = formula2, data= train, family = binomial)

summary(model2)

formula3 <- subscribed ~ prev_campaign_outcome + has_current_account + has_mortgage + euribor_three_mth + cons_conf_index + has_savings_account + has_credit_card

model3 <- glm(formula = formula3, data= train, family = binomial) 

summary(model3)

formula4 <- subscribed ~ prev_campaign_outcome + has_current_account + has_mortgage + euribor_three_mth + cons_conf_index + contact_method + month + day

model4 <- glm(formula = formula4, data= train, family = binomial)

summary(model4)

#final model containing all predictors (hypothesized + potential)#

formula5 <- subscribed ~ prev_campaign_outcome + has_current_account + has_mortgage + euribor_three_mth + cons_conf_index + salary_level + has_personal_loan +
  has_savings_account + has_credit_card + contact_method + month + day

model5 <- glm(formula = formula5, data=train, family = binomial)

summary(model5)

#assessment of Pseudo R square for each model#

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}
logisticPseudoR2s(model1)

logisticPseudoR2s(model2)

logisticPseudoR2s(model3)

logisticPseudoR2s(model4)

logisticPseudoR2s(model5)

#calculation of odds ratio for final model#

exp(model5$coefficients)

#this is used to interpret coefficients across all models#

#Assumption testing#

#Predicted Probabilities#

train$predictedProbabilities <- fitted(model5)

head(bank_clean(train$predictedProbabilities, train$subscribed))

#Analysing Residuals#

train$StandardisedResiduals <- rstandard(model5) 
train$StudentisedResiduals <- rstudent(model5)

sum(train$StandardisedResiduals > 1.96)

hist(train$StandardisedResiduals)

#Variance Inflation Factor#

vif(model1)

vif(model2)

vif(model3)

#no values of GVIF above 10#

#Prediction Building#

predictions1 <- predict(model1 ,test, type = "response")

predictions2 <- predict(model2 ,test, type = "response")

predictions3 <- predict(model3 ,test, type = "response")

predictions4 <- predict(model4 ,test, type = "response")

predictions5 <- predict(model5 ,test, type = "response")


#accuracy check for each predictive model#

class_pred1 <- as.factor(ifelse(predictions1 > .5, "no", "yes"))

postResample(class_pred1, test$subscribed)

class_pred2 <- as.factor(ifelse(predictions2 > .5, "no", "yes"))

postResample(class_pred2, test$subscribed)

class_pred3 <- as.factor(ifelse(predictions3 > .5, "no", "yes"))

postResample(class_pred3, test$subscribed)

class_pred4 <- as.factor(ifelse(predictions4 > .5, "no", "yes"))

postResample(class_pred4, test$subscribed)

class_pred5 <- as.factor(ifelse(predictions5 > .5, "no", "yes"))

postResample(class_pred5, test$subscribed)


#confusion matrices for each predictive model#

confusionMatrix(data=class_pred1, test$subscribed)

confusionMatrix(data=class_pred2, test$subscribed)

confusionMatrix(data=class_pred3, test$subscribed)

confusionMatrix(data=class_pred4, test$subscribed)

confusionMatrix(data=class_pred5, test$subscribed)

#Presentation of Model Results using Stargazer#

stargazer(model1, model2, model3, model4, model5, type="html", out="models.html")
