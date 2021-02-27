setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata = read.csv('tecator.csv')
library(glmnet)

n = dim(mydata)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = mydata[id,]
test = mydata[-id,]

#Assignment 3 Task 1
#Fit linear regression to the training data and 
#estimate the training and test errors.

mean_squared_error = function(y, y_hat) {
  squared_error = (y-y_hat)^2
  sum_squared_error = sum(squared_error)
  n = length(squared_error)
  return(sum_squared_error/n)
}

fit = lm(formula = Fat ~., data = train[,-c(1, 103, 104)])

#Predictions
fitted_train = predict(fit, newdata = train)
fitted_test = predict(fit, newdata = test)

#MSE Train
MSE_train = mean_squared_error(train$Fat, fitted_train)

#MSE Test
MSE_test = mean_squared_error(test$Fat, fitted_test)

print(MSE_train)
print(MSE_test)

summary(fit)

#Assignment 3 Task 2,3
#Fit the Lasso regression model to the training data. 
#Present a plot illustrating how the regression coefficients 
#depend on the log of penalty factor (log 𝜆)
#What value of the penalty factor can be 
#chosen if we want to select a model with only three features?

covariates=scale(train[,2:101])
response=scale(train$Fat)

model_Lasso=glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
pdf('lasso.pdf')
plot(model_Lasso, xvar="lambda", label=TRUE, main="LASSO regression\n\n")
dev.off()

#Assignment 3 Task 4
#Present a plot of how degrees of freedom depend on the penalty parameter. 

pdf('degrees.pdf')
plot(log(model_Lasso$lambda), model_Lasso$df, xlab = "Log lambda (Penalty Factor)", ylab = "Degrees of freedom(df)")
dev.off()

#Assignment 3 Task 5
#Fit Ridge instead of the LASSO regression and compare the 
#plots from steps 3 and 5

model_Ridge=glmnet(as.matrix(covariates), response, alpha=0, family="gaussian")

pdf('ridge.pdf')
plot(model_Ridge, xvar="lambda", label=TRUE, main="Ridge regression\n\n")
dev.off()

#Assignment 3 Task 6
#Use cross-validation to compute the optimal LASSO model. Present a plot
#showing the dependence of the CV score on log 𝜆𝜆 and comment how the CV
#score changes with log 𝜆. 
#Finally, create a scatter plot of the
#original test versus predicted test values for the model corresponding to
#optimal lambda

covariates_cv=train[,2:101]
response_cv=train$Fat

model_cv_Lasso=cv.glmnet(as.matrix(covariates_cv), response_cv, alpha=1, family="gaussian")
lambda_best_cv = model_cv_Lasso$lambda.min

pdf('cv.pdf')
plot(model_cv_Lasso)
dev.off()

coef(model_cv_Lasso, s="lambda.min")

y = test[,102]
y_predict = predict(model_cv_Lasso, s = lambda_best_cv, newx = as.matrix(test[, 2:101]), type="response")

pdf('scattercv.pdf')
plot(y_predict, y, xlab = "Y-hat (predicted)", ylab = "Y")
dev.off()

#Assignment 3 task 7
#Use the feature values from test data (the portion of test data with Channel
#columns) and the optimal LASSO model from step 6 to generate new target
#values.
#Make a scatter plot of original Fat in test data
#versus newly generated ones.

std_dev = sd(y_predict - y)

pdf('newfat.pdf')
plot(rnorm(108, y_predict, std_dev), y, xlab = "New generated values", ylab = "Y")
dev.off()
