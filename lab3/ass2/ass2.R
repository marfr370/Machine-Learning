# Lab 3 block 1 of 732A99/TDDE01 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)

index <- sample(1:4601)
tr <- spam[index[1:3000], ]
va <- spam[index[3001:3800], ]
trva <- spam[index[1:3800], ]
te <- spam[index[3801:4601], ]

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3 ? Why ?

#The fourth filter, filter 3, is the one with the lowest misclassification error. The
#prediction here is made on the test data but since we fit the model on the train, validation
#and test data combined we cannot use this. We found out optimal value of C by checking the 
#validation error on the fitted training data. When predicting on the test data and checking
#the error, we have to do first train and fit the model on only the training and validation
#data.  

#The err0 is not chosen since that is using the validation data as a predictor. By comparing
#the err_va and err0 we see that they are the same since we perform the exact same 
#fitting and predicting data sets. 

#The third filter, filter 1, is predicting using the test data which is good. But the fitted 
#filter is only fitting the model using the training data. Therefore
#this filter is not an accurate depiction since we want to include all data that is 
#not test data in the fitting.

#We therefore choose filter2 to return to the user, since it is trained and fitted using both
#training and validation data and is predicting on the test data. We then get the 
#correct model based on the data. 

# 2. What is the estimate of the generalization error of the filter returned ? err0, err1, err2 or err3 ? Why ?

#The estimate of the generalization error of the filter returned is err2, since we use the 
#test data to make the prediction on the model, i.e. the unseen data. This prediction 
#is the so called out-of-sample prediction. By doing this and then evaluating the error 
#we get the so called out-of-sample error, which is also called the generalization error. 
#In this case the generalization error for filter2 is err2 which is equal to 0.08364544.