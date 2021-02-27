setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(iris)
summary(iris)
my_data <- iris

## Task 1 - Scatterplot Sepal

#Plotting the Sepal Length variable and the Sepal Width variable for the 3 species (classes) 
#Setosa, Versicolor and Virginica. There are in total 150 datapoints with 50 for each 
#class in the original data. Setosa separate from the rest in the graph which means that 
#it would be quite easy to classify with LDA. Versicolor and Virginica mixed -> more equal 
#mean and covariance -> more difficult to predict correctly with LDA.
plot(iris$Sepal.Width, iris$Sepal.Length, pch = c(1,2,3)[iris$Species], 
     col=c("brown1","dodgerblue1","limegreen")[iris$Species], main = "Sepal Scatter", 
     xlab = "Sepal Width", ylab = "Sepal Length", xlim = c(1.8, 5), ylim = c(4, 8.5))
legend('topright', legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("brown1","dodgerblue1","limegreen"))

## Task 2 - Linear Discriminant Analysis

## Task 2a
#Computing the mean vector, the covariance matrix and prior probabilities for each class.
#The prior probabilities are the same for each class since there are 50 datapoints for 
#each class in the original data. 

#Setosa
x1 <- iris$Sepal.Length[iris$Species == "setosa"]
x2 <- iris$Sepal.Width[iris$Species == "setosa"]
x_setosa = cbind(x1, x2)
mu_x1 = (sum(x1))/50
mu_x2 = (sum(x2))/50

#mean vector
mu_set = rbind(mu_x1, mu_x2)
cov(x_setosa)
setosa_prior = 50/150

#Versicolor
x3 <- iris$Sepal.Length[iris$Species == "versicolor"]
x4 <- iris$Sepal.Width[iris$Species == "versicolor"]
x_versicolor = cbind(x3, x4)
mu_x3 = (sum(x3))/50
mu_x4 = (sum(x4))/50
mu_ver = rbind(mu_x3, mu_x4)
cov(x_versicolor)
versicolor_prior = 50/150

#Virginica
x5 = iris$Sepal.Length[iris$Species == "virginica"]
x6 = iris$Sepal.Width[iris$Species == "virginica"]
x_virginica = cbind(x5, x6)
mu_x5 = (sum(x5))/50
mu_x6 = (sum(x6))/50
mu_vir = rbind(mu_x5, mu_x6)
cov(x_virginica)
virginica_prior = 50/150

## Task 2b
#Since LDA assumes that every class has the same covariance matrix, we compute the
#pooled covariance matrix. As we saw in Task 2a the classes did not have equal covariance
#matrices which is the assumption for LDA, which is why we need a pooled matrix. 
Pooled_covariance = (cov(x_setosa)*50 + cov(x_versicolor)*50 + 
                       cov(x_virginica)*50)/150

## Task 2d
#Here we calculate the discriminant functions for each class, which we can use to 
#predict the class for each datapoint. The function is taken from the lecture. 
#For each datapoint, the class with the highest discriminant value is the class we 
#predict the datapoint to belong to, this is shown in Task 3. 
x = cbind(iris$Sepal.Length, iris$Sepal.Width)

#Setosa
Dk_set_x = x %*% solve(Pooled_covariance) %*% mu_set - ((t(mu_set) %*% solve(Pooled_covariance) %*% mu_set) / 2)[1, 1] + log(setosa_prior)

#Versicolor
Dk_ver_x = x %*% solve(Pooled_covariance) %*% mu_ver - ((t(mu_ver) %*% solve(Pooled_covariance) %*% mu_ver) / 2)[1, 1] + log(versicolor_prior)

#Virginica
Dk_vir_x = x %*% solve(Pooled_covariance) %*% mu_vir - ((t(mu_vir) %*% solve(Pooled_covariance) %*% mu_vir) / 2)[1, 1] + log(virginica_prior)

## Task 2e
#We get the decision boundaries between the classes by setting the discriminant functions
#for each class equal to each other. If any datapoint fulfills this, it should be 
#exactly on "the predicted line" between the two classes -> you would not really know which
#class the datapoint would belong to (i think).
 
#Between setosa and versicolor, Dk_set_x = Dk_ver_x
x %*% solve(Pooled_covariance) %*% mu_set - ((t(mu_set) %*% solve(Pooled_covariance) %*% mu_set) / 2)[1, 1] + log(setosa_prior) = 
  x %*% solve(Pooled_covariance) %*% mu_ver - ((t(mu_ver) %*% solve(Pooled_covariance) %*% mu_ver) / 2)[1, 1] + log(versicolor_prior)

#Between setosa and virginica, Dk_set_x = Dk_vir_x
x %*% solve(Pooled_covariance) %*% mu_set - ((t(mu_set) %*% solve(Pooled_covariance) %*% mu_set) / 2)[1, 1] + log(setosa_prior) = 
  x %*% solve(Pooled_covariance) %*% mu_vir - ((t(mu_vir) %*% solve(Pooled_covariance) %*% mu_vir) / 2)[1, 1] + log(virginica_prior)

#Between versicolor and virginica, Dk_ver_x = Dk_vir_x
x %*% solve(Pooled_covariance) %*% mu_ver - ((t(mu_ver) %*% solve(Pooled_covariance) %*% mu_ver) / 2)[1, 1] + log(versicolor_prior) = 
  x %*% solve(Pooled_covariance) %*% mu_vir - ((t(mu_vir) %*% solve(Pooled_covariance) %*% mu_vir) / 2)[1, 1] + log(virginica_prior)

## Task 3 - Prediction with Discriminant function
#Here we use the discriminant values to predict the class for each datapoint. As
#stated in task 2d the class with the highest discriminant value for a datapoint, 
#is the class we predict the datapoint to belong to. 

#Loop over the 150 datapoints to see which class they belong to. It says if the setosa
#discriminant value is higher than the versicolor and virginica discriminant values for
#a specific datapoint, we set the class for that datapoint to setosa. The same for 
#versicolor and virginica.
for(i in 1:150) {
  if (Dk_set_x[i, 1] > Dk_ver_x[i, 1] && Dk_set_x[i, 1] > Dk_vir_x[i, 1]) {
    my_data[i, ]$Species <- "setosa"
  } else if (Dk_ver_x[i, 1] > Dk_set_x[i, 1] && Dk_ver_x[i, 1] > Dk_vir_x[i, 1]) {
    my_data[i, ]$Species <- "versicolor"
  } else if (Dk_vir_x[i, 1] > Dk_set_x[i, 1] && Dk_vir_x[i, 1] > Dk_ver_x[i, 1]) {
    my_data[i, ]$Species <- "virginica"
  }
}

#Plotting the Sepal Length variable and Sepal Width variable where we predicted 
#the classes for each point. Can see how versicolor and virginica has split up.
plot(my_data$Sepal.Width, my_data$Sepal.Length, pch = c(1,2,3)[my_data$Species], 
     col=c("brown1","dodgerblue1","limegreen")[my_data$Species], main = "Discriminant Plot", 
     xlab = "Sepal Width", ylab = "Sepal Length", xlim = c(1.8, 5), ylim = c(4, 8.5))
legend('topright', legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("brown1","dodgerblue1","limegreen"))

#Confusion matrix between predicted and original values.
table(iris$Species, my_data$Species, dnn = c('Actual class', 'Predicted class'))

#Misclassification rate of prediction and LDA. 
n_errors = 1+15+14
error_rate = n_errors/150

#Here we instead use the built in LDA function in R. We predict the Species with 
#Sepal Length and Sepal Width. 
iris_lda = lda(Species ~ Sepal.Length + Sepal.Width, data = iris)

lda_pred = predict(iris_lda)$class

#Confusion matrix. The same one as in our calculated LDA.
table(iris$Species, lda_pred, dnn = c('Actual class', 'Predicted class'))

#Misclassification. The same one as in our calculated LDA. Gives our calculations
#some legitimacy. This is also expected since both methods are LDA. 
lda_n_errors = 1+15+14
lda_error_rate = lda_n_errors/150

## Task 4 - Generate with Probabilistic Model¨
my_new_data = iris

#Sample species(classes). Have no idea why we use this since this create 50 of each 
#class which would yield the same result as the original species vector (i think).
test = sample(unique(iris$Species), 150, replace = TRUE)

#Generate new values. Here we use the mean vector for each class and the pooled
#covariance to generate new values. We loop through 150 times and say that
#if the class in our sampled vector test is "setosa" we generate new values for 
#Sepal Length and Sepal Width by using the mean vector for setosa and the 
#pooled covariance. The same for versicolor and virginica. 
for(i in 1:150) {
  if (test[i] == "setosa") {
    generate = rmvnorm(1, mean = mu_set, sigma = Pooled_covariance)
    my_new_data[i, ]$Sepal.Length <- generate[1,1]
    my_new_data[i, ]$Sepal.Width <- generate[1,2]
    my_new_data[i, ]$Species <- "setosa"
  } else if (test[i] == "versicolor") {
    generate1 = rmvnorm(1, mean = mu_ver, sigma = Pooled_covariance)
    my_new_data[i, ]$Sepal.Length <- generate1[1,1]
    my_new_data[i, ]$Sepal.Width <- generate1[1,2]
    my_new_data[i, ]$Species <- "versicolor"
  } else if (test[i] == "virginica") {
    generate2 = rmvnorm(1, mean = mu_vir, sigma = Pooled_covariance)
    my_new_data[i, ]$Sepal.Length <- generate2[1,1]
    my_new_data[i, ]$Sepal.Width <- generate2[1,2]
    my_new_data[i, ]$Species <- "virginica"
  }
}

#Plotting the new generated Sepal Length variable and the Sepal Width variable for each 
#class. They are a bit different since we used the pooled covariance instead of the 
#class´ own covariance matrix. We also see that the Versicolor and Virginica are mixed
#which is not surprising since we used the original data mean. 
plot(my_new_data$Sepal.Width, my_new_data$Sepal.Length, pch = c(1,2,3)[my_new_data$Species], 
     col=c("brown1","dodgerblue1","limegreen")[my_new_data$Species], main = "Generated Plot", 
     xlab = "Sepal Width", ylab = "Sepal Length", xlim = c(1.8, 5.5), ylim = c(4, 8.5))
legend('topright', legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("brown1","dodgerblue1","limegreen"))

## Task 5 - Logistic regression
#Classifying the species with logistic regression instead of LDA. Do not really have
#a good explanation for this method. 
iris_multinom = multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)

multinom_pred = predict(iris_multinom, type = "class")

for(i in 1:150) {
  my_data[i, ]$Species = multinom_pred[i]
}

#Plotting the Sepal Length variable and Sepal Width variable where we predicted 
#the classes for each point. Can see how the logistic regression handles outliers
#such as the most left setosa better than the LDA since it classifies that correctly.
plot(my_data$Sepal.Width, my_data$Sepal.Length, pch = c(1,2,3)[my_data$Species], 
     col=c("brown1","dodgerblue1","limegreen")[my_data$Species], main = "Log Regression Plot", 
     xlab = "Sepal Width", ylab = "Sepal Length", xlim = c(1.8, 5), ylim = c(4, 8.5))
legend('topright', legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("brown1","dodgerblue1","limegreen"))

#Confusion matrix
table(iris$Species, multinom_pred, dnn = c('Actual class', 'Predicted class'))


#Misclassification error. Better than the LDA which could reflect the LDA:s inability 
#to handle outliers as well as the fact that the classes in the LDA do not have the same 
#covariance matrix, where we instead have to pool them together. This is not an assumption 
#in Logistic regression. 
multinom_n_errors = 12+13
multinom_error_rate = multinom_n_errors/150


