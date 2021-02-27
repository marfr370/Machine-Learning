
library(boot)
library(ggplot2)
setwd("~/Desktop/TDDE01/tdde01/lab2/ass3")
origin_data = read.csv("communities.csv")

#Scaling all the data except for ViolentCrimesPerPop
data_without_crimes = origin_data[, 1:100]
data_without_crimes= scale(data_without_crimes)
data_without_crimes = as.matrix(data_without_crimes)
n=dim(data_without_crimes)
# Calculate covariance matrix, S is a 100x100 matrix with the covariances
S = 1/n * t(data_without_crimes)%*%data_without_crimes

# Calculate eigenvalues from the covariance matrix  
# Eigenvectors of the Covariance matrix are actually the directions of the axes where there is the most variance(most information) and that we call Principal Components.
# Eigenvalues of the Covariance matrix contains the amount of variance 
# Eigen() func on covariance matrix with only values=FALSE returns both eigenvectors and eigenvalues. The eigenvalues are sorted in descending order
eigen_values_vectors = eigen(S, only.values = FALSE)
eigenvalues = eigen_values_vectors$values

# variance covered by principal components as Strings and as numbers. 
# There are as many Principal components as variables in the original data
# All principal components are perpendicular to each other
# Calculating how much variance in % (therefore multiplying by 100 in denominator) that an eigenvalue covers of the total variation in the data. 
variance = sprintf("%2.3f",eigenvalues/sum(eigenvalues)*100) 
variance1 = eigenvalues/sum(eigenvalues)*100

#Calculate how many principal components is needed to cover 95% of variance
var = sum(variance1[1:34]) #Dosen't quite reach 95%
var_correct = sum(variance1[1:35]) #Covers more that 95%
var_2 = sum(variance1[1:2]) #How much variance PC1 and PC2 accounts for in the original data
var_PC1=variance1[1]
var_PC2=variance1[2]


#----------------------------
#TASK 2 ----------------------------
#----------------------------
  
#plot the variance covered by principal components 
#princomp function does the PCA on our data
#Loadings describe how much each variable contributes to a particular principal component.
res=princomp(data_without_crimes)
screeplot(res)
U= res$loadings

# Getting the eigenvector i.e the loadings for PC1. The loadings define the (cosine of angles) i.e. how the vector is positioned in the n-dimensional space
U1 = U[,1]
U1_vec = as.vector(U1)
plot(U1, main="Traceplot, PC1")

#Get the 5 varaibles that covers the largest variation in the target variable by absolute value
tail(sort(abs(U1)),5)


# The ViolentCrimesPerPop is projected onto the 2 dimensional PC1-PC2 plane. 
x = data.frame(pc1=res$scores[,1], pc2=res$scores[,2], crimes=origin_data$ViolentCrimesPerPop)
ggplot(x, aes(x=pc1, y=pc2, color=crimes)) + xlab("PC1") + ylab("PC2") +
  geom_point() + labs(color="Violent Crimes Per Pop") 



##################
##### TASK 3 #########
##################

polynomial <- lm(crimes ~ poly(pc1, 2), data)  #Doing second degree polynomial regression
#Fitting a line to the points

summary(polynomial)

plot(data$pc1, data$crimes)   #plotting the data points, e.g what value the crime level has for different scores of PC1. 
mycol <- rgb(0, 120, 185, max = 255, alpha = 30, names = "blue50")
points(x=data$pc1,    #Plotting the fitted line to the data points
       y=fitted(polynomial),
       pch=16,
       col=mycol)
dev.off()

data = data.frame(pc1=res$scores[,1], crimes=origin_data$ViolentCrimesPerPop)

########################
###   BOOTSTRAP TASK 4
### resampling a known distribution function, whose parameters are estimated from our sample
########################

#How random values are to be generated
rng=function(data1, polynomial){
  data2=data.frame(Crime=data1$crimes, pc1=data1$pc1)
  n=length(data$crimes)
  data2$crimes=rnorm(n=n,predict(polynomial, newdata=data2),sd(polynomial$residuals)) #Randomizing a new set of crime levels based on our polynomial regression
  return(data2)
}

f1=function(data2){
  result <- lm(crimes~poly(pc1,2), data=data2) #Fit polynomial model
  newCrime=predict(result, newdata = data) #Fitting our new data to a polynomial regression model
  return(newCrime)
}

res_conf=boot(data, statistic=f1, R=10, mle=polynomial, ran.gen=rng, sim="parametric")


f2=function(data3){ 
  result <- lm(crimes~poly(pc1,2), data=data3) #fit polynomial model
  Newcrimes=predict(result,newdata=data) #Fitted line of crime rates
  n=length(data3$crimes) 
  predictedCrimes=rnorm(n,Newcrimes, sd(polynomial$residuals)) #Generating new crime rate from residuals, normally distributed
  return(predictedCrimes)
}
 
res_pred=boot(data, statistic=f2, R=10, mle=polynomial, ran.gen=rng, sim="parametric")

env_conf=envelope(res_conf) #compute confidence bands
env_pred=envelope(res_pred) #Compute prediction bands

#pdf('Scatterplot.pdf')
ggplot(data=data) +
  geom_point(mapping = aes(x=pc1, y=crimes), shape=1) +
  geom_line(mapping= aes(x=pc1, y=fitted(polynomial), color="red")) + 
  geom_line(mapping =  aes(x = pc1, y = env_conf$point[2,]), color="#00AFBB") +
  geom_line(mapping =  aes(x = pc1, y = env_conf$point[1,]), color="#00AFBB") + 
  geom_line(mapping =  aes(x = pc1, y = env_pred$point[1,]), color="#52854C") +
  geom_line(mapping =  aes(x = pc1, y = env_pred$point[2,]), color="#52854C") +
  xlab("PC1") + 
  ylab("crimes")
#dev.off()


  
  


