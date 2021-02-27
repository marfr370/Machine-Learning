#####################  
##  The data file parkinson.csv is composed of a range of biomedical voice
##  measurements from 42 people with early-stage Parkinson's disease recruited to a
##  six-month trial of a telemonitoring device for remote symptom progression
##  monitoring. The purpose is to predict Parkinson's disease symptom score (motor    
##  UPDRS) from the following voice characteristics:
##
##  • Jitter(%),Jitter(Abs),Jitter:RAP,Jitter:PPQ5,Jitter:DDP - Several measures of
##   variation in fundamental frequency
##
##  • Shimmer,Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,Shimmer:APQ11,Shi
##  mmer:DDA - Several measures of variation in amplitude
##
##  • NHR,HNR - Two measures of ratio of noise to tonal components in the voice

##  • RPDE - A nonlinear dynamical complexity measure

##  • DFA - Signal fractal scaling exponent

##  • PPE - A nonlinear measure of fundamental frequency variation

csvfile = read.csv("parkinsons.csv")
csvfile = scale(csvfile)
set.seed(12345)
n=dim(csvfile)[1]
id=sample(1:n, floor(n*0.6))
train=csvfile[id,]
test=csvfile[-id,]
#parentheses are for functions and brackets are for indicating the position of items
##Implement 4 following functions by using basic R commands only (no external packages)

#loglikelihood function that for a given parameter vector w and dispersion sigma computes the log-likelihood function log P(D|w,sigma)
#for the model from step 1 for the training data
#we use loglikelihood instead of the lm func
Loglikelihood = function(w, sigma, train) {
  n = dim(train)[1]
  Y = train[,5]
  X = as.matrix(train[,7:22])
  #-n/2*(ln(2pi)+ln(sigma^2) = -n/2*(log(2*pi*sigma^2))
  return(- n / 2 * log(2 * pi * sigma^2) - 1 / (2 * sigma^2) * sum((Y-X%*%w)^2)) #sum(t(Y - X%*%w)%*%(Y - X %*% w)))
}

Ridge = function(a, lamda, train){
  w = a[-1]
  sigma = a[1]
  return(- Loglikelihood(w, sigma, train) + lamda * sum(w)^2)
}

RidgeOpt = function(lamda, train){
 optim(rep(1,17), fn=Ridge, lamda = lamda, train = train , method = "BFGS")
}
#degrees of freedom is = n-k-1 where n is the number of observations, k is the number of variables
DF = function(){
  
}

## TASK 4: compute optimal w parameters for lamda = 1 etc. Use the estimated parameters
## to predict the motor_IPDRS values for training and test data and report the training and 
## the mean squared error (MSE) values. Which penalty parameter is most appropriate? Why is mean
## squared error (MSE) more appropriate measure here than other empirical risk functions? 
#lamda=1 => value 4931.945
#lamda=100 => value 4951.242
#lamda=1000 => value 4990.468
lamda=1000
r = RidgeOpt(lamda, train)


parameters = r$par
opt_weights = parameters[-1]
opt_sigma = parameters[1]

MSEval = function(opt_weights, train){
  n = dim(train)[1]
  y = train[5]
  x = train[, 7:22]
  MSE = sum((y - x%*%opt_weights)^2/n)
}

lamda1000 = MSEval(opt_weights, train)
##lamda 100 bäst för 4, 5. AIC värdena bör vara 7000, Jonas 6500

