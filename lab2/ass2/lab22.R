setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
colClasses = c("numeric", rep("factor",4), "numeric", rep("factor",3), "numeric", "factor", rep("numeric",4), rep("factor", 2))
dataPure = read.csv('bank-full.csv', sep=';', header=TRUE, colClasses=colClasses)

# TASK 1
# Split data 40/30/30
data = subset(dataPure, select = -duration)
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
id3 = setdiff(id1,id2)
train = data[id, ]
valid = data[id2,]
test = data[id3,]

# TASK 2
# Fit decision trees to training data
library(tree)
n = dim(train)[1]
fita = tree(y ~ ., data=train)
fitb = tree(y ~ ., data=train, control=tree.control(nobs= n, minsize=7000))
fitc = tree(y ~ ., data=train, control=tree.control(nobs= n, mindev=0.0005))

print("Training data")
summary(fita)
summary(fitb)
summary(fitc)
print("Validation data")

# MISSCLASS ERROR VALIDATION DATA MODEL A
predicta = predict(fita, newdata=valid, type='class')
1-sum(diag(table(valid$y,predicta))/sum(table(valid$y,predicta)))

# MISSCLASS ERROR VALIDATION DATA MODEL B
predictb = predict(fitb, newdata=valid, type='class')
1-sum(diag(table(valid$y,predictb))/sum(table(valid$y,predictb)))

# MISSCLASS ERROR VALIDATION DATA MODEL C
predictc = predict(fitc, newdata=valid, type='class')
1-sum(diag(table(valid$y,predictc))/sum(table(valid$y,predictc)))

# TASK 3
# Finding optimal tree depth
library(ggplot2)
# USING MODEL C TO FIND OPTIMAL DEPTH TREE
fit = tree(y ~ ., data=train, control=tree.control(nobs= n, mindev=0.0005))
trainScore=rep(0,50)
validScore=rep(0,50)
# NUMBER OF DATA POINTS IN VALIDATION AND TRAINING DATA
n_train = dim(train)[1]
n_valid = dim(valid)[1]

for(i in 2:50) {
  # PRUNE TREE FOR EACH LEAF SIZE
  prunedTree=prune.tree(fit,best=i)
  # PREDICT FOR VALID DATA OF THE PRUNED TREE
  pred=predict(prunedTree, newdata=valid, type="tree")
  # DEVIANCE SCORE DIVIDED BY NUMBER OF DATA POINTS, SINCE THERE DIFFERENT IN SIZE
  trainScore[i]=deviance(prunedTree)/n_train
  validScore[i]=deviance(pred)/n_valid
}
# PLOT IT
data_frame = data.frame(y1=trainScore[2:50], y2=validScore[2:50], x=2:50)
pdf('task3.pdf')
ggplot(data_frame, aes(x=x)) +
  geom_point(aes(y=y1, color='Training score')) + geom_point(aes(y=y2, color='Validation score')) + labs(color="") + 
  xlab('Number of leaves') + ylab('Deviance')
dev.off()

# BEST TREE WHERE LEAFES=22
bestTree=prune.tree(fit,best=22)
summary(bestTree)

# MISSCLASS AND CONFUSION MATRIX
Yfit=predict(bestTree, newdata=test, type="class")
print('Task 3 missclass')
table(test$y,Yfit)
1-(sum(diag(table(test$y,Yfit)))/sum(table(test$y,Yfit)))

# TASK 4
library(rpart)
library(rpart.plot)
# LOSS MATRIX
l = matrix(c(0, 5, 1, 0), ncol = 2)
# USING rpart TO USE THE LOSS MATRIX
fit = rpart(y ~ ., data=test, parms = list(loss = l))
# PREDICT WITH LOSS MATRIX TREE
pred <- predict(fit, type = "class")
# CUNFUSION MATRIX AND MISSCLASS RATE
table(test$y, pred)
1-(sum(diag(table(test$y,pred)))/sum(table(test$y,pred)))

# TASK 5
#library(pROC)
library(e1071)
library(ggplot2)

# NAIVE BAYES MODEL
fit_bayes = naiveBayes(y ~ ., data=train)
# PREDICTION OBJECT FOR NAIVE BAYES MODEL
y_bayes = predict(fit_bayes, newdata=test, type='raw')[,2]
# PREDICTION OBJECT FOR BEST TREE MODEL FROM 2.1
y_tree = predict(bestTree, test, type='vector')[,2]

# VECTOR INIT
tpr_bayes=vector()
fpr_bayes=vector()
tpr_tree=vector() 
fpr_tree=vector()
i=1
# LOOP OVER THRESHOLD (thr=pi) FROM 0 to 1 TO GET COMPLETE ROC CURVE
for (thr in seq(from=0.00, to=1, by=0.05)) {
  # PREDICTION USING PRINCIPLE FROM LAB FOR NAIVE BAYES MODEL
  y_hat_bayes = ifelse(y_bayes>thr, 'yes', 'no')
  # PREDICTION USING PRINCIPLE FROM LAB FOR TREE MODEL
  y_hat_tree = ifelse(y_tree>thr, 'yes', 'no')
  
  # CONFUSION MATRIXES
  conf_m_bayes = table(test$y, y_hat_bayes)
  conf_m_tree = table(test$y, y_hat_tree)
  
  # UGLY CODE TO ADD 0 COLUMNS IF THEY ARE EMPTY
  if (is.na(table(y_hat_tree)[2])) {
    if (colnames(conf_m_tree)[1] == 'yes') {
      conf_m_tree = cbind(c(0,0), conf_m_tree)
    } else {
      conf_m_tree = cbind(conf_m_tree, c(0,0))
    }
  }
  if (is.na(table(y_hat_bayes)[2])) {
      if (colnames(conf_m_bayes)[1] == 'yes') {
        conf_m_bayes = cbind(c(0,0), conf_m_bayes)
      } else {
        conf_m_bayes = cbind(conf_m_bayes, c(0,0))
      }
  }
  
  # CALCULATE true positive rates AND false positive rates FOR BAYES MODEL ACCORDING TO LECTURE
  tpr_bayes[i] = conf_m_bayes[2,2]/sum(conf_m_bayes[2,])
  fpr_bayes[i] = conf_m_bayes[1,2]/sum(conf_m_bayes[1,])
  
  # CALCULATE true positive rates AND false positive rates FOR TREE MODEL ACCORDING TO LECTURE
  tpr_tree[i] = conf_m_tree[2,2]/sum(conf_m_tree[2,])
  fpr_tree[i] = conf_m_tree[1,2]/sum(conf_m_tree[1,])
  
  i = i + 1
}
# PLOT THE RESULT
df <- data.frame(tpr=c(tpr_tree,tpr_bayes), fpr=c(fpr_tree, fpr_bayes),
                 Method=c(rep(paste("Optimal Tree"), each=length(tpr_tree)),
                          rep(paste("Naïve Bayes"), each=length(tpr_bayes))) )
pdf('task5.pdf')
ggplot(aes(x=fpr, y=tpr, color=Method), data=df) + 
  geom_line() + xlim(0,1) + ylim(0,1) + 
  xlab('fpr') + ylab('tpr')

dev.off()