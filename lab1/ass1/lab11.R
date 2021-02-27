setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = read.csv('optdigits.csv', header=F)

# TASK 1
# Split data 50/25/25
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
id3 = setdiff(id1,id2)
train = data[id,]
valid = data[id2,]
test = data[id3,]

# TASK 2
# Use training data to fit 30-nearest neighbor classifier with function kknn() and
# kernel=”rectangular” from package kknn and estimate
#   • Confusion matrices for the training and test data (use table())
#   • Misclassification errors for the training and test data
library(kknn)
library(gridExtra)
library(grid)
library(knitr)

kknn_train = kknn(as.factor(V65) ~ ., train=train, test=train, k = 30, kernel = 'rectangular')
kknn_fitted_train = fitted(kknn_train)

kknn_test = kknn(as.factor(V65) ~ ., train=train, test=test, k = 30, kernel = 'rectangular')
kknn_fitted_test = fitted(kknn_test)

confusionM_train = table(kknn_fitted_train, train$V65)
misclassification_train = 1-sum(diag(confusionM_train)/sum(confusionM_train))

confusionM_test = table(kknn_fitted_test, test$V65)
misclassification_test = 1-sum(diag(confusionM_test)/sum(confusionM_test))

# TASK 3
# Find any 2 cases of digit “8” in the training data which were easiest to classify 
# and 3 cases that were hardest to classify 
# (i.e. having highest and lowest probabilities of the correct class).
library(dplyr)
make_heatmap = function(list, name) {
  x <- unlist(list)
  x <- matrix(x, nrow=8, byrow=TRUE)
  pdf(paste(name, '.pdf'))
  heatmap(x, Colv=NA, Rowv=NA, main=name, labRow=NA, labCol=NA)
  dev.off()
}
eights_row_no = which(grepl(8, train$V65))
probs_eights = data.frame(row_no=eights_row_no, probs=kknn_train[["prob"]][eights_row_no,9])

easy = (probs_eights %>% top_n(2))[1:2,1]
hard = (probs_eights %>% top_n(-3))[1:3,1]

make_heatmap(train[easy[1],][1:64], 'Easy 1')
make_heatmap(train[easy[2],][1:64], 'Easy 2')
make_heatmap(train[hard[1],][1:64], 'Hard 1')
make_heatmap(train[hard[2],][1:64], 'Hard 2')
make_heatmap(train[hard[3],][1:64], 'Hard 3')

# TASK 4
# Plot errors and dependecy
library(ggplot2)
x4 = vector()
y4_train = vector()
y4_valid = vector()
for (k in 1:30) {
  kknn_train = kknn(as.factor(V65) ~ ., train=train, test=train, k = k, kernel = 'rectangular')
  kknn_valid = kknn(as.factor(V65) ~ ., train=train, test=valid, k = k, kernel = 'rectangular')
  confusionM_train = table(fitted(kknn_train), train$V65)
  confusionM_valid = table(fitted(kknn_valid), valid$V65)
  x4[k] = k
  y4_train[k] = (1-sum(diag(confusionM_train)/sum(confusionM_train)))*100
  y4_valid[k] =  (1-sum(diag(confusionM_valid)/sum(confusionM_valid)))*100
}
data_frame = data.frame(y1=y4_train, y2=y4_valid, x=x4)
task4_plot = ggplot(data_frame, aes(x=x4)) +
  geom_point(aes(y=y1, color='Training error')) + geom_point(aes(y=y2, color='Validation error')) + labs(color="") + 
  xlab('K-value') + ylab('missclassification error (%)')
pdf('task4.pdf')
task4_plot
dev.off()

# TASK 5
x5 = vector()
y5 = vector()
for (k in 1:30) {
  prob_kknn = kknn(as.factor(V65) ~ ., train=train, test=valid, k = k, kernel = 'rectangular')
  pred_kknn = fitted(prob_kknn)
  confusionM_valid = table(pred_kknn, valid$V65)
  x5[k] = k
  crossent = 0
  for (i in 1:dim(valid)[1]) {
    q = prob_kknn[["prob"]][i, valid$V65[i]+1]
    crossent = crossent + (log(q+1e-15))
  }
  y5[k] = -crossent / dim(valid)[1]
}
pdf('task5.pdf')
plot(x5, y5, xlab='K-value', ylab='cross-entropy')
dev.off()