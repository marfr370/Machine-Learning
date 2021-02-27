setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(neuralnet) 
library(ggplot2)

# TASK 1
set.seed(1234567890)
Var <- runif(500, 0, 10)
sin <- data.frame(Var, Sin=sin(Var)) 
train <- sin[1:25,]
test <- sin[26:500,]

n_layers=2
n_hidden=c(5,7)

set.seed(1234567890)
winit <- sample(x = seq(from=-1, to=1, by=0.01), size=n_layers)

nn <- neuralnet(Sin ~ ., data=train, hidden=n_hidden, startweights=winit)
pred <- predict(nn, test)

data_frame <- data.frame(x1=train$Var,
                         y1=train$Sin,
                         x2=test$Var,
                         y2=pred[,1], 
                         x3=test$Var,
                         y3=test$Sin)

pdf('1.pdf')
ggplot(data_frame) +
  geom_point(aes(x=x1, y=y1, color='Training'), fill=NA, shape=21, size=4) + 
  geom_point(aes(x=x3, y=y3, color='Testing'), fill=NA, shape=21, size=2) +
  geom_point(aes(x=x2, y=y2, color='Prediction'), fill=NA, shape=21) + 
  scale_color_manual(values = c("Testing" = 'blue', 'Training' = 'black', 'Prediction' = 'red')) +
  labs(color="") + xlab("x") + ylab("y")
dev.off()

# TASK 2
set.seed(1234567890)
Var <- runif(500, 0, 20)
valid <- data.frame(Var, Sin=sin(Var))
pred <- predict(nn, valid)

data_frame <- data.frame(x1=valid$Var,
                         y1=valid$Sin,
                         x2=valid$Var,
                         y2=pred[,1], 
                         x3=train$Var,
                         y3=train$Sin)

pdf("3.pdf")
ggplot(data_frame) +
  geom_point(aes(x=x1, y=y1, color='Validation'), fill=NA, shape=21, size=2) + 
  geom_point(aes(x=x3, y=y3, color='Training'), fill=NA, shape=21, size=4) +
  geom_point(aes(x=x2, y=y2, color='Prediction'), fill=NA, shape=21) + 
  scale_color_manual(values = c("Validation" = 'blue', 'Training' = 'black', 'Prediction' = 'red')) +
  labs(color="") + xlab("x") + ylab("y")
dev.off()

# TASK 3
set.seed(1234567890)
Var <- runif(500, 0, 10)
train <- data.frame(Var, Sin=sin(Var))
set.seed(1234567890)
Var <- runif(500, 0, 20)
valid <- data.frame(Var, Sin=sin(Var))

n_layers=1
n_hidden=c(6)

set.seed(1234567890)
winit <- sample(x = seq(from=-1, to=1, by=0.01), size=n_layers)

nn <- neuralnet(Var ~ Sin, data=train, hidden=n_hidden, startweights=winit)
pred <- predict(nn, newdata=valid)

data_frame <- data.frame(x1=valid$Var, y1=valid$Sin,
                         y2=valid$Sin,
                         x2=pred[,1],
                         x3=train$Var,
                         y3=train$Sin)

pdf("4.pdf")
ggplot(data_frame) +
  geom_point(aes(x=y3, y=x3, color='Training'), fill=NA, shape=21, size=2) +
  geom_point(aes(x=y1, y=x1, color='Validation'), fill=NA, shape=21) +
  geom_point(aes(x=y2, y=x2, color='Prediction'), fill=NA, shape=21) + 
  scale_color_manual(values = c("Validation" = 'blue', 'Training' = 'black', 'Prediction' = 'red')) +
  labs(color="") + xlab("x") + ylab("y")
dev.off()

