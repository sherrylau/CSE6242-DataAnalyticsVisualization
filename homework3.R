#################################
### Name: Kawing Lau (Sherry) ###
### GTID: klau37 ################
### CSE6242 Homework 3 ##########

library(mlbench)
library(knitr)
library(caret)
library(reshape2)
library(ggplot2)
library(glm2)

#### Q2 ####
# Function to fit logistic regression based on gradient descent
logreg_gd = function(x, y, initial_w, alpha, maxit, conv_thres){
  if(is.matrix(x)&is.numeric(x)){
    if(any(!is.na(x))){
      # Implement Sigmoid function
      sigmoid = function(z){
        s = 1/(1+exp(-z))
        return(s)
      }
      # Implement Cost function 
      cost = function(w){
        n = nrow(x)
        g = sigmoid(x %*% c(w)) # x is a matrix, w is a vector
        L = -(1/n)*(sum((y*log(g)) + ((1-y)*log(1-g))))
        return(L)
      }
      # Define the gradient function
      gradient = function(w){
        n = nrow(x)
        g = sigmoid(x %*% c(w))
        grad = (1/n)*(t(x) %*% (g-y))
        return(t(grad))
      }
      gradient_descent = function(initial_w, alpha, maxit, conv_thres){
        # Initial w
        w = initial_w
        c = cost(w)
        c_initial = cost(w)
        c_history = NULL
        # Update w in maxit times
        for(i in 1:maxit){
          if((c_initial - c)<conv_thres){
            w = w - alpha * gradient(w)
            c = cost(w)
            c_history[[i]] = cost(w)
          }
        }
        return(list(w=w, c_history=c_history, iter=length(c_history)))
      }
      # Derive w and c using gradient descent
      gd = gradient_descent(initial_w, alpha, maxit, conv_thres)
      w = gd$w
      c_history = gd$c_history
      iter = gd$iter
      
      return(list(w_optm = w, c_history = c_history, iter = iter))
    } else{
      print("x contains missing values")
    }
  } else{
    print("x as to be a numeric matrix")
  }
}

# Function to predict probability based on fitted logreg_gd
logreg_gd_predict = function(new_x, w_optm){
  sigmoid = function(z){
    g = 1/(1+exp(-z))
    return(g)
  }
  prob = sigmoid(new_x %*% c(w_optm))
  return(prob)
}

#### Q3 ####
# Load Breast Cancer data
data("BreastCancer")

# Function to split data into 70% training and 30% testing with 10 times repetitions and 
# fit logistic regression from Q2 function - logreg_gd
logreg_gd_split_repeat = function(initial_w, alpha, conv_thres){
  # Function to impute missing by column mean
  impute_mean = function(x){
    x = as.numeric(as.character(x))
    x[is.na(x)] = mean(x, na.rm=TRUE)
    return(x)
  }
  # Function to calculate accuracy
  accuracy = function(y_pred, y){
    e = y - y_pred
    a = length(e[e==0])/length(y)
    return(a)
  }
  lr_c_history = NULL
  accuracy_train = NULL
  accuracy_test = NULL
  for (i in 1:10){
    # Partition dataset into 70% training and 30% testing
    train_idx = createDataPartition(BreastCancer$Class, p = 0.7)
    train = BreastCancer[unlist(train_idx),]
    test = BreastCancer[-unlist(train_idx),]
    
    trainX = train[,2:10]
    trainX = apply(trainX, 2, impute_mean)
    trainY = ifelse(train$Class=="benign",1,0)
    
    testX = test[,2:10]
    testX = apply(testX, 2, impute_mean)
    testY = ifelse(test$Class=="benign",1,0)
    
    # Fit Logistic Regression
    lr_fit = logreg_gd(trainX, trainY, initial_w, alpha, maxit=5000, conv_thres)
    lr_w = lr_fit$w_optm
    lr_c_history[[i]] = lr_fit$c_history
    train_prob = logreg_gd_predict(trainX, lr_w)
    train_event = ifelse(train_prob>0.5,1,0)
    test_prob = logreg_gd_predict(testX, lr_w)
    test_event = ifelse(test_prob>0.5,1,0)
    accuracy_train[[i]] = accuracy(train_event, trainY)
    accuracy_test[[i]] = accuracy(test_event, testY)
  }
  avg_train_accuracy = mean(accuracy_train)
  avg_test_accuracy = mean(accuracy_test)
  return(list(avg_train_accuracy=avg_train_accuracy,
              avg_test_accuracy=avg_test_accuracy,
              c_history=lr_c_history))
}

initial_w = rep(0, ncol(BreastCancer[,2:10]))
alpha = 0.05
conv_thres = 0.2
fit = logreg_gd_split_repeat(initial_w, alpha, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit$avg_test_accuracy,4))
c_history = as.matrix(plyr::ldply(fit$c_history, rbind))
c_history = melt(c_history)
colnames(c_history) = c("run", "iter", "cost")
ggplot(c_history, aes(x=iter, y=cost, color=as.factor(run))) + 
  geom_line() + 
  labs(x="Iterations to meet convergence threshold",
       y="Cost",
       color="Run",
       title="Cost across iterations for 10 repetitions")

alpha = 0.05
conv_thres = 0.2

# Setup 1: all 0
initial_w1 = rep(0, ncol(BreastCancer[,2:10]))
fit1 = logreg_gd_split_repeat(initial_w1, alpha, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(fit1$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit1$avg_test_accuracy,4))

# Setup 2: all 0.4
initial_w2 = rep(0.4, ncol(BreastCancer[,2:10]))
fit2 = logreg_gd_split_repeat(initial_w2, alpha, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(fit2$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit2$avg_test_accuracy,4))

# Setup 3: randomly selected values from {0, 0.05,,, 0.35, 0.4}
initial_w3 = sample(seq(0,0.4,0.05), size=9, replace=TRUE)
fit3 = logreg_gd_split_repeat(initial_w3, alpha, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(fit3$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit3$avg_test_accuracy,4))

alpha_ls = c(0.05, 0.15, 0.25)
conv_ls = c(0.1, 0.2, 0.3)
initial_w = rep(0, ncol(BreastCancer[,2:10]))

comb = unlist(lapply(alpha_ls,function(a) lapply(conv_ls, function (b) c(a, b))),recursive=FALSE)

row = NULL
for(i in 1:length(comb)){
  alpha = unlist(comb[i])[1]
  conv_thres = unlist(comb[i])[2]
  fit = logreg_gd_split_repeat(initial_w, alpha, conv_thres)
  row[[i]] = c(alpha, conv_thres, fit$avg_train_accuracy, fit$avg_test_accuracy)
}
alpha_conv = data.frame(do.call(rbind, row))
colnames(alpha_conv) = c("alpha", "convergence threshold", "average train accuracy", "average test accuracy")
kable(alpha_conv)

#### Q4 ####
logreg_glm_split_repeat = function(split_size, initial_w, conv_thres){
  # Function to impute missing by column mean
  impute_mean = function(x){
    x = as.numeric(as.character(x))
    x[is.na(x)] = mean(x, na.rm=TRUE)
    return(x)
  }
  # Function to calculate accuracy
  accuracy = function(y_pred, y){
    e = y - y_pred
    a = length(e[e==0])/length(y)
    return(a)
  }
  lr_c_history = NULL
  accuracy_train = NULL
  train_neg_loglikelihood = NULL
  accuracy_test = NULL
  test_neg_loglikelihood = NULL
  for (i in 1:10){
    # Partition dataset into 70% training and 30% testing
    train_idx = createDataPartition(BreastCancer$Class, p = split_size)
    train = BreastCancer[unlist(train_idx),]
    test = BreastCancer[-unlist(train_idx),]
    
    trainX = train[,2:10]
    trainX = apply(trainX, 2, impute_mean)
    trainY = ifelse(train$Class=="benign",1,0)
    train = data.frame(cbind(trainX, trainY))
    
    testX = test[,2:10]
    testX = apply(testX, 2, impute_mean)
    testY = ifelse(test$Class=="benign",1,0)
    
    # Fit Logistic Regression using glm2
    train$trainY = as.factor(train$trainY)
    glm_fit = glm2(trainY~., 
                   data = train,
                   family=binomial,
                   start = initial_w,
                   control=list(maxit=5000, epsilon=conv_thres))
    glm_w = glm_fit$coefficients
    # Train Accuracy
    train_prob = glm_fit$fitted.values
    train_event = ifelse(train_prob>0.5,1,0)
    accuracy_train[[i]] = accuracy(train_event, trainY)
    # Train Loss 
    train_neg_loglikelihood[[i]] = -mean(trainY * log(train_prob) + (1-trainY) * log(1-train_prob))
    # Test Accuracy
    test_prob = predict(glm_fit, newdata=data.frame(testX), type="response")
    test_event = ifelse(test_prob>0.5,1,0)
    accuracy_test[[i]] = accuracy(test_event, testY)
    # Test Loss
    test_neg_loglikelihood[[i]] = -mean(testY * log(test_prob) + (1-testY) * log(1-test_prob))
  }
  avg_train_accuracy = mean(accuracy_train)
  avg_train_neg_logll = mean(train_neg_loglikelihood)
  avg_test_accuracy = mean(accuracy_test)
  avg_test_neg_logll = mean(test_neg_loglikelihood)
  return(list(avg_train_accuracy=avg_train_accuracy,
              avg_train_neg_logll=avg_train_neg_logll,
              avg_test_accuracy=avg_test_accuracy,
              avg_test_neg_logll=avg_test_neg_logll))
}

initial_w = rep(0, 10)
conv_thres = 0.2
glm_fit = logreg_glm_split_repeat(initial_w, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(glm_fit$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(glm_fit$avg_test_accuracy,4))

conv_thres = 0.2

# Setup 1: all 0
initial_w1 = rep(0, 10)
glm_fit1 = logreg_glm_split_repeat(initial_w1, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(glm_fit1$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(glm_fit1$avg_test_accuracy,4))

# Setup 2: all 0.4
initial_w2 = rep(0.4, 10)
glm_fit2 = logreg_glm_split_repeat(initial_w2, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(glm_fit2$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(glm_fit2$avg_test_accuracy,4))

# Setup 3: randomly selected values from {0, 0.05,,, 0.35, 0.4}
initial_w3 = sample(seq(0,0.4,0.05), size=10, replace=TRUE)
glm_fit3 = logreg_glm_split_repeat(initial_w3, conv_thres)
paste("Average Training Accuracy of 10 runs:", round(glm_fit3$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(glm_fit3$avg_test_accuracy,4))

conv_ls = c(0.1, 0.2, 0.3)
initial_w = rep(0, 10)

row = NULL
for(i in 1:length(conv_ls)){
  conv_thres = conv_ls[i]
  glm_fit = logreg_glm_split_repeat(initial_w, conv_thres)
  row[[i]] = c(conv_thres, fit$avg_train_accuracy, fit$avg_test_accuracy)
}
conv = data.frame(do.call(rbind, row))
colnames(conv) = c("convergence threshold", "average train accuracy", "average test accuracy")
kable(conv)

#### Q5 ####
train_size = seq(0.05, 0.95, 0.05)
initial_w = rep(0, 10)
conv_thres = 0.1
row = NULL
for(i in 1:length(train_size)){
  split_size = train_size[i]
  glm_fit = logreg_glm_split_repeat(split_size, initial_w, conv_thres)
  row[[i]] = c(split_size, glm_fit$avg_train_accuracy, glm_fit$avg_test_accuracy)
}
split = data.frame(do.call(rbind, row))
colnames(split) = c("train_size", "train_accuracy", "test_accuracy")
ggplot(split) + 
  geom_line(aes(x=train_size, y=train_accuracy, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_accuracy, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Accuracy across 10 runs",
       colour="",
       title="Average Training and Testing Accuracy for different Training size")

#### Q6 ####
train_size = seq(0.05, 0.95, 0.05)
initial_w = rep(0, 10)
conv_thres = 0.1
row_neg_logll = NULL
for(i in 1:length(train_size)){
  split_size = train_size[i]
  glm_fit = logreg_glm_split_repeat(split_size, initial_w, conv_thres)
  row_neg_logll[[i]] = c(split_size, glm_fit$avg_train_neg_logll, glm_fit$avg_test_neg_logll)
}
split_neg_logll = data.frame(do.call(rbind, row_neg_logll))
colnames(split_neg_logll) = c("train_size", "train_negll", "test_negll")
ggplot(split_neg_logll) + 
  geom_line(aes(x=train_size, y=train_negll, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_negll, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Negative Log Likelihood across 10 runs",
       colour="",
       title="Average Training and Testing Loss Function Values for different Training size")
