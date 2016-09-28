require(data.table)
require(reshape2)
require(corrplot)
library(Metrics)
require(neuralnet)

# Fitting an ANN
maxs <- apply(currentMOH, 2, max) 
mins <- apply(currentMOH, 2, min)

scaledCurrentMOH <- as.data.frame(scale(currentMOH, center = mins, scale = maxs - mins))

#index <- sample(1:nrow(currentMOH),round(0.75*nrow(currentMOH)))
train_ <- scaledCurrentMOH[1:39,]
test_ <- scaledCurrentMOH[40:52,]

n <- names(train_)
f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(10),linear.output = FALSE, learningrate = 0.1, algorithm = "backprop")

plot(nn)

predict.nn <- compute(nn,test_[,2:ncol(currentMOH)])

predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)

MSE.nn <- sum((test.rescaled - predict.nn_)^2)/nrow(test_)
MSE.nn
#...................................................................#

# Plot ANN results vs Actual results
par(mfrow = c(2,1))

plot(currentMOH[40:52,]$cases, predict.nn_, col='red', main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(c(40:52), currentMOH[40:52,]$cases, xlab = "Week", ylab = "Cases", type = "l", col = "1")
lines(c(40:52), predict.nn_, xlab = "", ylab = "Cases", type = "l", col = "2")
par(new = T)
#plot(c(40:52), predict.nn_, xaxt = "n", yaxt = "n", xlab = "", ylab = "Cases", type = "l", col = "2")
legend("topleft",legend=c("Actual","Predicted"),bty="n",lwd=3,col=c(1,2))
axis(4)
#...................................................................#

# ANN as a function
ann = function(hidden) {
  maxs <- apply(currentMOH, 2, max) 
  mins <- apply(currentMOH, 2, min)
  
  scaledCurrentMOH <- as.data.frame(scale(currentMOH, center = mins, scale = maxs - mins))
  
  #index <- sample(1:nrow(currentMOH),round(0.75*nrow(currentMOH)))
  train_ <- scaledCurrentMOH[1:39,]
  test_ <- scaledCurrentMOH[40:52,]
  
  n <- names(train_)
  f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))
  nn <- neuralnet(f,data=train_,hidden=hidden,linear.output=FALSE, learningrate = 0.3, algorithm = "backprop")
  
  predict.nn <- compute(nn,test_[,2:ncol(currentMOH)])
  
  predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
  test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
  
  MSE.nn <- sum((test.rescaled - predict.nn_)^2)/nrow(test_)
  MSE.nn
}
#..............................................................................#

# Test ANN for multiple nodes
MSEArray = array(100000)
for (nodes in c(3:30)) {
  MSEArray[nodes] = ann(nodes)
}
MSEArray
#..............................................................................#

# ANN with two hidden layers

ann = function(currentMOH = )
maxs <- apply(currentMOH, 2, max) 
mins <- apply(currentMOH, 2, min)

scaledCurrentMOH <- as.data.frame(scale(currentMOH, center = mins, scale = maxs - mins))

#index <- sample(1:nrow(currentMOH),round(0.75*nrow(currentMOH)))
train_ <- scaledCurrentMOH[1:39,]
test_ <- scaledCurrentMOH[40:52,]

n <- names(train_)
f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))

bestANN = neuralnet()
bestLayer1 = 1
bestLayer2 = 1
bestMSE = 10000
range1 = c(8:20)
range2 = c(5:16)
for(layer1 in range1) {
  for(layer2 in range2) {
    nn <- neuralnet(f,data=train_,hidden=c(layer1:layer2),linear.output = FALSE, learningrate = 0.01, algorithm = "backprop", rep = 5)
    
    predict.nn <- compute(nn,test_[,2:ncol(currentMOH)])
    
    predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
    test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
    
    MSE <- mse(test.rescaled, predict.nn_)
    
    if(bestMSE > MSE) {
      bestMSE = MSE
      bestANN = nn
      bestLayer1 = layer1
      bestLayer2 = layer2
    }
  }
}

bestMSE
#..............................................................................#

# Predict the future values
predict.nn <- compute(bestANN,test_[,2:ncol(currentMOH)])

predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)

mse(test.rescaled, predict.nn_)

data = data.frame(week = c(40:52), predicted = as.numeric(predict.nn_), cleanedCases = as.numeric(currentMOH[40:52,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013 Prediction by ANN - Dehiwala MOH  MSE = ", round(bestMSE, digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)

# Model fit
predict.nn <- compute(bestANN,train_[,2:ncol(currentMOH)])
predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
train.rescaled <- (train_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)

data = data.frame(week = c(1:39), predicted = as.numeric(predict.nn_), cleanedCases = as.numeric(currentMOH[1:39,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013 Fit For ANN - Kolonnawa MOH  MSE = ", round(mse(train.rescaled, predict.nn_), digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)









# ANN function with two hidden layers

ann <- function(currentMOH=currentMOH, trainEnd, testEnd, layer1Neurons, layer2Neurons, learningrate = 0.01, algorithm = "backprop", repitions = 3) {
  maxs <- apply(currentMOH, 2, max) 
  mins <- apply(currentMOH, 2, min)
  
#  print(paste(ncol(currentMOH), " ", trainEnd, " ", testEnd, " ", layer1Neurons, " ", layer2Neurons, " ", learningrate, " ", algorithm, " ", repitions))
  
  scaledCurrentMOH <- as.data.frame(scale(currentMOH, center = mins, scale = maxs - mins))
  
  #index <- sample(1:nrow(currentMOH),round(0.75*nrow(currentMOH)))
  train_ <- scaledCurrentMOH[1:trainEnd,]
  test_ <- scaledCurrentMOH[as.numeric(trainEnd+1):testEnd,]
  print(paste("train_ = ", dim(train_)))
  print(paste("test_ = ", dim(test_)))
  
  
  
  n <- names(train_)
  f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))
  
  bestANN = NULL
  bestLayer1 = 1
  bestLayer2 = 1
  bestMSE = 10000
  range1 = layer1Neurons
  range2 = layer2Neurons
  for(layer1 in range1) {
    for(layer2 in range2) {
      nn <- neuralnet(f,
                      data=train_,
                      hidden=c(layer1:layer2),
                      linear.output = FALSE, 
                      learningrate = learningrate, 
                      algorithm = algorithm, 
                      rep = repitions)
      
      predict.nn <- compute(nn,test_[,2:ncol(currentMOH)])
      
      predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
      test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
      
      MSE <- mse(test.rescaled, predict.nn_)
      print(paste("currentMSE = ", MSE, ", layer 1 = ", layer1, ", layer 2 = ", layer2))

      if(bestMSE > MSE) {
        bestMSE = MSE
        bestANN = nn
        bestLayer1 = layer1
        bestLayer2 = layer2
      }
    }
  }
  
  print(paste("Best MSE = ",bestMSE))
  print(paste("layer 1 = ", bestLayer1))
  print(paste("layer 2 = ", bestLayer2))
   
  bestANN
}
#..............................................................................#


# Analyse currentMOH with ANN function


bestANN = ann(currentMOH = currentMOH, trainEnd = 78, testEnd = 104, layer1Neurons = c(5:15), layer2Neurons = c(7:15), repitions = 5)


#.............................................................................#
require(gridExtra)
grid.arrange(myplot1, myplot2, ncol=2)


# Prediction
maxs <- apply(currentMOH, 2, max) 
mins <- apply(currentMOH, 2, min)
scaledCurrentMOH <- as.data.frame(scale(currentMOH, center = mins, scale = maxs - mins))
train_ <- scaledCurrentMOH[1:trainEnd,]
test_ <- scaledCurrentMOH[as.numeric(trainEnd+1):testEnd,]

predict.nn <- compute(bestANN,test_[,2:ncol(currentMOH)])
predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
test.rescaled <- (test_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)

data = data.frame(week = c(79:104), predicted = as.numeric(predict.nn_), cleanedCases = as.numeric(currentMOH[79:104,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 Prediction by ANN - Colombo MOH  MSE = ", round(mse(test.rescaled, predict.nn_), digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)

# Model fit
predict.nn <- compute(bestANN,train_[,2:ncol(currentMOH)])
predict.nn_ <- predict.nn$net.result*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)
train.rescaled <- (train_$cases)*(max(currentMOH$cases)-min(currentMOH$cases))+min(currentMOH$cases)

data = data.frame(week = c(1:78), predicted = as.numeric(predict.nn_), cleanedCases = as.numeric(currentMOH[1:78,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 Fit For ANN - Colombo MOH  MSE = ", round(mse(train.rescaled, predict.nn_), digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)
