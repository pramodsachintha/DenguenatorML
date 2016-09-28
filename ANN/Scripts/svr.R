require(e1071)

f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))
model <- svm(f , currentMOH[1:39,])
predictedY <- predict(model, currentMOH[40:52,])

#Tuning
tuneResult <- tune(svm, f,  data = currentMOH[1:39,], ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
tuneResult <- tune(svm, f,  data = currentMOH[1:39,], ranges = list(epsilon = seq(0.7,1,0.01), cost = 2^seq(2,4,0.1)))
print(tuneResult)
plot(tuneResult)

tunedModel <- tuneResult$best.model
predictedY <- predict(tunedModel, currentMOH[40:52,])