library(Matrix)
library(Metrics)
library(xgboost)

require(xgboost)
require(Ckmeans.1d.dp)
library(AnomalyDetection)

#sparce_matrix_train <- sparse.model.matrix(Demanda_uni_equil ~ .-1, data = week789dataLess2000)
#model <- xgb.cv(data = sparce_matrix_train, label = week789dataLess2000[, "Demanda_uni_equil"], nfold = 4, max.depth = 23, eta = 0.06, nthread = 2, nround = 300, objective = "reg:linear", early.stop.round = 10, maximize = FALSE)
#model <- xgboost(data = sparce_matrix_train, label = week789dataLess2000[, "Demanda_uni_equil"], nfold = 4, max.depth = 23, eta = 0.05, nthread = 2, nround = 63, objective = "reg:linear", maximize = FALSE)



# XGBoost or two years data
sparce_matrix_train <- sparse.model.matrix(cases ~ .-1, data = as.data.frame(currentMOH[1:104, ]))
eta = 0.01
nround = 100000
max.depth = 7
model <- xgb.cv(data = sparce_matrix_train, label = currentMOH[, "cases"][1:104], nfold = 4, max.depth = max.depth, eta = eta, nthread = 4, nround = nround, objective = "reg:linear", early_stopping_rounds  = 50, maximize = FALSE)

nround = 220
model <- xgboost(data = sparce_matrix_train, label = currentMOH[, "cases"][1:104], nfold = 4, max.depth = max.depth, eta = eta, nthread = 4, nround = nround, objective = "reg:linear", maximize = FALSE)

sparce_matrix_test <- sparse.model.matrix(cases ~ .-1, data = as.data.frame(Actualvalues2014[1:52, ]))
pred = predict(model, sparce_matrix_test)
mse(predicted = pred, actual = Actualvalues2014[, "cases"][1:52])

importance_matrix <- xgb.importance(sparce_matrix_train@Dimnames[[2]], model = model)
xgb.plot.importance(importance_matrix)

data = data.frame(week = c(1:52), predicted = as.numeric(pred), cleanedCases = as.numeric(currentMOH[79:104,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 Prediction By XGBoost - COLOMBO MOH  MSE = ", round(mse(predicted = pred, actual = Actualvalues2014[, "cases"][1:52])
                                                                                               , digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable,shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title)

#comparison plot
previouspred=as.numeric(pred)
#ploting compared data
data = data.frame(week = c(79:104), WithMobility = as.numeric(pred), cleanedCases = as.numeric(currentMOH[79:104,]$cases),WithoutMobility=previouspred)
dmelt = melt(data, id = "week")
title = paste("With Mobility(UOM) vs Without Mobility ")

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable,shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  # theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title)


# Model Fit
pred = predict(model, sparce_matrix_train)
data = data.frame(week = c(1:78), predicted = as.numeric(pred), cleanedCases = as.numeric(currentMOH[1:78,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 XGBoost fit Colombo MOH  MSE = ", round(mse(predicted = pred, actual = currentMOH[, "cases"][1:78])
                                                                                   , digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)
#anomaly detecter
AnomalyDetectionVec(unCleanedDengueData2013[,2], max_anoms=0.02, period=1440, direction='both', only_last=FALSE, plot=TRUE)
#res = AnomalyDetectionTs(unCleanedDengueData2013, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
#res$plot