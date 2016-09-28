require(randomForest)

n <- names(train_)
f <- as.formula(paste("cases ~", paste(n[!n %in% "cases"], collapse = " + ")))

forest = randomForest(data = currentMOH, f)
# View the forest results.
print(forest) 

# Importance of each predictor.
print(importance(forest,type = 2)) 
