
tempArea = 138
uncleanedTwoColumnTempData2013= data.frame(weeks=1:52,Temprature =melt(temperatureData2013[tempArea,][,3:54])$value )

#anomaly Detection
augmented.week.info <- paste("2013", as.character(uncleanedTwoColumnTempData2013$week), "0", sep = ".")
uncleanedTwoColumnTempData2013$weeks <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))
anomaly.results <- AnomalyDetectionTs(uncleanedTwoColumnTempData2013, 
                                      max_anoms = 0.01, direction = 'both', plot = TRUE, e_value = TRUE)
cleanedTwoColumnTempData2013<- uncleanedTwoColumnTempData2013
if(nrow(anomaly.results$anoms) == 0) {
  uncleanedTwoColumnTempData2013$Temprature
} else {
  replace.index.list <- which(
    uncleanedTwoColumnTempData2013$weeks == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
  cleanedTwoColumnTempData2013$Temprature = replace(uncleanedTwoColumnTempData2013$Temprature, replace.index.list, anomaly.results$anoms$expected_value)
}