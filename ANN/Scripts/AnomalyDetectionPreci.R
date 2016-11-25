
PreciArea = 152

uncleanedTwoColumnPreciData2013= data.frame(weeks=1:52,Precipitation =melt(rainfallData2013[PreciArea,][,2:53])$value )
cleanedTwoColumnPreciData2013 <- uncleanedTwoColumnPreciData2013
#anomaly Detection
augmented.week.info <- paste("2013", as.character(uncleanedTwoColumnPreciData2013$weeks), "0", sep = ".")
uncleanedTwoColumnPreciData2013$weeks <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))
anomaly.results <- AnomalyDetectionTs(uncleanedTwoColumnPreciData2013, 
                                      max_anoms = 0.01, direction = 'both', plot = TRUE, e_value = TRUE)
cleanedTwoColumnPreciData2013 <- uncleanedTwoColumnPreciData2013
if(nrow(anomaly.results$anoms) == 0) {
  uncleanedTwoColumnPreciData2013$Precipitation
} else {
  replace.index.list <- which(
    uncleanedTwoColumnPreciData2013$weeks == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
  cleanedTwoColumnPreciData2013$Precipitation = replace(uncleanedTwoColumnPreciData2013$Precipitation, replace.index.list, anomaly.results$anoms$expected_value)
}