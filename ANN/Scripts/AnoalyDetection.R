require(AnomalyDetection)
#data for AnomalyDetection

#specific Moh Area
area=142
uncleanedTwoColumnData2013= data.frame(weeks=1:52,cases =melt(unCleanedDengueData2013[area,][,3:54])$value )


uncleanedTwoColumnData2014= data.frame(weeks=1:52,cases =melt(unCleanedDengueData2014[area,][,3:54])$value )




#....................2013 Dengue Data-------------------------

augmented.week.info <- paste("2013", as.character(uncleanedTwoColumnData2013$weeks), "0", sep = ".")
uncleanedTwoColumnData2013$weeks <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))
anomaly.results <- AnomalyDetectionTs(uncleanedTwoColumnData2013, 
                                      max_anoms = 0.01, direction = 'both', plot = TRUE, e_value = TRUE)

cleanedTwoColumnData2013<- uncleanedTwoColumnData2013
if(nrow(anomaly.results$anoms) == 0) {
  uncleanedTwoColumnData2013$cases
} else {
  replace.index.list <- which(
    uncleanedTwoColumnData2013$weeks == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
  cleanedTwoColumnData2013$cases = replace(uncleanedTwoColumnData2013$cases, replace.index.list, anomaly.results$anoms$expected_value)
}
#....................2014 Dengue Data-------------------------

augmented.week.info <- paste("2014", as.character(uncleanedTwoColumnData2014$weeks), "0", sep = ".")
uncleanedTwoColumnData2014$weeks <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))
anomaly.results <- AnomalyDetectionTs(uncleanedTwoColumnData2014, 
                                      max_anoms = 0.01, direction = 'both', plot = TRUE, e_value = TRUE)

cleanedTwoColumnData2014<- uncleanedTwoColumnData2014
if(nrow(anomaly.results$anoms) == 0) {
  uncleanedTwoColumnData2014$cases
} else {
  replace.index.list <- which(
    uncleanedTwoColumnData2014$weeks == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
  cleanedTwoColumnData2014$cases = replace(uncleanedTwoColumnData2014$cases, replace.index.list, anomaly.results$anoms$expected_value)
}


#combining 2014 and 2013 Data

uncleanedTwoColumnData2013_2014 = merge(uncleanedTwoColumnData2013,uncleanedTwoColumnData2014,all=T)
cleanedTwoColumnData2013_2014<-uncleanedTwoColumnData2013_2014

anomaly.results <- AnomalyDetectionTs(uncleanedTwoColumnData2013_2014, 
                                      max_anoms = 0.01, direction = 'both', plot = TRUE, e_value = TRUE)

if(nrow(anomaly.results$anoms) == 0) {
  uncleanedTwoColumnData2013_2014$cases
} else {
  replace.index.list <- which(
    uncleanedTwoColumnData2013_2014$weeks == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
  cleanedTwoColumnData2013_2014$cases = replace(uncleanedTwoColumnData2013_2014$cases, replace.index.list, anomaly.results$anoms$expected_value)
}



