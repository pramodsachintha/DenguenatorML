require(AnomalyDetection)
#data for AnomalyDetection

area=166
uncleanedTwoColumnData2013= data.frame(weeks=1:52,cases =melt(unCleanedDengueData2013[area,][,3:54])$value )


uncleanedTwoColumnData2014= data.frame(weeks=1:52,cases =melt(unCleanedDengueData2014[area,][,3:54])$value )



#data in single data frame
uncleanedTwoColumnData2013= data.frame(cases =melt(unCleanedDengueData2013[area,][,3:54])$value )

#as a vector
AnomalyDetectionVec(uncleanedTwoColumnData2013, max_anoms=0.02, period=52, direction='both', only_last=FALSE, plot=TRUE)
#time Series
AnomalyDetectionTs(uncleanedTwoColumnData2013, max_anoms=0.02, direction='both', only_last='day', plot=TRUE)

# converting weeks in to time stamps
#.............................................
uncleanedTwoColumnData2013[1,][,1] 
as.POSIXct(uncleanedTwoColumnData2013$weeks,format="01-01-2013 8:00", origin="01-01-1970 8:00")
uncleanedTwoCoumnTimestampedData2013<-uncleanedTwoColumnData2013
uncleanedTwoCoumnTimestampedData2013$weeks<-as.Date(uncleanedTwoColumnData2013$weeks,origin="01-01-1970 8:00")

#....................2013-------------------------

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
#....................2014-------------------------

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

# Cleaned data set Integration

