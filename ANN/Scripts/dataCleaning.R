require(ggplot2)
require(data.table)

unCleanedDengueData2013 = fread("D:/Dengue 2.0/Data/Dengue/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))
cleanedDengueData2013 = fread("D:/Dengue 2.0/Data/Dengue/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))
unCleanedDengueData2014 = fread("D:/Dengue 2.0/Data/Dengue/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))
cleanedDengueData2014 = fread("D:/Dengue 2.0/Data/Dengue/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))

#Summery of the uncleaned and cleaned data
describe(unCleanedDengueData2013)
summary(unCleanedDengueData2013)

#exchange the raws and columns to annalyze data
for (row in c(1:NROW(unCleanedDengueData2013))) {
    unCleanedDengueData2013_rawvise= data.frame(row.names  = melt(unCleanedDengueData2013[row,][,3:54]$value))
  
}



# Histograms kollonawa -142
qplot(abs(diff(as.numeric(unCleanedDengueData2014[142,][3:54]))), geom = "histogram", binwidth=0.5) +
  ggtitle(paste("Histogram - ", unCleanedDengueData2014[142,]$MOH_name, " MOH Weekly Differences of Dengue cases")) +
  xlab("Week") +
  ylab("Count") +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank()) 
#.....................................................................#

total = 0
for(row in c(1:NROW(unCleanedDengueData2013))) {
  data = unCleanedDengueData2014[row,][3:54]
  data2 <- data
  thirdQuartile = as.numeric(quantile(abs(diff(x = as.numeric(data))))[4])
  
  for(i in c(2:52)) {
    if(abs(data[i] - data[i-1]) > thirdQuartile) {
      ifelse(data[i] > data[i-1], data2[i-1] <- data[i] - thirdQuartile, data2[i] <- data[i-1] - thirdQuartile)
    }
  }
  total = sum(data2)
  cleanedDengueData2013[row,][3:54] = data2
  cleanedDengueData2013[row,][55] = total
}



write.csv(x = cleanedDengueData2014, file = "E:/R_git/Pramod_ML/data/cleanedDengueData2014.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.csv(x = dengueData2013_2014, file = "E:/R_git/Pramod_ML/data/dengueData2013_2014.csv", sep = ",", row.names = FALSE, col.names = TRUE)
