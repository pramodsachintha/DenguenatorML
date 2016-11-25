require(GGally)
require(corrgram)
require (Hmisc)
# Read all the data
#dengueData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))
dengueData2013 = fread("E:/R_git/Pramod_ML/data/cleanedDengueData2013.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:52), "Total"))
temperatureData2013 = fread("D:/Dengue 2.0/Data/Met_data/temp/temp.csv", data.table = F, header = T)
rainfallData2013 = fread("D:/Dengue 2.0/Data/rainfall2013.csv", data.table = F, header = T, col.names = c("MOH_name", c(1:52)))
write.csv(x = rainfallData2013, file = "rainfall2013.csv", sep = ",", row.names = FALSE, col.names = TRUE)

dengueData2014 = fread("E:/R_git/Pramod_ML/data/cleanedDengueData2014.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:52), "Total"))
#...................................................................#

# Histograms
qplot(abs(diff(as.numeric(dengueData2013[142,][3:54]))), geom = "histogram", binwidth=0.5) +
  ggtitle(paste("Histogram - ", dengueData2013[142,]$MOH_name, " MOH Weekly Differences of Dengue cases")) +
  xlab("Week") +
  ylab("Count") +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank()) 


# Combining 2013 and 2014
dengueData2013_2014 = data.frame(dengueData2013[1:54],dengueData2014[3:54])
colnames(dengueData2013_2014) <- c("id", "MOH_name", c(1:104))
dengueData2013_2014$Total = rowSums(dengueData2013_2014[3:106])

# Chose a specific MOH
area = 55
tempArea = 296
currentMOH = data.frame(cases = melt(dengueData2013_2014[area,][,3:106])$value, 
                        temperature = melt(temperatureData2013[tempArea,][,3:54])$value,
                        rainfall = melt(rainfallData2013[301,][,2:53])$value
)
#...................................................................#

# On 2013_2014 data
# Chose a specific MOH
currentMOH$lastWeekCases = c((currentMOH$cases[1] + currentMOH$cases[53])/2, currentMOH$cases[1:103])
currentMOH$casesLag2 = c((currentMOH$cases[2:1] + currentMOH$cases[54:53])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[3:1] + currentMOH$cases[55:53])/2, currentMOH$cases[1:101])
currentMOH$casesLag4 = c((currentMOH$cases[4:1] + currentMOH$cases[56:53])/2, currentMOH$cases[1:100])
currentMOH$colomboMOHLag1 = c(as.numeric(dengueData2013_2014[181,][,3] + dengueData2013_2014[181,][,55])/2, 
                                     as.numeric(dengueData2013_2014[181,][,3:105]))
currentMOH$colomboMOHLag2 = c(as.numeric(dengueData2013_2014[181,][,4:3] + dengueData2013_2014[181,][,56:55])/2, 
                                     melt(dengueData2013_2014[181,][,3:106])$value[1:102])
currentMOH$colomboMOHLag3 = c(as.numeric(dengueData2013_2014[181,][,5:3] + dengueData2013_2014[181,][,57:55])/2, 
                                     melt(dengueData2013_2014[181,][,3:106])$value[1:101])
currentMOH$colomboMOHLag4 = c(as.numeric(dengueData2013_2014[181,][,6:3] + dengueData2013_2014[181,][,58:55])/2, 
                                     melt(dengueData2013_2014[181,][,3:106])$value[1:100])
currentMOH$moratuwaMOHLag1 = c(as.numeric(dengueData2013_2014[200,][,3] + dengueData2013_2014[200,][,55])/2, 
                                      melt(dengueData2013_2014[200,][,3:106])$value[1:103])
currentMOH$moratuwaMOHLag2 = c(as.numeric(dengueData2013_2014[200,][,4:3] + dengueData2013_2014[200,][,56:55])/2, 
                                      melt(dengueData2013_2014[200,][,3:106])$value[1:102])
currentMOH$moratuwaMOHLag3 = c(as.numeric(dengueData2013_2014[200,][,5:3] + dengueData2013_2014[200,][,57:55])/2, 
                                      melt(dengueData2013_2014[200,][,3:106])$value[1:101])
currentMOH$moratuwaMOHLag4 = c(as.numeric(dengueData2013_2014[200,][,6:3] + dengueData2013_2014[200,][,58:55])/2, 
                                      melt(dengueData2013_2014[200,][,3:106])$value[1:100])


currentMOH$tempLag1 = c(currentMOH$temperature[52], currentMOH$temperature[1:51])
currentMOH$tempLag2 = c(currentMOH$temperature[51:52], currentMOH$temperature[1:50])
currentMOH$tempLag3 = c(currentMOH$temperature[50:52],currentMOH$temperature[1:49])
currentMOH$tempLag4 = c(currentMOH$temperature[49:52],currentMOH$temperature[1:48])
currentMOH$tempLag5 = c(currentMOH$temperature[48:52],currentMOH$temperature[1:47])
currentMOH$tempLag6 = c(currentMOH$temperature[47:52],currentMOH$temperature[1:46])
currentMOH$tempLag7 = c(currentMOH$temperature[46:52],currentMOH$temperature[1:45])
currentMOH$tempLag8 = c(currentMOH$temperature[45:52],currentMOH$temperature[1:44])


currentMOH$rainfallLag1 = c(currentMOH$rainfall[52], currentMOH$rainfall[1:51])
currentMOH$rainfallLag2 = c(currentMOH$rainfall[51:52], currentMOH$rainfall[1:50])
currentMOH$rainfallLag3 = c(currentMOH$rainfall[50:52],currentMOH$rainfall[1:49])
currentMOH$rainfallLag4 = c(currentMOH$rainfall[49:52],currentMOH$rainfall[1:48])
currentMOH$rainfallLag5 = c(currentMOH$rainfall[48:52],currentMOH$rainfall[1:47])
currentMOH$rainfallLag6 = c(currentMOH$rainfall[47:52],currentMOH$rainfall[1:46])
currentMOH$rainfallLag7 = c(currentMOH$rainfall[46:52],currentMOH$rainfall[1:45])
currentMOH$rainfallLag8 = c(currentMOH$rainfall[45:52],currentMOH$rainfall[1:44])
#...................................................................#

# Add columns
area = 55
tempArea = 296
currentMOH = data.frame(cases = melt(dengueData2013[area,][,3:54])$value, 
                        temperature = melt(temperatureData2013[tempArea,][,3:54])$value,
                        rainfall = melt(rainfallData2013[301,][,2:53])$value
)
#...................................................................#

# Add columns
currentMOH$lastWeekCases = c(dengueData2014[area,][3:54]$`52`, currentMOH$cases[1:51])
currentMOH$casesLag2[3:52] = c(currentMOH$cases[1:50])
currentMOH$casesLag3[4:52] = c(currentMOH$cases[1:49])
currentMOH$casesLag4[5:52] = c(currentMOH$cases[1:48])
currentMOH$colomboMOHLag1[2:52] = c(melt(dengueData2013[181,][,3:54])$value[1:51])
currentMOH$colomboMOHLag2[3:52] = c(melt(dengueData2013[181,][,3:54])$value[1:50])
currentMOH$colomboMOHLag3[4:52] = c(melt(dengueData2013[181,][,3:54])$value[1:49])
currentMOH$colomboMOHLag4[5:52] = c(melt(dengueData2013[181,][,3:54])$value[1:48])
currentMOH$moratuwaMOHLag1[2:52] = c(melt(dengueData2013[200,][,3:54])$value[1:51])
currentMOH$moratuwaMOHLag2[3:52] = c(melt(dengueData2013[200,][,3:54])$value[1:50])
currentMOH$moratuwaMOHLag3[4:52] = c(melt(dengueData2013[200,][,3:54])$value[1:49])
currentMOH$moratuwaMOHLag4[5:52] = c(melt(dengueData2013[200,][,3:54])$value[1:48])


currentMOH$tempLag1 = c(currentMOH$temperature[52], currentMOH$temperature[1:51])
currentMOH$tempLag2 = c(currentMOH$temperature[51:52], currentMOH$temperature[1:50])
currentMOH$tempLag3 = c(currentMOH$temperature[50:52],currentMOH$temperature[1:49])
currentMOH$tempLag4 = c(currentMOH$temperature[49:52],currentMOH$temperature[1:48])
currentMOH$tempLag5 = c(currentMOH$temperature[48:52],currentMOH$temperature[1:47])
currentMOH$tempLag6 = c(currentMOH$temperature[47:52],currentMOH$temperature[1:46])
currentMOH$tempLag7 = c(currentMOH$temperature[46:52],currentMOH$temperature[1:45])
currentMOH$tempLag8 = c(currentMOH$temperature[45:52],currentMOH$temperature[1:44])

for(i in 1:ncol(currentMOH)){
  currentMOH[is.na(currentMOH[,i]), i] <- mean(currentMOH[,i], na.rm = TRUE)
}
#...................................................................#



ggcorr(currentMOH, geom = "tile", min_size = 15, max_size = 30, label = F, label_round = 2) +
  ggtitle("Variable Correlation - Colombo MOH")
#.........................correlation of the data set.........................#
corrgram(currentMOH)
correlations <- rcorr(as.matrix(currentMOH))
correlations$P
