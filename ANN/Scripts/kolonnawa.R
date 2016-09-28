area = 142
tempArea = 168
currentMOH = data.frame(cases = melt(dengueData2013[area,][,3:54])$value, 
                        temperature = melt(temperatureData2013[tempArea,][,3:54])$value
#                        rainfall = melt(rainfallData2013[301,][,2:53])$value
                        #dehiwala = melt(dengueData2013[55,][,3:54])$value, 
                        #colomboMOH = melt(dengueData2013[181,][,3:54])$value, 
                        #                        kolonnawa = melt(dengueData2013[142,][,3:54])$value, 
                        #                       kaduwela = melt(dengueData2013[111,][,3:54])$value, 
                        #moratuwaMOH = melt(dengueData2013[200,][,3:54])$value
                        #                      kaduwela = melt(dengueData2013[235,][,3:54])$value
)
#...................................................................#

# Add columns
currentMOH$lastWeekCases = c(dengueData2014[area,][3:54]$`52`, currentMOH$cases[1:51])
currentMOH$casesLag2[3:52] = c(currentMOH$cases[1:50])
currentMOH$casesLag3[4:52] = c(currentMOH$cases[1:49])
currentMOH$casesLag4[5:52] = c(currentMOH$cases[1:48])
#currentMOH$kaduwelaMOHLag1[2:52] = c(melt(dengueData2013[111,][,3:54])$value[1:51])
#currentMOH$kaduwelaMOHLag2[3:52] = c(melt(dengueData2013[111,][,3:54])$value[1:50])
#currentMOH$kaduwelaMOHLag3[4:52] = c(melt(dengueData2013[111,][,3:54])$value[1:49])
#currentMOH$kaduwelaMOHLag4[5:52] = c(melt(dengueData2013[111,][,3:54])$value[1:48])
currentMOH$colomboMOHLag1[2:52] = c(melt(dengueData2013[181,][,3:54])$value[1:51])
currentMOH$colomboMOHLag2[3:52] = c(melt(dengueData2013[181,][,3:54])$value[1:50])
currentMOH$colomboMOHLag3[4:52] = c(melt(dengueData2013[181,][,3:54])$value[1:49])
currentMOH$colomboMOHLag4[5:52] = c(melt(dengueData2013[181,][,3:54])$value[1:48])


currentMOH$tempLag1[2:52] = c(currentMOH$temperature[1:51])
#currentMOH$tempLag2[3:52] = c(currentMOH$temperature[1:50])
currentMOH$tempLag3[4:52] = c(currentMOH$temperature[1:49])
currentMOH$tempLag4[5:52] = c(currentMOH$temperature[1:48])
#currentMOH$tempLag5[6:52] = c(currentMOH$temperature[1:47])
#currentMOH$tempLag6[7:52] = c(currentMOH$temperature[1:46])
#currentMOH$tempLag7[8:52] = c(currentMOH$temperature[1:45])
#currentMOH$tempLag8[9:52] = c(currentMOH$temperature[1:44])

for(i in 1:ncol(currentMOH)){
  currentMOH[is.na(currentMOH[,i]), i] <- mean(currentMOH[,i], na.rm = TRUE)
}
#...................................................................#

#Visualizing Data
data = data.frame(week = c(1:52), actualCases = as.numeric(unCleanedDengueData2013[area,][3:54]), cleanedCases = as.numeric(cleanedDengueData2013[area,][3:54]))
dmelt = melt(data, id = "week")

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  scale_fill_discrete(name="",
                      breaks=c("actualCases", "cleanedCases"),
                      labels=c("Actual Data", "Cleaned Data")) +
  ggtitle("Dengue Incidences 2013 - Kolonnawa MOH")

