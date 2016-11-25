currentMOH = data.frame(cases = cleanedTwoColumnData2013_2014$cases, 
                        temperature = cleanedTwoColumnTempData2013$Temprature,
                        rainfall = (cleanedTwoColumnPreciData2013$Precipitation)
)
# On 2013_2014 data
currentMOH$lastWeekCases = c((currentMOH$cases[52] + currentMOH$cases[104])/2, currentMOH$cases[1:103])
currentMOH$casesLag2 = c((currentMOH$cases[51:52] + currentMOH$cases[103:104])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[50:52] + currentMOH$cases[102:104])/2, currentMOH$cases[1:101])
currentMOH$casesLag4 = c((currentMOH$cases[49:52] + currentMOH$cases[101:104])/2, currentMOH$cases[1:100])

#lags of the Moh area Dehiwala

currentMOH$dehiwalaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])
currentMOH$dehiwalaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])
currentMOH$dehiwalaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

#lags of the Moh area Wattala

currentMOH$WattalaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$WattalaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$WattalaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

#lags of the Moh area Kelaniya

currentMOH$kelaniyaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$kelaniyaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$kelaniyaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

#lags of the Moh area Kollonnawa

currentMOH$kollonnawaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$kollonnawaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$kollonnawaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

# Temprature Lags

currentMOH$tempLag1 = c(currentMOH$temperature[52], currentMOH$temperature[1:51])
currentMOH$tempLag2 = c(currentMOH$temperature[51:52], currentMOH$temperature[1:50])
currentMOH$tempLag3 = c(currentMOH$temperature[50:52],currentMOH$temperature[1:49])
currentMOH$tempLag4 = c(currentMOH$temperature[49:52],currentMOH$temperature[1:48])
currentMOH$tempLag5 = c(currentMOH$temperature[48:52],currentMOH$temperature[1:47])
currentMOH$tempLag6 = c(currentMOH$temperature[47:52],currentMOH$temperature[1:46])
currentMOH$tempLag7 = c(currentMOH$temperature[46:52],currentMOH$temperature[1:45])
currentMOH$tempLag8 = c(currentMOH$temperature[45:52],currentMOH$temperature[1:44])

# RainFall Lags

currentMOH$rainfallLag1 = c(currentMOH$rainfall[52], currentMOH$rainfall[1:51])
currentMOH$rainfallLag2 = c(currentMOH$rainfall[51:52], currentMOH$rainfall[1:50])
currentMOH$rainfallLag3 = c(currentMOH$rainfall[50:52],currentMOH$rainfall[1:49])
currentMOH$rainfallLag4 = c(currentMOH$rainfall[49:52],currentMOH$rainfall[1:48])
currentMOH$rainfallLag5 = c(currentMOH$rainfall[48:52],currentMOH$rainfall[1:47])
currentMOH$rainfallLag6 = c(currentMOH$rainfall[47:52],currentMOH$rainfall[1:46])
currentMOH$rainfallLag7 = c(currentMOH$rainfall[46:52],currentMOH$rainfall[1:45])
currentMOH$rainfallLag8 = c(currentMOH$rainfall[45:52],currentMOH$rainfall[1:44])
