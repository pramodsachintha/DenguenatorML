currentMOH = data.frame(cases = cleanedTwoColumnData2012_2013$cases, 
                        temperature = cleanedTwoColumnTempData2013$Temprature,
                        rainfall = (cleanedTwoColumnPreciData2013$Precipitation)
)

Actualvalues2014 = data.frame(cases = cleanedTwoColumnData2014$cases, 
                              temperature = cleanedTwoColumnTempData2013$Temprature,
                              rainfall = (cleanedTwoColumnPreciData2013$Precipitation)
)
# On 2013_2014 data
currentMOH$lastWeekCases = c((currentMOH$cases[52] + currentMOH$cases[104])/2, currentMOH$cases[1:103])
currentMOH$casesLag2 = c((currentMOH$cases[51:52] + currentMOH$cases[103:104])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[50:52] + currentMOH$cases[102:104])/2, currentMOH$cases[1:101])
currentMOH$casesLag4 = c((currentMOH$cases[49:52] + currentMOH$cases[101:104])/2, currentMOH$cases[1:100])
currentMOH$casesLag5 = c((currentMOH$cases[48:52] + currentMOH$cases[100:104])/2, currentMOH$cases[1:99])
currentMOH$casesLag6 = c((currentMOH$cases[49:52] + currentMOH$cases[99:104])/2, currentMOH$cases[1:98])

#actual value lags 2014
Actualvalues2014$lastWeekCases = c(Actualvalues2014$cases[52], Actualvalues2014$cases[1:51])
Actualvalues2014$casesLag2 = c(Actualvalues2014$cases[51:52], Actualvalues2014$cases[1:50])
Actualvalues2014$casesLag3 = c(Actualvalues2014$cases[50:52], Actualvalues2014$cases[1:49])
Actualvalues2014$casesLag4 = c(Actualvalues2014$cases[49:52], Actualvalues2014$cases[1:48])
Actualvalues2014$casesLag5 = c(Actualvalues2014$cases[48:52], Actualvalues2014$cases[1:47])
Actualvalues2014$casesLag6 = c(Actualvalues2014$cases[47:52], Actualvalues2014$cases[1:46])

#mobility values with model 1

currentMOH$LastweekMobility = c(MohAreawisemobility$mobility_value[52],MohAreawisemobility$mobility_value[1:103])
currentMOH$MobilityLag2 = c((MohAreawisemobility$mobility_value[51:52]),MohAreawisemobility$mobility_value[1:102])
currentMOH$MobilityLag3 = c((MohAreawisemobility$mobility_value[50:52]),MohAreawisemobility$mobility_value[1:101])
currentMOH$MobilityLag4 = c((MohAreawisemobility$mobility_value[49:52]),MohAreawisemobility$mobility_value[1:100])
currentMOH$MobilityLag5 = c((MohAreawisemobility$mobility_value[48:52]),MohAreawisemobility$mobility_value[1:99])

Actualvalues2014$LastweekMobility = c(MohAreawisemobility$mobility_value[52],MohAreawisemobility$mobility_value[1:51])
Actualvalues2014$MobilityLag2 = c((MohAreawisemobility$mobility_value[51:52]),MohAreawisemobility$mobility_value[1:50])
Actualvalues2014$MobilityLag3 = c((MohAreawisemobility$mobility_value[50:52]),MohAreawisemobility$mobility_value[1:49])
Actualvalues2014$MobilityLag4 = c((MohAreawisemobility$mobility_value[49:52]),MohAreawisemobility$mobility_value[1:48])
Actualvalues2014$MobilityLag5 = c((MohAreawisemobility$mobility_value[48:52]),MohAreawisemobility$mobility_value[1:47])

#mobility values with model 2
model2mobility= data.frame(weeks= 1:104)
#MohAreawisemobilityModel2$mobility_value= c(MohAreawisemobilityModel2$mobility_value[45:48],MohAreawisemobilityModel2$mobility_value[1:48])
model2mobility$mobility= c((MohAreawisemobilityModel2$mobility_value[45:48]),(MohAreawisemobilityModel2$mobility_value[1:48]))
#sample$mobility= MohAreawisemobilityModel2$mobility_value

currentMOH$LastweekMobilityModel2 = c((model2mobility$mobility[52]),model2mobility$mobility[1:103])
currentMOH$MobilityLag2Model2 = c((model2mobility$mobility[51:52]),model2mobility$mobility[1:102])
currentMOH$MobilityLag3Model2 = c((model2mobility$mobility[50:52]),model2mobility$mobility[1:101])
currentMOH$MobilityLag4Model2 = c((model2mobility$mobility[49:52]),model2mobility$mobility[1:100])
currentMOH$MobilityLag5Model2 = c((model2mobility$mobility[48:52]),model2mobility$mobility[1:99])

#mobility trips


#temprature data
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

#actual alues 2014 lag
Actualvalues2014$tempLag1 = c(Actualvalues2014$temperature[52], Actualvalues2014$temperature[1:51])
Actualvalues2014$tempLag2 = c(Actualvalues2014$temperature[51:52], Actualvalues2014$temperature[1:50])
Actualvalues2014$tempLag3 = c(Actualvalues2014$temperature[50:52],Actualvalues2014$temperature[1:49])
Actualvalues2014$tempLag4 = c(Actualvalues2014$temperature[49:52],Actualvalues2014$temperature[1:48])
Actualvalues2014$tempLag5 = c(Actualvalues2014$temperature[48:52],Actualvalues2014$temperature[1:47])
Actualvalues2014$tempLag6 = c(Actualvalues2014$temperature[47:52],Actualvalues2014$temperature[1:46])
Actualvalues2014$tempLag7 = c(Actualvalues2014$temperature[46:52],Actualvalues2014$temperature[1:45])
Actualvalues2014$tempLag8 = c(Actualvalues2014$temperature[45:52],Actualvalues2014$temperature[1:44])

# RainFall Lags

Actualvalues2014$rainfallLag1 = c(Actualvalues2014$rainfall[52], Actualvalues2014$rainfall[1:51])
Actualvalues2014$rainfallLag2 = c(Actualvalues2014$rainfall[51:52], Actualvalues2014$rainfall[1:50])
Actualvalues2014$rainfallLag3 = c(Actualvalues2014$rainfall[50:52],Actualvalues2014$rainfall[1:49])
Actualvalues2014$rainfallLag4 = c(Actualvalues2014$rainfall[49:52],Actualvalues2014$rainfall[1:48])
Actualvalues2014$rainfallLag5 = c(Actualvalues2014$rainfall[48:52],Actualvalues2014$rainfall[1:47])
Actualvalues2014$rainfallLag6 = c(Actualvalues2014$rainfall[47:52],Actualvalues2014$rainfall[1:46])
Actualvalues2014$rainfallLag7 = c(Actualvalues2014$rainfall[46:52],Actualvalues2014$rainfall[1:45])
Actualvalues2014$rainfallLag8 = c(Actualvalues2014$rainfall[45:52],Actualvalues2014$rainfall[1:44])

#writing current moh to file
write.csv(x = currentMOH, file = "C:/Users/Prasanna Dassanayake/Desktop/CaseLags456/Colombo/currentmoh.csv", sep = ",", row.names = FALSE, col.names = TRUE)
#actual cases
write.csv(x = Actualvalues2014, file = "C:/Users/Prasanna Dassanayake/Desktop/CaseLags456/Colombo/actualcases.csv", sep = ",", row.names = FALSE, col.names = TRUE)
