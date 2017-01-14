currentMOH = data.frame(cases = cleanedTwoColumnData2013_2014$cases 
                       # temperature = cleanedTwoColumnTempData2013$Temprature,
                       # rainfall = (cleanedTwoColumnPreciData2013$Precipitation)
)
# On 2013_2014 data
currentMOH$lastWeekCases = c((currentMOH$cases[52] + currentMOH$cases[104])/2, currentMOH$cases[1:103])
currentMOH$casesLag2 = c((currentMOH$cases[51:52] + currentMOH$cases[103:104])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[50:52] + currentMOH$cases[102:104])/2, currentMOH$cases[1:101])
currentMOH$casesLag4 = c((currentMOH$cases[49:52] + currentMOH$cases[101:104])/2, currentMOH$cases[1:100])

#mobility values with model 1
currentMOH$LastweekMobility = c((MohAreawisemobility$mobility_value[52]),MohAreawisemobility$mobility_value[1:103])
currentMOH$MobilityLag2 = c((MohAreawisemobility$mobility_value[51:52]),MohAreawisemobility$mobility_value[1:102])
currentMOH$MobilityLag3 = c((MohAreawisemobility$mobility_value[50:52]),MohAreawisemobility$mobility_value[1:101])
currentMOH$MobilityLag4 = c((MohAreawisemobility$mobility_value[49:52]),MohAreawisemobility$mobility_value[1:100])
currentMOH$MobilityLag5 = c((MohAreawisemobility$mobility_value[48:52]),MohAreawisemobility$mobility_value[1:99])

#mobility values with model 2
sample= data.frame(weeks= 1:52)
MohAreawisemobilityModel2$mobility_value= c(MohAreawisemobilityModel2$mobility_value[45:48],MohAreawisemobilityModel2$mobility_value[1:48])
sample$mobility= c((MohAreawisemobilityModel2$mobility_value[45:48]),(MohAreawisemobilityModel2$mobility_value[1:48]))
sample$mobility= MohAreawisemobilityModel2$mobility_value

currentMOH$LastweekMobilityModel2 = c((MohAreawisemobility$mobility_value[52]),MohAreawisemobility$mobility_value[1:103])
currentMOH$MobilityLag2Model2 = c((MohAreawisemobility$mobility_value[51:52]),MohAreawisemobility$mobility_value[1:102])
currentMOH$MobilityLag3Model2 = c((MohAreawisemobility$mobility_value[50:52]),MohAreawisemobility$mobility_value[1:101])
currentMOH$MobilityLag4Model2 = c((MohAreawisemobility$mobility_value[49:52]),MohAreawisemobility$mobility_value[1:100])
currentMOH$MobilityLag5Model2 = c((MohAreawisemobility$mobility_value[48:52]),MohAreawisemobility$mobility_value[1:99])
