currentMOH = data.frame(cases = cleanedTwoColumnData2013_2014$cases, 
                        temperature = cleanedTwoColumnTempData2013$Temprature,
                        rainfall = (cleanedTwoColumnPreciData2013$Precipitation)
)
# On 2013_2014 data
currentMOH$lastWeekCases = c((currentMOH$cases[52] + currentMOH$cases[104])/2, currentMOH$cases[1:103])
currentMOH$casesLag2 = c((currentMOH$cases[51:52] + currentMOH$cases[103:104])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[50:52] + currentMOH$cases[102:104])/2, currentMOH$cases[1:101])
currentMOH$casesLag4 = c((currentMOH$cases[49:52] + currentMOH$cases[101:104])/2, currentMOH$cases[1:100])

#mobility trip filter
currentMOH$LastweekMobility = c(MohAreawisemobility$mobility_value[52],MohAreawisemobility$mobility_value[1:103])
currentMOH$MobilityLag2 = c((MohAreawisemobility$mobility_value[51:52]),MohAreawisemobility$mobility_value[1:102])
currentMOH$MobilityLag3 = c((MohAreawisemobility$mobility_value[50:52]),MohAreawisemobility$mobility_value[1:101])
currentMOH$MobilityLag4 = c((MohAreawisemobility$mobility_value[49:52]),MohAreawisemobility$mobility_value[1:100])
currentMOH$MobilityLag5 = c((MohAreawisemobility$mobility_value[48:52]),MohAreawisemobility$mobility_value[1:99])

#mobility trips of top 10 MOH areas
#mobility filter for destination COLOMBO with source DEHIWALA
DestinationMohArea="CMC"
SourceMohArea="DEHIWALA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromDehiwala = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromDehiwala = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromDehiwala = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromDehiwala = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromDehiwala = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination COLOMBO with source Nugegoda
DestinationMohArea="CMC"
SourceMohArea="NUGEGODA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromNugegoda = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromNugegoda= c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromNugegoda = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromNugegoda = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromNugegoda = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination COLOMBO with source KOLLONAWA
DestinationMohArea="CMC"
SourceMohArea="KOLONNAWA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromKolonnawa = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromKolonnawa = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromKolonnawa = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromKolonnawa = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromKolonnawa = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination dehiwala with source Piliyandala
DestinationMohArea="DEHIWALA"
SourceMohArea="PILIYANDALA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromPiliyandala = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])


#cases Dehiwals
currentMOH$DehiwalaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])
currentMOH$DehiwalaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])
currentMOH$DehiwalaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

#cases Rathmalana

#cases Nugegoda
currentMOH$NugegodaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$NugegodaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$NugegodaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])


#cases kolonnawa
currentMOH$KolonnawaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$KolonnawaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$KolonnawaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])
