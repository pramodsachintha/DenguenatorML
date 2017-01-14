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
#mobility filter for destination dehiwala with source colombo
DestinationMohArea="DEHIWALA"
SourceMohArea="CMC"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromCMC = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromCMC = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromCMC = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromCMC = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromCMC = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination dehiwala with source Rathmalana
DestinationMohArea="DEHIWALA"
SourceMohArea="RATHMALANA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromRathmalana = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromRathmalana = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromRathmalana = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromRathmalana = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromRathmalana = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination dehiwala with source Boralesgamuwa
DestinationMohArea="DEHIWALA"
SourceMohArea="BORALESGAMUWA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromBoralesgamuwa = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromBoralesgamuwa = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromBoralesgamuwa = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromBoralesgamuwa = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromBoralesgamuwa = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])

#mobility filter for destination dehiwala with source Piliyandala
DestinationMohArea="DEHIWALA"
SourceMohArea="PILIYANDALA"
MohAreawisemobilityModelTripsData= data.frame(weeks=1:104,mobility_value =mobilitydata2013Trip[mobilitydata2013Trip$`Destination MOH`==DestinationMohArea & mobilitydata2013Trip$`Source MOH`==SourceMohArea,,]$`Mobility Value`)

currentMOH$LastweekMobilityFromPiliyandala = c(MohAreawisemobilityModelTripsData$mobility_value[52],MohAreawisemobilityModelTripsData$mobility_value[1:103])
currentMOH$MobilityLag2FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[51:52]),MohAreawisemobilityModelTripsData$mobility_value[1:102])
currentMOH$MobilityLag3FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[50:52]),MohAreawisemobilityModelTripsData$mobility_value[1:101])
currentMOH$MobilityLag4FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[49:52]),MohAreawisemobilityModelTripsData$mobility_value[1:100])
currentMOH$MobilityLag5FromPiliyandala = c((MohAreawisemobilityModelTripsData$mobility_value[48:52]),MohAreawisemobilityModelTripsData$mobility_value[1:99])


#cases Colombo
currentMOH$colomboMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])
currentMOH$colomboMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])
currentMOH$colomboMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])

#cases Rathmalana

#cases Boralesgamuwa
currentMOH$boralesgamuwaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$boralesgamuwaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$boralesgamuwaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])


#cases Piliyandala
currentMOH$PiliyandalaMOHLag1=c((cleanedTwoColumnData2013_2014$cases[52]+cleanedTwoColumnData2013_2014$cases[104])/2,cleanedTwoColumnData2013_2014$cases[1:103])

currentMOH$PiliyandalaMOHLag2=c((cleanedTwoColumnData2013_2014$cases[51:52]+cleanedTwoColumnData2013_2014$cases[103:104])/2,cleanedTwoColumnData2013_2014$cases[1:102])

currentMOH$PiliyandalaMOHLag3=c((cleanedTwoColumnData2013_2014$cases[50:52]+cleanedTwoColumnData2013_2014$cases[102:104])/2,cleanedTwoColumnData2013_2014$cases[1:101])
