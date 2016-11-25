unCleanedDengueData2013[which(unCleanedDengueData2013$MOH_name=="MC - Colombo"),]
which(unCleanedDengueData2013$MOH_name=="Kollonnawa")
which(temperatureData2013$MOH_name=="Boralesgamuwa")
which(rainfallData2013$MOH_name=="Boralesgamuwa")


#365 date to week coversion
augmented.week.info <- paste("2014", as.character(uncleanedTwoColumnData2014$weeks), "0", sep = ".")
uncleanedTwoColumnData2014$weeks <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))

mydf$Dt <- as.Date(mydf$date, format="%d-%m-%Y")
 weeknum <- as.numeric( format(mydf$Dt+3, "%U"))
 weeknum
 dateweeks
 as.POSIXct(dateweeks$weeks,format="01-01-2013 8:00", origin="01-01-1970 8:00")
 uncleanedTwoCoumnTimestampedData2013<-uncleanedTwoColumnData2013
 uncleanedTwoCoumnTimestampedData2013$weeks<-as.Date(uncleanedTwoColumnData2013$weeks,origin="01-01-1970 8:00")
 dateweeks$days=1:365
 dateweeks$days <- as.Date(dateweeks$days,origin="01-01-1970 8:00")