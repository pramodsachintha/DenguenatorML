VegetationIndexMonthly = fread("D:/Data/vegtationIndeices.csv", data.table = F, header = T)
require(graphics)

x<- c(VegetationIndexMonthly$`2013_1`[1],VegetationIndexMonthly$`2013_2`[1])

y<-VegetationIndexMonthly[1,][,2]



for(i in 1:nrow(VegetationIndexMonthly)){
  for(j in 2:ncol(VegetationIndexMonthly)){
   x= c(VegetationIndexMonthly[i,][,j])
   
   vegetationIndextemp=data.frame(points=(spline(x, y = NULL, n = 4, method = "fmm",
                        xmin = min(x), xmax = max(x), ties = mean)$x))
   
  }
}

vegetationIndexWeekly= data.frame(MOH_Name=VegetationIndexMonthly$MOH_Area)
vegetationIndextemp=vegetationIndexWeekly

x= c(VegetationIndexMonthly[2,][,2],VegetationIndexMonthly[2,][,2+1])


#time series conversion

moh_area= "MC - Colombo"

MohAreawisemobilityModel2= data.frame(weeks=1:48,mobility_value =mobilitydata2013Model2[mobilitydata2013Model2$`moh name`==mobilityArea2,]$MOBILITY_VALUE)

MOHAreaWiseVegetationIndexMonthly= data.frame(Months= 1:12, Vegetation_index = melt(VegetationIndexMonthly[VegetationIndexMonthly$MOH_Area==moh_area,][2:13]))
MOHAreaWiseVegetationIndexMonthly=within.data.frame(MOHAreaWiseVegetationIndexMonthly,rm(Vegetation_index.variable))

augmented.week.info <- paste("2013", as.character(MOHAreaWiseVegetationIndexMonthly$Months), "0", sep = ".")
MOHAreaWiseVegetationIndexMonthly$Months <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))


