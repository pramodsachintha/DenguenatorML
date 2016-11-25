require(ggplot2)
require(tidyr)
require(dplyr)
require(AnomalyDetection)
require(reshape2)

getMohData <- function(x) {
  moh_data <- data.frame(rep(x[1],52), rep(x[2],52), c(1:52), as.numeric(x[3:54]))
  names(moh_data) <- c('moh_id','moh','week','cases')  
  return(moh_data)
}

getTempData <- function(x) {
  temp_data <- data.frame(rep(x[1],52), c(1:52), as.numeric(x[3:54]))
  names(temp_data) <- c('moh','week','temp')
  return(temp_data)
}

getWeatherData <- function(x) {
  weather_data <- data.frame(rep(x[1],52), c(1:52), as.numeric(x[2:53]))
  names(weather_data) <- c('moh','week','precip')  
  return(weather_data)
}

GetDataNWeeksBack <- function(row.vector, src.data.set, column.idx, n) {
  target.week <- as.integer(row.vector['week']) - n
  if(target.week <= 0) {
    return(0)
  } else {
    target.moh <- row.vector['moh']
    targe.row.vector <- src.data.set[src.data.set['week'] == target.week & src.data.set['moh'] == target.moh,]
    return(targe.row.vector[,column.idx])
  }
}

preprocess.dengue.data <- FALSE

if(preprocess.dengue.data) {
  dengue.cases.2013 <- read.csv("~/data/lirneasia/dengue-data/Dengue/dengueCases2013.csv", header=FALSE)
  mohDataList <- apply(dengue.cases.2013,1,getMohData)
  mohData <- do.call('rbind',mohDataList)
  
  temp <- read.csv("~/data/lirneasia/dengue-data/Met_data/temp/temp.csv")
  tempDataList <- apply(temp,1,getTempData)
  tempData <- do.call('rbind',tempDataList)
  
  mohWeather <- read.csv("~/data/lirneasia/dengue-data/Met_data/corrected_weather/mohWeather.csv")
  precipDataList <- apply(mohWeather, 1, getWeatherData)
  precip.data <- do.call('rbind',precipDataList)
  
  temperatue.and.case.data <- left_join(mohData, tempData, by = c('moh','week'))
  full.data <- left_join(temperatue.and.case.data, precip.data, by = c('moh','week'))
  missingTempData <- subset(full.data, is.na(full.data$temp))
  missingPrecipData <- subset(full.data, is.na(full.data$precip))
  
  full.data.without.na <- na.omit(full.data)
  full.data.without.na$moh_id <- NULL
  # Calculate lagged dengue cases
  prev.week.cases <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 3, 1)
  two.weeks.back.cases <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 3, 2)
  three.weeks.back.cases <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 3, 3)
  four.weeks.back.cases <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 3, 4)
  # Calculate lagged temperature
  one.week.back.temperature <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 4, 1)
  two.weeks.back.temperature <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 4, 2)
  three.weeks.back.temperature <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 4, 3)
  four.weeks.back.temperature <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 4, 4)
  # Calculate lagged precipitation
  one.week.back.precip <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 5, 1)
  two.weeks.back.precip <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 5, 2)
  three.weeks.back.precip <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 5, 3)
  four.weeks.back.precip <- apply(full.data.without.na,1, GetDataNWeeksBack, full.data.without.na, 5, 4)
  
  full.data.without.na$one.week.lag.cases <- prev.week.cases
  full.data.without.na$two.week.lag.cases <- two.weeks.back.cases
  full.data.without.na$three.week.lag.cases <- three.weeks.back.cases
  full.data.without.na$four.week.lag.cases <- four.weeks.back.cases
  full.data.without.na$one.week.lag.temperature <- one.week.back.temperature
  full.data.without.na$two.week.lag.temperature <- two.weeks.back.temperature
  full.data.without.na$three.week.lag.temperature <- three.weeks.back.temperature
  full.data.without.na$four.week.lag.temperature <- four.weeks.back.temperature
  full.data.without.na$one.week.lag.precip <- one.week.back.precip
  full.data.without.na$two.week.lag.precip <- two.weeks.back.precip
  full.data.without.na$three.week.lag.precip <- three.weeks.back.precip
  full.data.without.na$four.week.lag.precip <- four.weeks.back.precip
  
  saveRDS(full.data.without.na, "~/Dengue-Current-Work/dengue-preprocessed-dataset.rds")
}

full.data.without.na <- readRDS("~/Dengue-Current-Work/dengue-preprocessed-dataset.rds")

moh.mobility.ten.days <- read.csv("~/data/lirneasia/dengue-data/Mobility/moh-mobility-jan20-29.csv")
moh.mobility.week <- filter(moh.mobility.ten.days, DATE < 20130127)
summarized.moh.mobility <- moh.mobility.week %>% group_by(MOH_ID) %>% summarise(MOH_MOB_VAL = mean(MOH_PERCENTAGE))
moh.location.raw <- read.csv("~/data/lirneasia/dengue-data/MOH/mohLocation.csv")
names(moh.location.raw)[1] <- 'MOH_ID'
moh.location.raw$X <- NULL
augmented.moh.mobility <- inner_join(moh.location.raw, summarized.moh.mobility, by = "MOH_ID")
names(augmented.moh.mobility) <- c('moh_id','moh','lat','lon','moh_mob_val')
levels(augmented.moh.mobility$moh)[levels(augmented.moh.mobility$moh) == 'Homagama '] <- 'Homagama'

ReplaceAnomalousValues <- function(moh.dataframe) {
  augmented.week.info <- paste("2013", as.character(moh.dataframe$week), "0", sep = ".")
  moh.dataframe$week <- as.POSIXct(as.Date(strptime(augmented.week.info, format = "%Y.%U.%w")))
  anomaly.results <- AnomalyDetectionTs(moh.dataframe, 
                                        max_anoms = 0.01, direction = 'both', plot = F, e_value = TRUE)
  if(nrow(anomaly.results$anoms) == 0) {
    moh.dataframe$cases
  } else {
    replace.index.list <- which(
      moh.dataframe$week == paste(anomaly.results$anoms$timestamp,"IST", sep = " "))
    replace(moh.dataframe$cases, replace.index.list, anomaly.results$anoms$expected_value)
  }
}

corrected.dengue.data <- full.data.without.na
corrected.dengue.data$moh <- as.factor(corrected.dengue.data$moh)
corrected.dengue.data$corrected.cases <- rep(0,nrow(corrected.dengue.data))
correction.process.output <- sapply(levels(corrected.dengue.data$moh), 
                                    function(x) corrected.dengue.data[corrected.dengue.data$moh == x, 'corrected.cases'] <<- 
                                      ReplaceAnomalousValues(corrected.dengue.data[corrected.dengue.data$moh == x,2:3]))

data.moh.levels <- levels(corrected.dengue.data$moh)
filtered.augmented.moh.mobility <- filter(augmented.moh.mobility, moh %in% data.moh.levels)
filtered.augmented.moh.mobility$moh <- factor(filtered.augmented.moh.mobility$moh)
filtered.corrected.dengue.data <- 
  filter(corrected.dengue.data, moh %in% levels(filtered.augmented.moh.mobility$moh))
corrected.dengue.data.with.mobility <- inner_join(filtered.corrected.dengue.data, filtered.augmented.moh.mobility[,c(2,5)], by = 'moh')
corrected.dengue.data.with.mobility$moh <- factor(corrected.dengue.data.with.mobility$moh)

selected.moh <- 'Badulla'
corrected.singlemoh.data <- corrected.dengue.data.with.mobility[corrected.dengue.data.with.mobility$moh == selected.moh, c(2,3,18)]
reshaped.singlemoh.data <- melt(corrected.singlemoh.data, id.vars = 'week')
g.cleaned <- ggplot(reshaped.singlemoh.data, aes(x=week, y=value)) + 
  ggtitle(paste("Corrected vs Reported Cases (", selected.moh, ")")) +
  xlab("Week of year (2013)") + ylab("Dengue Cases")
g.cleaned + geom_line(aes(color = variable, group = variable)) +
  geom_point(aes(color = variable, group = variable, shape = variable))

saveRDS(corrected.dengue.data, "~/Dengue-Current-Work/dengue-preprocessed-corrected-dataset.rds")
saveRDS(corrected.dengue.data.with.mobility, "~/Dengue-Current-Work/dengue-preprocessed-corrected-dataset-w-mobility.rds")