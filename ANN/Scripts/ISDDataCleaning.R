unCleanedISDData = fread("D:/Data/ISD-Weather/SL-2006-2016/9009387163514dat.txt", data.table = F, header = F)

read_cols <- function(file_name, colsToKeep) {
  header <- fread(file_name, nrows = 1, header = FALSE)
  all_in_header <- all(colsToKeep %chin% unlist(header))
  stopifnot(all_in_header)
  
  fread(file_name, header=TRUE, select=colsToKeep, verbose=TRUE)
}

my_data <- read_cols("D:/Data/ISD-Weather/SL-2006-2016/9009387163514dat.txt", c(" USAF","YR--MODAHRMN","TEMP"))

unCleanedISDData = read.table("D:/Data/ISD-Weather/SL-2006-2016/9009387163514dat.txt", 
               sep=" ", 
               header = F,
               fill=T, 
               strip.white=FALSE)