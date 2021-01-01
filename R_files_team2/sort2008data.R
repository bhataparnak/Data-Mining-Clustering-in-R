library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)

txw_2008  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/hourly_2008.csv",
                      header =  FALSE, sep = "",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
txw_2008 <- slice(txw_2008, -c(1))
colnames(txw_2008)=c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP",
                     "STP","Count_STP","Visib","Count_Visib","WDSP","Count_WDSP","MXSDP","Gust","PRCP",
                     "SNDP","FRSHIFT")
txw_2008[20:23] <- list(NULL)

trial_df_2008<- tbl_df(txw_2008)
length(unique(trial_df_2008$STN))
df_2008 <- sqldf("SELECT DISTINCT trial_df_2008.* from trial_df_2008 where yearModa_hr like '200802%' ")
df_2008<-df_2008[!duplicated(df_2008[1]),]



write.csv(df_2008,"F:/UTA/Courses/Assignments/Data Mining/Project2/feb2008.csv", row.names = TRUE)