library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)


txw_2009  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/hourly_2009.csv",
                      header =  FALSE, sep = "",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
txw_2009 <- slice(txw_2009, -c(1))
colnames(txw_2009)=c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP",
                     "STP","Count_STP","Visib","Count_Visib","WDSP","Count_WDSP","MXSDP","Gust","PRCP",
                     "SNDP","FRSHIFT")
txw_2009[20:23] <- list(NULL)

trial_df_2009<- tbl_df(txw_2009)
length(unique(trial_df_2009$STN))
df_2009 <- sqldf("SELECT DISTINCT trial_df_2009.* from trial_df_2009 where yearModa_hr like '200902%' ")
df_2009<-df_2009[!duplicated(df_2009[1]),]

write.csv(df_2009,"F:/UTA/Courses/Assignments/Data Mining/Project2/feb2009.csv", row.names = TRUE)
