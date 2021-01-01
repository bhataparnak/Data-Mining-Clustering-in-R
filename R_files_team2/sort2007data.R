library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)

txw_2007  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/hourly_2007.csv",
                      header =  FALSE, sep = "",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
txw_2007 <- slice(txw_2007, -c(1))
colnames(txw_2007)=c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP",
                     "STP","Count_STP","Visib","Count_Visib","WDSP","Count_WDSP","MXSDP","Gust","PRCP",
                     "SNDP","FRSHIFT")

trial_df_2007<- tbl_df(txw_2007)
length(unique(trial_df_2007$STN))
df_2007 <- sqldf("SELECT DISTINCT trial_df_2007.* from trial_df_2007 where yearModa_hr like '200702%' ")
df_2007<-df_2007[!duplicated(df_2007[1]),]

write.csv(df_2007,"F:/UTA/Courses/Assignments/Data Mining/Project2/feb2007.csv", row.names = TRUE)