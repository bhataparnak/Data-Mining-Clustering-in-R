library(dplyr)
library(magrittr)

read.csv("F:/UTA/Courses/Assignments/Data Mining/Project2/feb2008.csv") %>%
  full_join(read.csv("F:/UTA/Courses/Assignments/Data Mining/Project2/feb2007.csv")) %>%
  write.csv("F:/UTA/Courses/Assignments/Data Mining/Project2/third.csv",quote = F, row.names=F)


txw_2007  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/third.csv",
                      header =  FALSE, sep = ",",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
txw_2007 <- slice(txw_2007, -c(1))
txw_2007[1] <- list(NULL)
colnames(txw_2007)=c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP",
                     "STP","Count_STP","Visib","Count_Visib","WDSP","Count_WDSP","MXSDP","Gust","PRCP",
                     "SNDP","FRSHIFT")
trial_df_2007<- tbl_df(txw_2007)
length(unique(trial_df_2007$STN))
df_2007 <- sqldf("SELECT DISTINCT trial_df_2007.* from trial_df_2007")

df_2007<-df_2007[!duplicated(df_2007[1]),]

write.csv(df_2007,"F:/UTA/Courses/Assignments/Data Mining/Project2/feb2008_feb2007.csv", row.names = TRUE)