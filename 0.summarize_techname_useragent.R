####################################################################################
# Name: Summarize technical names list from tapad user agents (measurement data) 
# Description: Create a list of technical device names order by popularity on Tapad partner
# Version:
#   2016/10/04 RS: Initial version
#   
####################################################################################

library(data.table)
library(dplyr)

# Clear objects
rm(list=ls())

# Set default directory
default_directory <- "D:/Tapad_UC1/Mobile_Atlas"
#default_directory <- "R:/ShareData/Rata/Mobile_Atlas"

setwd(default_directory)

# Read tapad raw data
dta <- fread("action_events_160914_sg.csv", integer64 = "character",header=TRUE) 

dta <- data.frame(dta)


dta <- dta[,c(4,8)]
colnames(dta) <- c("user_agent","model_device")
dta <- dta[!(dta$model_device==""),]
dta2 <- dta %>% 
  group_by(model_device) %>% 
  summarise(user_agent=user_agent[1],counts=n()) %>% arrange(desc(counts))
dta2 <- dta2[dta2$counts>100,] # choose only technical names having more than 100 reach
dta <- data.frame(cbind(dta2[,2],dta2[,1],dta2[,3]))

dta <- dta[!duplicated(dta$model_device),]
write.csv(dta, file=paste(default_directory ,"/RESULTS/","useragent_techname_tapad.csv",sep=""), row.names = FALSE)
