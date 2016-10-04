####################################################################################
# Name: Write Impala useragent2commercname
# Description: Write the impala code for creating a function useragent2commercname from mobile atlas table 
# Version:
#   2016/10/04 RS: Initial version
#   
####################################################################################


library(data.table)
library(dplyr)
# clear objects
rm(list=ls())

# Set default directory
default_directory <- "D:/Tapad_UC1/Mobile_Atlas/RESULTS"
#default_directory <- "R:/ShareData/Rata/Mobile_Atlas/RESULTS"

setwd(default_directory)

dta <- fread("techname_mobile_atlas.csv", integer64 = "character",header=TRUE)
dta <- data.frame(dta)

# Hypothesis for phones whose info is not available on gsmarena

dta[grepl('smart|true|lava|iris|dtac|blade|zte|joey',dta$model_device)& is.na(dta$release_price) ,6] <- 70
dta[grepl('i-mobile|i-style',dta$model_device)& is.na(dta$release_price) ,6] <- 100
dta[grepl('iphone os',dta$model_device)& is.na(dta$release_price) ,6] <- 750
dta[grepl('wiko',dta$brand)& is.na(dta$release_price) ,6] <- 150
dta[grepl('vivo',dta$brand)& is.na(dta$release_price) ,6] <- 300
dta[grepl('oppo',dta$brand)& is.na(dta$release_price) ,6] <- 240 # mean(dta[!is.na(dta$release_price)&dta$brand=='oppo',6],na.rm=TRUE) 
dta[is.na(dta$model_name),3] <- dta[is.na(dta$model_name),1]

write.csv(dta, file="techname_mobile_atlas_approx.csv", row.names = FALSE)

# inverse the order of the table to have the case of the most popular phone at the end in case of multiple match

dta$index <- as.numeric(row.names(dta))
dta <- dta[order(dta$index, decreasing = TRUE),  ]

mobileInfo <- dta

nametowrite <- mobileInfo$model_device
modeltowrite <- mobileInfo$model_name
pricetowrite <- mobileInfo$release_price
regex.model <- ""



#like
for (j in 1:length(nametowrite)){
    regex.model <- paste(regex.model,paste("when lower(user_agent) like ('%",tolower(nametowrite[j]),"%')"," then '",modeltowrite[j],"' ",sep=""),sep="")
  }
  
print(regex.model)
write(regex.model, file = "sql_modelname.txt")

#like
nametowrite <- mobileInfo[!is.na(mobileInfo$release_price),1]
pricetowrite <- mobileInfo[!is.na(mobileInfo$release_price),6]
regex.model <- ""
for (j in 1:length(nametowrite)){
  regex.model <- paste(regex.model,paste("when lower(user_agent) like ('%",tolower(nametowrite[j]),"%')"," then ",pricetowrite[j]," ",sep=""),sep="")
}

print(regex.model)
write(regex.model, file = "sql_pricephone.txt")
  

