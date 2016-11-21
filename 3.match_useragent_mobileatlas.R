####################################################################################
# Name: Match useragent to mobile atlas
# Description: Matching phone technical names extracted from user agents to mobile atlas table 
# Version:
#   2016/10/04 RS: Initial version
#   2016/10/13 RS: Eliminate Apple and unuseful brands from the mobile atlas table
####################################################################################

# install.packages("data.table")
# install.packages("XML")
# install.packages("RCurl")
library(data.table)
library(dplyr)
library(rvest)
library(xml2)
library(httr)
library(XML)
library(RCurl)

# Clear objects
rm(list=ls())

# Set default directory
default_directory <- "D:/Tapad_UC1/Mobile_Atlas/RESULTS"
#default_directory <- "R:/ShareData/Rata/Mobile_Atlas/RESULTS"

setwd(default_directory)


# set function
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

getGoogleURL <- function(search.term, domain = '.co.th', quotes=TRUE) 
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
                        search.term, sep='')
}

getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                           (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                        (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

cutAlphabetSamsung <- function(model.samsung){
  element <- gsub('.*sch-','',gsub('.*shv-','',gsub('.*sgh-','',gsub('.*gt-','',gsub('.*sm-','',model.samsung)))))
  root <- gsub('-.*','',model.samsung)
  dummy <- strsplit(element,split='')
  if (length(grep("[0-9]",dummy))>0){
    last.number <- tail(grep("[0-9]",dummy[[1]]),1)
    v <- dummy[[1]][1:last.number]
    v <- paste(v,collapse="")
    model.samsung.reduced <- paste(root,'-',v,sep="")
  } else model.samsung.reduced <- model.samsung
  return (model.samsung.reduced)
  
}



# start



dta <- fread("useragent_techname_tapad.csv", integer64 = "character",header=TRUE)
dta <- data.frame(dta)

#colnames(dta)[4] <- 'model_device_raw'

dta$model_device <- sub('/.*','',dta$model_device)
dta$model_device <- sub('applewebkit.*','',dta$model_device)
dta$model_device <- sub('.*samsung','',dta$model_device)
dta$model_device <- gsub('_',' ',dta$model_device)
dta$model_device <- gsub('lg-','lg',dta$model_device)
dta$model_device <- gsub('lg','lg ',dta$model_device)
dta$model_device <- gsub('oppo','oppo ',dta$model_device)
dta$model_device <- gsub(')','',dta$model_device)
#dta$model_device <- gsub('xperia','',dta$model_device)
dta$model_device <- gsub('"','',dta$model_device)
dta$model_device <- sub('by.*','',dta$model_dev)
dta$model_device <- sub(';.*','',dta$model_dev)
dta$model_device <- sub('\\?.*','',dta$model_dev)
dta$model_device <- sub('\\(.*','',dta$model_dev)
dta <- dta[!dta$model_device==" ",]
dta$model_device <- trim(as.character(dta$model_device))

dta <- dta[!is.na(dta$model_device),]
dta <- dta[!grepl('iphone os|ipad',dta$model_device),]
dta <- data.frame(dta)

dta<- dta[!grepl('x-citeï¼�',dta$model_device),]
dta$model_device[grepl('zh-cn',dta$model_device)] <- 'lenovo a3300'


dta[grepl('sm|gt',dta$model_device) & !grepl('smart',dta$model_device),2] <- sapply(dta[grepl('sm|gt',dta$model_device) & !grepl('smart',dta$model_device),2], function(x) cutAlphabetSamsung(x))

dta <- dta[!duplicated(dta$model_device),]



temp = list.files(pattern="INFO.csv")
temp <- temp[!grepl('alcatel|allview|amazon|amoi|apple|archos|at&t|benefon|benq_siemens|bird|bosch|bq|casio|cat|chea|emporia|eten|fujitsu_siemens|ericsson|garmin_asus|haier|i_mate|innostream|inq|jolla|karbonn|kyocera|maxon|mitac|mitsubishi|modu|mwg|nec|neonode|nvidia|o2|orange |palm|parla|qtek|sagem|sendo|sewon|siemens|sonim|sony_ericsson|thuraya|t_mobile|tel_me|telit|thuraya|vertu|vk_mobile|vodafone|wnd|xcute',tolower(temp))]
brandlist <- tolower(sub('_INFO.*','',temp))
brandlist[brandlist=="i_mobile"] <- "i-mobile"



# write a table of the information of all brands
for (k in 1:length(temp)){
  info <- fread(temp[k], integer64 = "character",header=TRUE)
  
  info <- data.frame(info)
  info <- info[info$SCREEN > 3,]
  info <- unique(info)
  info$DETAIL_NAME <- tolower(info$DETAIL_NAME)
  info$MODEL <- tolower(info$MODEL)
  if(brandlist[k]=="samsung"){ #for Samsung, "gear" is eliminated
    colnames(info) <-colnames(info.All)
    info<-info[!grepl("gear",info$MODEL),]
  }
  
  if (k==1){
    info.All <- info
  } else {
    info.All <- rbind(info.All,info)
  }
}



info.All <- cbind(info.All,gsub(',.*','',info.All$RELEASE_TIME),trim(gsub('.*,','',info.All$RELEASE_TIME)))
colnames(info.All)[7:8] <- c("RELEASE_YEAR","RELEASE_MONTH")
info.All[grep("TRUE",as.character(info.All$RELEASE_YEAR)==as.character(info.All$RELEASE_MONTH)),8] <- NA
info.All$RELEASE_YEAR <- as.numeric(as.character(info.All$RELEASE_YEAR))
info.All$RELEASE_MONTH <- as.character(info.All$RELEASE_MONTH)
info.All <- data.frame(info.All, stringsAsFactors=FALSE)
info.All <- unique(info.All)
info.All$BRAND <- tolower(info.All$BRAND)
info.All <- info.All[as.numeric(as.character(info.All$RELEASE_YEAR))>2006,]
info.All <- info.All[,c(-3)]
info.All <- cbind(info.All[,c(1:2,6:7)],info.All[,c(3:5)])
info.All <-info.All[order(info.All$RELEASE_YEAR,decreasing = FALSE),]
addinfo.All <- data.frame(matrix(NA, nrow = nrow(dta), ncol = 4))
colnames(addinfo.All) <- c("YEAR","MONTH","PRICE","SCREEN")



model_device.augmented<- data.frame(matrix(NA, nrow = nrow(dta), ncol = (ncol(info.All)+1)))
model_device.augmented[,1] <- dta$model_device
colnames(model_device.augmented) <- c("model_device","brand","model_name",
                                      "release_year","release_month","release_price","screensize","detail")
#for (k in (1:nrow(dta))){
pb <- grep('sm-t211|sm-t116|sm-g316|sm-t715|Kgt-n5120|sm-t719|shv-t719|gt-i9205',dta$model_device)
for (k in pb){
  
  tryCatch({
    print(k)
    model_device <- dta$model_device[k]
    user_agent <- tolower(dta$user_agent[k])
    info_brand <- info.All
    brand <- "unknown"
    
    # determine the brand of the phone from user agent and limit a set of search on gsmarena table to the models whose brand is the same.
    
    if (length(grep("sm-|gt-|galaxy|samsung|sch|shv|sgh",user_agent))>0){
      info_brand <- info.All[info.All$BRAND=="samsung",]
      brand <- "samsung"
    } else if (length(grep("iris",user_agent))>0){
      info_brand <- info.All[info.All$BRAND=="lava",]
      brand <- "lava"
    } else if (length(grep("u feel|tommy|sunny|robby|lenny|fever|pulp|rainbow|selfy|sunset|bloom|ridge|highway|star|birdy|goa|jimmy|gataway|wax",user_agent))>0){
      info_brand <- info.All[info.All$BRAND=="wiko",]
      brand <- "wiko"
      
    } else
      
      for (m in 1:length(brandlist)){
        if (length(grep(brandlist[m],user_agent))>0){
          info_brand <- info.All[info.All$BRAND==brandlist[m],]
          brand <- brandlist[m]
          print(brand)
        } 
      }
    
    
    
    
    info_brand <- info_brand[order(info_brand$RELEASE_YEAR,decreasing=FALSE),]
    info_brand <- info_brand[!is.na(info_brand$BRAND),]
    
    for (j in 1:nrow(info_brand)){
      
      if (length(grep(model_device,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
        
        print(j)
        addinfo.All[k,] <- info_brand[j,3:6]
        model_device.augmented[k,] <- c(model_device,info_brand[j,])
        print(model_device.augmented[k,] )
        
      }
    }
    
    # For Samsung whose gsmarena info does not include gt/ sm, e.g. Samsung Galaxy Note II N7100 (http://www.gsmarena.com/samsung_galaxy_note_ii_n7100-4854.php)
    # we cut the sm, gt so that it could find the matched expression in gsmarena tables
    
    if (is.na(addinfo.All[k,1]) & brand=='samsung' ){
      
      model_device_cut <- gsub('gt-','',gsub('sm-','',model_device))
      samsung_number <- gsub('[^0-9]','',model_device_cut) 
      samsung_alphabet <- gsub('[^a-z]','',model_device_cut) # n = note, t = tab
      
      if(samsung_alphabet=="n"){
        info_brand <- info_brand[grep("note",info_brand$MODEL),]
      } else if (samsung_alphabet=="t"){
        info_brand <- info_brand[grep("tab",info_brand$MODEL),]
      } else if (samsung_alphabet=="a"){
        info_brand <- info_brand[grepl("galaxy a",info_brand$MODEL)&!grepl("galaxy ace",info_brand$MODEL),]
      } else if (samsung_alphabet=="j"){
        info_brand <- info_brand[grepl("galaxy j",info_brand$MODEL),]
      }  
      
      for (j in 1:nrow(info_brand)){
        
        if (length(grep(model_device_cut,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
          
          print(j)
          addinfo.All[k,] <- info_brand[j,3:6]
          model_device.augmented[k,] <- c(model_device,info_brand[j,])
          print(model_device.augmented[k,] )
          
        }
      }
      
      if(is.na(addinfo.All[k,1])&(samsung_alphabet=="a"|samsung_alphabet=="j")){
        
        model_device_cut <- paste(samsung_alphabet,unlist(strsplit(samsung_number,split=''))[1],sep="")
        for (j in 1:nrow(info_brand)){
          
          if (length(grep(model_device_cut,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
            
            print(j)
            addinfo.All[k,] <- info_brand[j,3:6]
            model_device.augmented[k,] <- c(model_device,info_brand[j,])
            print(model_device.augmented[k,] )
            
          }
        }
        
        
      }
      
      
      # For Samsung phone that have not still found the match, we cut all the alphabet. In fact, there exists a case where the information on the gsmarena page
      # does not include the alphabet at all, e.g. as http://www.gsmarena.com/samsung_galaxy_note_8_0-5252.php has only the keyword 510 available
      # and we keep only the first 3 numbers.
      if(is.na(addinfo.All[k,1])){
        
        model_device_cut <- gsub('[^0-9]','',model_device_cut)
        e<- strsplit(model_device_cut,split='')
        v <- e[[1]][1:3]
        model_device_cut <- paste(v,collapse="")
        for (j in 1:nrow(info_brand)){
          
          if (length(grep(model_device_cut,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
            
            print(j)
            addinfo.All[k,] <- info_brand[j,3:6]
            model_device.augmented[k,] <- c(model_device,info_brand[j,])
            print(model_device.augmented[k,] )
            
          }
        }
      }
      
    } else if (is.na(addinfo.All[k,1])){ 
      # for other models whose still have not matched to the expressions in the gsmarena table yet, 
      # we cut the brand name from the model_device regex from kaidee.
      
      if (!brand=="unknown"){
        model_device_cut <- gsub(brand,'',model_device)
        model_device_cut <- trim(model_device_cut)
      } else 
        model_device_cut <- model_device 
      
      if (brand=="lenovo"){
        
        e<- strsplit(model_device_cut,split='')
        v <- e[[1]][1:3]
        model_device_cut <- paste(v,collapse="")
      }  
      
      for (j in 1:nrow(info_brand)){
        
        if (length(grep(model_device_cut,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
          
          addinfo.All[k,] <- info_brand[j,3:6]
          model_device.augmented[k,] <- c(model_device,info_brand[j,])
          print(model_device.augmented[k,] )
          
        }
      }
      
      
      
    }
    
    if (is.na(addinfo.All[k,1])){ 
      model_device <- 'sm-t116'
      model_device <- 'sm-t715'
      search.term <- paste(model_device,'+gsmarena',sep='')
      quotes <- "FALSE"
      search.url <- getGoogleURL(search.term=search.term, quotes=quotes)
      
      links <- getGoogleLinks(search.url)
      
      link <- links[1]
      link <- gsub('&sa.*','',gsub('.*q=','',link))
      brand <- gsub('_.*','',gsub('.*.com/','',link))
      info_brand <- info.All[info.All$BRAND==brand,]
      info_brand <- info_brand[!is.na(info_brand$BRAND),]
      model_device_cut <- gsub('-.*','',gsub('.*.com/','',link))
      model_device_cut <-  gsub(paste(brand,'_',sep=''),'',model_device_cut)
      model_device_cut <- gsub('_',' ',sub('_0','',model_device_cut))
   
      if (length(grep('galaxy mega',model_device_cut)>0)){
        samsung_number <- gsub('[^0-9]','',model_device_cut) 
        dummy <- strsplit(samsung_number,'')
        firstnum <- unlist(dummy)[1]
        model_device_cut <- paste(gsub('[0-9].*','',model_device_cut),firstnum,sep='') 
      }
      print(model_device_cut)
      for (j in 1:nrow(info_brand)){
        
        if (length(grep(model_device_cut,info_brand[j,c(2,7)][which( !is.na(info_brand[j,c(2,7)]), arr.ind=TRUE)]))>0) {
          print(j)
          addinfo.All[k,] <- info_brand[j,3:6]
          model_device.augmented[k,] <- c(model_device,info_brand[j,])
          print(model_device.augmented[k,] )
          
        }
      }
      
      
    }
    
    
    
  },error=function(e){}) 

}

test <- model_device.augmented[pb,]

write.csv(model_device.augmented, file="techname_mobile_atlas.csv", row.names = FALSE)
write.csv(model_device.augmented[!is.na(model_device.augmented$brand),], file="techname_mobile_atlas_filled.csv", row.names = FALSE)
write.csv(model_device.augmented[is.na(model_device.augmented$brand),], file="techname_mobile_atlas_unfilled.csv", row.names = FALSE)

# 
# model_device.augmented$X1 <- trim(model_device.augmented$X1)
# dta <- cbind(name.nonOEM[,1:2],model_device.augmented[,1:8])
# colnames(dta) <- cbind("MODEL_KAIDEE","COUNT","model_device_KAIDEE","BRAND","MODEL","RELEASE_YEAR","RELEASE_MONTH","PRICE","SCREEN","DETAIL")
# #install.packages("dplyr")
# library(dplyr)
# 
# dta.smry <- dta[!is.na(dta$MODEL),] %>%
#   group_by(MODEL) %>%
#   summarise(model_device_KAIDEE = model_device_KAIDEE[1],
#             COUNT=sum(COUNT),
#             BRAND=BRAND[1],
#             RELEASE_YEAR=RELEASE_YEAR[1],
#             RELEASE_MONTH=RELEASE_MONTH[1],
#             PRICE=PRICE[1],
#             SCREEN=SCREEN[1],
#             DETAIL=DETAIL[1])
# dta.smry.na <- dta[is.na(dta$MODEL),c(5,3,2,4,6:10)]
# 
# 
# dta.smry <- rbind(dta.smry,dta.smry.na)
# dta.smry <- dta.smry[order(dta.smry$COUNT,decreasing=TRUE),]
# 
# write.csv(dta.smry, file="model_device_meas_augmented_2308.csv", row.names = FALSE)

