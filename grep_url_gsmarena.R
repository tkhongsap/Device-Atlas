
library(rvest)
library(ggplot2)
library(data.table)
library(plyr)
library(bitops)
library(stringr)
# clear objects
rm(list=ls())

# set default directory
setwd("D:/Tapad/Web_scraping")
# Load the page
gsmarena.mainpage <- html(x = "http://www.gsmarena.com")

urls.brand <- gsmarena.mainpage %>% # feed `main.page` to the next step
  html_nodes(".clearfix li a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs
urls.brand <- urls.brand[grep("phone",urls.brand)]
dummy <- strsplit(urls.brand,"-")

brand.list <- data.frame(matrix(NA, nrow = length(urls.brand), ncol = 1))
for (k in 1:length(urls.brand)){
  brand.list[k,1] <- dummy[[k]][1]
}
brand.list<- brand.list[,1]

urls.brand <- apply(data.frame(urls.brand),1, function(x) paste(c("http://www.gsmarena.com/",x),collapse=''))


for (k in (1:length(urls.brand))){
print(brand.list[k])
main.page <- urls.brand[k]
html <- paste(readLines(main.page), collapse="\n")
allhref <- str_match_all(html, "<a href=\"(.*?)\"")

urls.brand.page <-  data.frame(allhref[[1]][ ,2][grepl("-p[1-9]",allhref[[1]][ ,2])])
urls.brand.page <- apply(urls.brand.page,1, function(x) paste(c("http://www.gsmarena.com/",x),collapse=''))


for (j in 1:length(urls.brand.page)){

  main.brand.page <- urls.brand.page[j]
  html <- paste(readLines(main.brand.page), collapse="\n")
  allmodel <- str_match_all(html, "<a href=\"(.*?)\"")
  
matched <- allmodel[[1]][ ,2][grepl(paste(c(brand.list[k],"_"),collapse=''),allmodel[[1]][ ,2])&!grepl("review",allmodel[[1]][ ,2])]
urls <- data.frame(matrix(NA, nrow = length(matched), ncol = 1))
matched <- data.frame(matched)
urls <- apply(matched,1, function(x) paste(c("http://www.gsmarena.com/",x),collapse=''))

if (j == 1){
  urls_brand <- data.frame(urls)  
} else {
  urls_brand <- rbind(urls_brand,data.frame(urls))
}

}

nameurls.file <- paste("URLS", toupper(brand.list[k]), sep="_")
assign(nameurls.file,urls_brand)
write.csv(urls_brand, file=paste(nameurls.file,".csv",sep=""), row.names = FALSE)


if (k==1){
  URLS_ALLMODEL <- data.frame(urls_brand)  
} else {
  URLS_ALLMODEL <- rbind(URLS_ALLMODEL,urls_brand)
}


}

write.csv(URLS_ALLMODEL, file="URLS_ALLMODEL.csv", row.names = FALSE)


