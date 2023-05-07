library(rvest)
library(dplyr)
library(stringr)
link <- 'https://www.cms.gov/Medicare/Coding/place-of-service-codes/Place_of_Service_Code_Set'
read_html(link) %>%
  html_table()  %>%
  as.data.frame()-> bozo

str_replace_all('[\\n]{1,10}[\\t]{1,10}',
                string = bozo$Place.of.Service.Name,
                replacement = ', ') %>%
  str_remove_all(pattern='\\*|\\/|') %>%
  trimws() ->bozo$Place.of.Service.Name

str_replace_all('[\\n]{1,10}[\\t]{1,10}',
                string = bozo$Place.of.Service.Description,
                replacement = ' ') %>%
  trimws() -> bozo$Place.of.Service.Description

colnames(bozo) <- c('code','name','description')


temp <- data.frame(bozo[1,][-1,])
for(i in 1:dim(bozo)[1]){
  if(grepl('\\-',bozo$code[i])){
    str_extract_all(bozo$code[i],'[0-9]{1,3}')[[1]]-> nums
    numout <- seq(nums[1],nums[2])
    nameout <- bozo[i,2]
    descout <- bozo[i,3]
    for(j in numout){
      temp %>% add_row(code=as.character(j),
                       name=nameout,
                       description=descout) -> temp
    }
  }
}

bozo <- union(bozo,temp)

bozo <- bozo[which(!grepl('\\-',bozo$code)),]

bozo %>% arrange(code) -> bozo

placeofservice<-bozo

usethis::use_data(placeofservice)
