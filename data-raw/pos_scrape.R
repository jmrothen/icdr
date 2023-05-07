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

