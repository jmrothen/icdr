library(rvest)
library(dplyr)
library(stringr)

test <- read_html('https://www.icd10data.com/ICD10CM/Codes')

test %>% html_elements(css = '.identifier') %>% html_text()%>% trimws() -> layer1; layer1 <-  layer1[1:22]
test %>% html_elements(css = '.body-content li') %>% html_text()%>% trimws() -> layer1t; layer1t <- layer1t[1:22]


icd_clean <- function(vec,super=F){
  str_remove_all(vec, '(\\-*[A-Z]{1}[0-9]{2})|(\\-*[A-Z]{1}[0-9]{1}[A-Z]{1})|(O9A)') %>% 
    trimws() -> out
  if(super){
    out %>% str_remove_all('(.[0-9]{1,4})|(.[0-9]{1,4}[A-Z]{1,3})') %>% trimws() -> out
  }
  return(out)
}
icd_extract <- function(vec){
  str_extract(vec,'(\\-*[A-Z]{1}[0-9]{2})|(\\-*[A-Z]{1}[0-9]{1}[A-Z]{1})|(O9A)')%>%
    trimws() -> out
  return(out)
}


zzz <- 5


ppp <- data.frame(l1='NA',l1c ='NA', l2='NA', l2c='NA', l3='NA', l3c='NA', l4='NA',l4c='NA')[-1,]

for(i in 1:length(layer1)){
  read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],sep='')) %>%
    html_elements(css='.i51 li') %>% html_text()%>% trimws() ->layer2
  Sys.sleep(zzz)
  read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],sep='')) %>%
    html_elements(css='.i51 .identifier') %>% html_text() %>% trimws()->layer22
  Sys.sleep(zzz)
  print(paste(layer1t[i],'...',sep=''))
  # '.i51 .identifier'
  for(j in 1:length(layer2)){
    read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],'/',layer22[j],sep='')) %>%
      html_elements(css='.i51 li') %>% html_text()%>% trimws() ->layer3
    Sys.sleep(zzz)
    read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],'/',layer22[j],sep='')) %>%
      html_elements(css='.i51 .identifier') %>% html_text() %>% trimws()->layer33
    Sys.sleep(zzz)
    print(paste(layer2[j],'...',sep=''))
    for(k in 1:length(layer3)){
      
      read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],'/',layer22[j], '/', layer33[k],'-',sep='')) %>%
        html_elements(css='.codeLine span') %>% html_text()%>% trimws() ->layer4
      Sys.sleep(zzz)
      read_html(paste('https://www.icd10data.com/ICD10CM/Codes/', layer1[i],'/',layer22[j], '/', layer33[k],'-',sep='')) %>%
        html_elements(css='.codeLine .identifier') %>% html_text() %>% trimws()->layer44
      Sys.sleep(zzz)
      print(paste(layer3[k],'...',sep=''))
      
      for(l in 1:length(layer4)){
        ppp %>% add_row(
          l1=icd_clean(layer1t[i]), 
          l1c=layer1[i],
          l2=icd_clean(layer2[j]), 
          l2c=layer22[j],
          l3=icd_clean(layer3[k]), 
          l3c=layer33[k],
          l4=icd_clean(layer4[l],T),
          l4c=layer44[l]
        ) -> ppp 
      }  
    }
  }
}


codes <- unique(ppp)

write.csv(codes, "icd10map.csv",row.names = F)
