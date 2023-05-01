codes <- rvest::read_html('https://med.noridianmedicare.com/web/jea/topics/claim-submission/revenue-codes')

# grab the left hand side
rvest::html_elements(codes, css='#tableToSearch td+ td') %>%
  rvest::html_text2() -> temp

# grab the right hand side
rvest::html_elements(codes, css='#tableToSearch td:nth-child(1)') %>%
  rvest::html_text2() -> group


stringr::str_split(temp,'\\n') -> pp



data.frame(group) -> df

idkout <- c()
idkout2 <- c()

for(i in 1:length(pp)){
  idk <- pp[[i]][1]
  idkout <- append(idkout,idk)
  idk2 <- pp[[i]][-1]
  idkout2 <- append(idkout2,idk2)
}



df$grouper <- idkout

str_replace_all(
  idkout2, '- ', ' - '
) -> idkout2


str_replace_all(
  idkout2, ' – ', ' - '
) -> idkout2



idkout2 %>% stringr::str_split(' - ') %>% unlist() -> pp2



cod <- c()
det <- c()

state <- '1'
for(i in 1:length(pp2)){
  if(substr(pp2[i],1,1) %in% c('0','1','2','3')){
    cod <- append(cod, pp2[i])
    state <- '1'
  }else{
    if(!(substr(pp2[i],1,1) %in% c('0','1','2','3')) & state=='1'){
      det <- append(det, pp2[i])
      state <- '0'
    }else{
      det[length(det)] <- paste(det[length(det)], pp2[i], sep=' - ')
      state <- '0'
    }
  }
}

cod <- trimws(cod)

grep('[a-zA-Z]',cod) -> mistakes

print(mistakes)

better <- data.frame(cod,det)

better$trim <- substr(better$cod,1,3)

df$trim <- substr(df$group,1,3)


merge(df,better,by='trim', all = T) ->fin

fin <- fin[,2:5]


colnames(fin) <- c('Category','Category Description','Code','Code Description')

fin$Code[is.na(fin$Code)] <- 'NA'
fin$`Code Description`[is.na(fin$`Code Description`)] <- 'NA'

revenuecode <- fin

usethis::use_data(revenuecode)
