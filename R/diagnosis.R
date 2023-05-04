utils::data("icd10map", envir=environment())


#' Trim ICD-10 codes
#'
#' remove ICD-10 codes from a string
#'
#' @param vec a vector of strings
#' @return trimmed vector
#' @examples
#' icd_trim(c("U07.1 Covid", "E11 Diabetes"))
#' @export
icd_trim <- function(vec) {
  stringr::str_remove_all(vec, "([A-Z]{1}[0-9]{2}[\\.]{0,1}[a-zA-Z0-9]{0,5})") %>%
    trimws() -> out
  return(out)
}


#' Extract ICD-10
#'
#' extract the ICD-10 codes from a string
#'
#' @param vec a vector of strings
#' @return trimmed vector
#' @examples
#' icd_extract(c("U07.1 Covid", "E11 Diabetes"))
#' @export
icd_extract <- function(vec) {
  stringr::str_extract(vec, "([A-Z]{1}[0-9]{2}[\\.]{0,1}[a-zA-Z0-9]{0,5})") %>%
    trimws() -> out
  return(out)
}



#' Validate ICD-10
#'
#' check if ICD-10 code is in the database
#'
#' @param code a vector of strings
#' @param dataset name of dataframe, default uses the internal table
#' @param column name of column, default uses the relevant columns in the internal table
#' @return boolean
#' @examples
#' icd_check('E11')
#' @export
icd_check <- function(code, dataset=NULL, column='l4c'){
  if(is.null(dataset)){
    dataset <- icd10map
  }
  code <- as.character(code)
  colindex <- grep(column, colnames(dataset))
  grepl(code,dataset[,colindex]) -> out
  return(sum(out)>=1)
}



#' Full Webscrape of ICD-10 data
#'
#' run a full refresh of the icd-10 scraper for diagnosiscodes
#' note, this takes a long time due to the sleep time in between http requests
#' this is to guarantee we do not ddos the website of interest
#'
#' @param zzz how long inbetween website pulls... please don't go below 5
#' @return updated icd-10-diag tables
#' @examples
#' # diag_scrape(5)
#' @export
diag_scrape <- function(zzz = 5) {
  test <- rvest::read_html("https://www.icd10data.com/ICD10CM/Codes")


  test %>%
    rvest::html_elements(css = ".identifier") %>%
    rvest::html_text() %>%
    trimws() -> tempe

  limit <- grep("Z99", tempe)[1]


  tempe %>% utils::head(limit) -> layer1


  test %>%
    rvest::html_elements(css = ".body-content li") %>%
    rvest::html_text() %>%
    trimws() %>%
    utils::head(limit) -> layer1t

  ppp <- data.frame(l1 = "NA", l1c = "NA", l2 = "NA", l2c = "NA", l3 = "NA", l3c = "NA", l4 = "NA", l4c = "NA")[-1, ]

  for (i in 1:length(layer1)) {
    rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], sep = "")) %>%
      rvest::html_elements(css = ".i51 li") %>%
      rvest::html_text() %>%
      trimws() -> layer2

    Sys.sleep(zzz)

    rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], sep = "")) %>%
      rvest::html_elements(css = ".i51 .identifier") %>%
      rvest::html_text() %>%
      trimws() -> layer22

    Sys.sleep(zzz)

    print(paste(layer1t[i], "...", sep = ""))

    # '.i51 .identifier'
    for (j in 1:length(layer2)) {
      rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], "/", layer22[j], sep = "")) %>%
        rvest::html_elements(css = ".i51 li") %>%
        rvest::html_text() %>%
        trimws() -> layer3

      Sys.sleep(zzz)

      rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], "/", layer22[j], sep = "")) %>%
        rvest::html_elements(css = ".i51 .identifier") %>%
        rvest::html_text() %>%
        trimws() -> layer33

      Sys.sleep(zzz)

      print(paste(layer2[j], "...", sep = ""))

      for (k in 1:length(layer3)) {
        rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], "/", layer22[j], "/", layer33[k], "-", sep = "")) %>%
          rvest::html_elements(css = ".codeLine span") %>%
          rvest::html_text() %>%
          trimws() -> layer4

        Sys.sleep(zzz)

        rvest::read_html(paste("https://www.icd10data.com/ICD10CM/Codes/", layer1[i], "/", layer22[j], "/", layer33[k], "-", sep = "")) %>%
          rvest::html_elements(css = ".codeLine .identifier") %>%
          rvest::html_text() %>%
          trimws() -> layer44

        Sys.sleep(zzz)

        print(paste(layer3[k], "...", sep = ""))

        for (l in 1:length(layer4)) {
          ppp %>% dplyr::add_row(
            l1 = icd_trim(layer1t[i]),
            l1c = layer1[i],
            l2 = icd_trim(layer2[j]),
            l2c = layer22[j],
            l3 = icd_trim(layer3[k]),
            l3c = layer33[k],
            l4 = icd_trim(layer4[l]),
            l4c = layer44[l]
          ) -> ppp
        }
      }
    }
  }
  codes <- dplyr::distinct(ppp)
  return(codes)
}


# write.csv(codes, "icd10map.csv",row.names = F)
