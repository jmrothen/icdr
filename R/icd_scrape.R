#' remove the icd from a string to leave just descriptors
#'
#' @param vec a vector of strings
#' @param proc indicator for procedurecode scrape
#' @return trimmed vector
#' @examples
#' icd_trim(c("U07.1 Covid", "E11 Diabetes"))
#' @export
icd_trim <- function(vec, proc = F) {
  stringr::str_remove_all(vec, "(\\-*[A-Z]{1}[0-9]{2})|(\\-*[A-Z]{1}[0-9]{1}[A-Z]{1})|(O9A)") %>%
    trimws() -> out
  # this was added in anticaption of procedurecodes
  if (proc) {
    out %>%
      stringr::str_remove_all("(.[0-9]{1,4})|(.[0-9]{1,4}[A-Z]{1,3})") %>%
      trimws() -> out
  }
  return(out)
}


#' extract the icd from a string to leave just descriptors
#'
#' @param vec a vector of strings
#' @return trimmed vector
#' @examples
#' icd_extract(c("U07.1 Covid", "E11 Diabetes"))
#' @export
icd_extract <- function(vec) {
  stringr::str_extract(vec, "(\\-*[A-Z]{1}[0-9]{2})|(\\-*[A-Z]{1}[0-9]{1}[A-Z]{1})|(O9A)") %>%
    trimws() -> out
  return(out)
}


#' run a full refresh of the icd-10 scraper for diagnosiscodes
#'
#' @param zzz how long inbetween website pulls... please don't go below 5
#' @return updated icd-10-diag tables
#' @examples
#' # diag_scrape(5)
#' @export
diag_scrape <- function(zzz = 5) {
  test <- rvest::read_html("https://www.icd10data.com/ICD10CM/Codes")

  # is currently 22
  limit <- grep("Z99", layer1)[1]

  test %>%
    rvest::html_elements(css = ".identifier") %>%
    rvest::html_text() %>%
    trimws() %>%
    head(limit) -> layer1
  # layer1 <-  layer1[1:22]

  test %>%
    rvest::html_elements(css = ".body-content li") %>%
    rvest::html_text() %>%
    trimws() %>%
    head(limit) -> layer1t

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
            l4 = icd_trim(layer4[l], T),
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
