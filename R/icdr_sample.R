utils::data("icd10map", envir=environment())
utils::data("billtype", envir=environment())
utils::data("revenuecode", envir=environment())
utils::data("placeofservice", envir=environment())

#' Generate some sample codes
#'
#' @param n number of codes wanted
#' @param code type of code desired
#' @return a random sample of the respective code type
#' @examples
#' icdr_sample(10)
#'
#' @export
icdr_sample <- function(n, code='diag'){
  out <- dplyr::case_when(
    tolower(code) %in% c('rev','revenue','revenuecode','revcode') ~
      sample(revenuecode$Code,n,replace = T),
    tolower(code) %in% c('pos','placeofservice','placeofservicecode','poscode') ~
      sample(placeofservice$code,n,replace = T),
    tolower(code) %in% c('diagnosiscode','diagcode','diag','icd10') ~
      sample(icd10map$l4c,n,replace = T),
    tolower(code) %in% c('billtype','bt','billtypecode','btcode') ~
      sample(billtype$billtype,n,replace = T),
    .default = c('Please Supply a Valid Code Type')
  )
  return(out)
}
