utils::data("billtype", envir=environment())


#' Validate Billtype
#'
#' check if billtype code is valid
#'
#' @param btcode a billtype code or vector of codes
#' @return boolean for validity
#' @examples
#' billtype_check('0111')
#' @export
billtype_check <- function(btcode){
  str_len_to_col(btcode) -> coll
  if(is.na(coll)){
    stop('billtype code provided does not fit the standard parameters')
  }
  out <- grepl(btcode,billtype[,coll])
  return(sum(out)>=1)
}


str_len_to_col <- function(btcode){
  stringr::str_length(btcode) -> Len
  coll <- dplyr::case_when(
    Len == 3 ~ grep('billtype3',colnames(billtype))[1],
    Len == 4 ~ grep('billtype',colnames(billtype))[1],
    .default = NA
  )
  return(coll)
}


#' Extract Facility information from Billtype
#'
#' @param btcode a billtype code or vector of codes
#' @return a string or vector of strings with facility info
#' @examples
#' get_facility(c('0123','731'))
#' @export
get_facility <- function(btcode){
  full_out <- c()
  for(i in 1:length(btcode)){
    if(billtype_check(btcode[i])){
      str_len_to_col(btcode[i]) -> coll
      out <- billtype[which(billtype[,coll] == btcode[i]),]$facility_type[1]
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}

#' Extract Care-Type information from Billtype
#'
#' @param btcode a billtype code or vector of codes
#' @return a string or vector of strings with care-type info
#' @examples
#' get_caretype(c('0123','731'))
#' @export
get_caretype <- function(btcode){
  full_out <- c()
  for(i in 1:length(btcode)){
    if(billtype_check(btcode[i])){
      str_len_to_col(btcode[i]) -> coll
      out <- billtype[which(billtype[,coll] == btcode[i]),]$care_type[1]
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}

#' Extract Frequency information from Billtype
#'
#' @param btcode a billtype code or vector of codes
#' @return a string or vector of strings with frequency info
#' @examples
#' get_frequency(c('0123','731'))
#' @export
get_frequency <- function(btcode){
  full_out <- c()
  for(i in 1:length(btcode)){
    if(billtype_check(btcode[i])){
      str_len_to_col(btcode[i]) -> coll
      out <- billtype[which(billtype[,coll] == btcode[i]),]$frequency_type[1]
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}

