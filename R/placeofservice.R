utils::data("placeofservice", envir=environment())


#' Validate placeofservice
#'
#' check if placeofservice code is valid
#'
#' @param poscode a placeofservice code or vector of codes
#' @return boolean for validity
#' @examples
#' pos_check('11')
#' @export
pos_check <- function(poscode){
  if(stringr::str_length(poscode) !=2){
    stop("a valid pos code require 2 characters")
  }else{
    out <- grepl(poscode,placeofservice[,'code'])
  }
  return(sum(out)>=1)
}




#' Extract POS name
#'
#' @param poscode a placeofservice code or vector of codes
#' @return a vector of corresponding pos names
#' @examples
#' get_pos_name(c('11','71'))
#' @export
get_pos_name <- function(poscode){
  full_out <- c()
  for(i in 1:length(poscode)){
    if(pos_check(poscode[i])){
      out <- placeofservice[which(placeofservice$code==poscode[i]),]$name
    }else{
      out <- 'Invalid POS code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}



#' Extract POS description
#'
#' @param poscode a placeofservice code or vector of codes
#' @return a vector of corresponding pos descriptions
#' @examples
#' get_pos_desc(c('11','71'))
#' @export
get_pos_desc <- function(poscode){
  full_out <- c()
  for(i in 1:length(poscode)){
    if(pos_check(poscode[i])){
      out <- placeofservice[which(placeofservice$code==poscode[i]),]$description
    }else{
      out <- 'Invalid POS code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}
