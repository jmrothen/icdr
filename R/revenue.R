utils::data("revenuecode", envir=environment())


#' Validate revenuecode
#'
#' check if revenuecode code is valid
#'
#' @param rvcode a revenuecode code or vector of codes
#' @return boolean for validity
#' @examples
#' revcode_check('0450')
#' @export
revcode_check <- function(rvcode){
  if(stringr::str_length(rvcode) !=4){
    stop("that is not a valid revenuecode! don't forget your leading 0")
  }else{
    out <- grepl(rvcode,c(revenuecode[,'Code'],'0001'))
  }
  return(sum(out)>=1)
}


#' Extract Revenuecode group description
#'
#' @param rvcode a revenuecode code or vector of codes
#' @return a string or vector of strings with group description info
#' @examples
#' get_rev_group(c('0450','0111'))
#' @export
get_rev_group <- function(rvcode){
  full_out <- c()
  for(i in 1:length(rvcode)){
    if(revcode_check(rvcode[i])){
      out <- paste(
        revenuecode[which(revenuecode[,'Code'] == rvcode[i]),]$`Category`[1],
        revenuecode[which(revenuecode[,'Code'] == rvcode[i]),]$`Category Description`[1],
        sep= ' - '
      )
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}


#' Extract Revenuecode code description
#'
#' @param rvcode a revenuecode code or vector of codes
#' @return a string or vector of strings with description info
#' @examples
#' get_rev_desc(c('0450','0111'))
#' @export
get_rev_desc <- function(rvcode){
  full_out <- c()
  for(i in 1:length(rvcode)){
    if(revcode_check(rvcode[i])){
      out <- paste(
        revenuecode[which(revenuecode[,'Code'] == rvcode[i]),]$`Category Description`[1],
        revenuecode[which(revenuecode[,'Code'] == rvcode[i]),]$`Code Description`[1],
        sep=' - '
      )
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}


#' Extract Revenuecode parent code category
#'
#' For example, the parent of 0451 is 045X, where x is a filler character
#'
#' @param rvcode a revenuecode code or vector of codes
#' @return a string or vector of strings with parent info
#' @examples
#' get_rev_parent(c('0450','0111'))
#' @export
get_rev_parent <- function(rvcode){
  full_out <- c()
  for(i in 1:length(rvcode)){
    if(revcode_check(rvcode[i])){
      out <- revenuecode[which(revenuecode[,'Code'] == rvcode[i]),]$`Category`[1]
    }else{
      out <- 'Invalid Code'
    }
    full_out <- append(full_out,out)
  }
  return(full_out)
}




