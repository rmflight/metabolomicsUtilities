#' extract parts of a value
#'
#' Often we want to transform a number into it's exponential representation,
#' having the number itself and the number of decimal places. This function
#' provides that functionality
#'
#' @param x the number to extract the parts from
#' @return list
extract <- function(x){
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x/10^e
  list(mantissa = m, exponent = e)
}

#' create a value from mantissa and exponent
#'
#' given a mantissa and exponent, returns the actual value as a numeric
#'
#' @param mantissa the base part of the number
#' @param exponent the exponent part
#'
#' @return numeric
create_value <- function(mantissa, exponent){
  mantissa * 10 ^ exponent
}


#' log-transform
#'
#' performs a log-transform while adding a small value to the data based on
#' finding the smallest non-zero value in the data
#'
#' @param