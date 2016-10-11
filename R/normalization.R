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


#' log-transform data
#'
#' performs a log-transform while adding a small value to the data based on
#' finding the smallest non-zero value in the data
#'
#' @param data_matrix the data to work with
#' @param min_value the minimum value
#' @param order_mag how many orders of magnitute smaller should min value be?
#' @param log_fun what log function to use for the transformation
#'
#' @export
#' @return matrix
log_with_min <- function(data_matrix, min_value = NULL, order_mag = 3, log_fun = log){
  stopifnot(class(data_matrix) != "matrix")
  stopifnot(class(data_matrix) != "numeric")

  if (min(data_matrix) < 0) {
    stop("Values less than zero detected, aborting!")
  }

  if (is.null(min_value)) {
    min_value <- min(data_matrix[data_matrix > 0])
  }

  split_min <- extract(min_value)
  split_min$exponent <- split_min$exponent - order_mag
  add_value <- create_value(split_min$mantissa, split_min$exponent)

  out_matrix <- data_matrix + add_value
  log_matrix <- log_fun(out_matrix)
  log_matrix
}