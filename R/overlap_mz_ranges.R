#' mz ranges to IRanges
#'
#' modifies a matrix of m/z ranges into an IRanges object to calculate
#' overlaps. Because IRanges objects are integer based, also multiplies
#' by a factor (default 10,000) to preserve resolution.
#'
#' @param mz_values either a vector or matrix of single or double ranges
#' @param multi_factor how much to multiply values by
#'
#' @importFrom IRanges IRanges
#' @export
#'
#' @return IRanges
mz_to_iranges <- function(mz_values, multi_factor = 10000){
  if (is.null(dim(mz_values))) {
    mz_ranges <- IRanges::IRanges(start = mz_values * multi_factor, width = 1)
  } else {
    mz_ranges <- IRanges::IRanges(start = mz_values[, 1] * multi_factor,
                                  end = mz_values[, 2] * multi_factor)
  }
  mz_ranges
}

#' Within ranges
#'
#' We rexport the %within% function from IRanges so it can be used by others
#' in this package.
#'
#' @importFrom IRanges %within%
#' @name %within%
#' @rdname within
#' @export
#' @param lhs,rhs A query and a subject
NULL