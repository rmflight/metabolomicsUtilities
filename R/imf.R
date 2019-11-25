#' get number of c13 incorporations
#'
#' Given a vector of isotopic molecular formula's (IMFs), attempts to determine
#' how many C13 incorporations are present. Isotopic element counts should be
#' separated by ","
#'
#' @param imfs vector of character formulas
#' @param add_one should **1** be added (to avoid zero)?
#'
#' @return integer
#' @export
#' @importFrom purrr map_int
mu_numberc13 = function(imfs, add_one = FALSE){
  split_imfs = strsplit(imfs, ",")

  out_c13 = purrr::map_int(split_imfs, function(in_imf){
    c13 = grep("^13C", in_imf, value = TRUE)
    if (length(c13) == 1) {
      return(gsub("13C", "", c13) %>% as.integer())
    } else {
      return(0L)
    }
  })
  if (add_one) {
    out_c13 = out_c13 + 1
  }
  return(out_c13)
}
