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
#' @param data the data to work with
#' @param min_value the minimum value
#' @param order_mag how many orders of magnitute smaller should min value be?
#' @param log_fun what log function to use for the transformation
#'
#' @export
#' @return matrix
log_with_min <- function(data, min_value = NULL, order_mag = 3, log_fun = log){
  #stopifnot(class(data_matrix) == "matrix")
  #stopifnot(class(data_matrix) == "numeric")

  if (min(data) < 0) {
    stop("Values less than zero detected, aborting!")
  }

  if (is.null(min_value)) {
    min_value <- min(data[data > 0])
  }

  split_min <- extract(min_value)
  split_min$exponent <- split_min$exponent - order_mag
  add_value <- create_value(split_min$mantissa, split_min$exponent)

  out_data <- data + add_value
  log_data <- log_fun(out_data)
  log_data
}

#' return all peak heights
#'
#' @param zip_file the zip file for a sample
#'
#' @export
#' @return data.frame
mu_peak_intensity = function(zip_file){
  if (require("FTMS.peakCharacterization")) {
    tmp_zip = FTMS.peakCharacterization::zip_ms(zip_file)
    tmp_zip$load_peak_finder()
    tmp_zip$cleanup()
    sample_id = tmp_zip$id
    all_intensity = data.frame(intensity = tmp_zip$peak_finder$peak_regions$peak_data$Height,
                               sample = sample_id,
                               stringsAsFactors = FALSE)
    return(all_intensity)
  } else {
    stop("FTMS.peakCharacterization is required for this functionality!")
  }

}

#' calculate sample normalization
#'
#' Given a set of peak intensities, calculate normalization factors using
#' the log-median of the intensities.
#'
#' @param intensity_df the data.frame of intensities
#'
#' @export
#' @return data.frame
mu_calc_normalization = function(intensity_df){
  split_sample = split(intensity_df$intensity, intensity_df$sample)

  medians = purrr::map_dbl(split_sample, median, na.rm = TRUE)
  return(medians)
}

#' apply normalization
#'
#' Apply normalization to a matrix of intensities. This uses division, so
#' if you had something logged, you would want to do the exponent first.
#'
#' @param intensity_matrix matrix of intensities, rows are entries, columns are samples
#' @param normalization_factors named vector of normalization factors
#'
#' @export
#' @return matrix
mu_apply_normalization = function(intensity_matrix, normalization_factors, transform = NULL){
  match_names = base::intersect(colnames(intensity_matrix), names(normalization_factors))
  other_names = setdiff(colnames(intensity_matrix), names(normalization_factors))

  if (length(other_names) > 0) {
    warning("Some sample names are missing, these samples will be normalized using a value of 1!")

    other_factors = rep(1, length(other_names))
    names(other_factors) = other_names
    normalization_factors = c(normalization_factors, other_factors)
  }
  normalization_factors = normalization_factors[colnames(intensity_matrix)]

  normalization_matrix = matrix(normalization_factors, nrow = nrow(intensity_matrix),
                                ncol = length(normalization_factors), byrow = TRUE)

  normalized_matrix = intensity_matrix / normalization_matrix
  normalized_matrix
}

#' calculate imputed threshold
#'
#' Calculate a reasonable threshold value to replace zeros
#'
#' @param intensity_df intensity values across many samples
#' @param value_transform how much to scale the value (default = 1/2)
#'
#' @details Not reported or missing values in our data cause all kinds of problems,
#'   and for data with proportional error, log-transforms mean we can't just set
#'   them to 0 either. In addition, just setting to zero inflates the differences
#'   of values. So, a reasonable value for noise is 1/2 of the lowest observed value
#'   in a *normal* like distribution. To achieve that for data with proportional data,
#'   we do a log-transform first. Because we want to use *all* the data across samples,
#'   we might have some weird outliers too. So we don't use the values directly
#'   from the distrubtion, but use `boxplot.stats` to get a reasonable handle
#'   on the distribution as well, and take a fraction of the lowest value in the
#'   distribution description.
#'
#' @export
#' @importFrom grDevices boxplot.stats
#' @return double
mu_calculate_threshold = function(intensity_df, value_transform = 1/2){
  intensity_values = intensity_df$intensity
  log_values = log(intensity_values)
  log_stats = boxplot.stats(log_values)
  threshold_value = value_transform * min(log_stats$stats)
  exp(threshold_value)
}