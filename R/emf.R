#' calculate emf sums
#'
#' For a given set of EMF data, calculates a nominal sum, or a set of weighted
#' sums where the weights are based on the number of C13 incorporations.
#'
#' @param normalized_data a matrix of normalized intensities
#' @param emf which emf are we working with
#' @param peak_info an optional data.frame of peak information
#'
#' @details If `peak_info` is `NULL` (not supplied), then the *nominal* sum is
#'   returned. If `peak_info` is supplied, then the weighted sums for each different
#'   EMF is returned. Note that the rownames of `normalized_data` should correspond
#'   to the `PeakID` in `peak_info`, and `PeakID`, `c13`, `complete_EMF` and `complete_IMF` must
#'   all be present.
#'
#' @export
#' @return A data.frame or list
mu_emf_sum = function(normalized_data, emf, peak_info = NULL){
  #message(emf)
  method = "nominal"
  if (is.null(peak_info)) {
    sums = colSums(normalized_data)

    sum_df = data.frame(sum = sums, sample = names(sums),
                        method = method, emf = emf, stringsAsFactors = FALSE)

  } else if (!is.null(peak_info)) {
    missing_peaks = rownames(normalized_data)[!(rownames(normalized_data) %in% peak_info$PeakID)]
    if (length(missing_peaks) > 0) {
      missed_peaks = paste0(missing_peaks, collapse = ", ")
      stop(paste0("These peaks are missing from peak_info: ", missed_peaks))
    }
    need_fields = c("PeakID", "c13", "complete_EMF", "complete_IMF")
    has_required_fields = all(need_fields %in% names(peak_info))
    if (!has_required_fields) {
      missing_fields = paste0(need_fields[!(need_field %in% names(peak_info))], collapse = ", ")
      stop(paste0("The following fields are missing from peak_info: ", missing_fields))
    }
    method = "weighted"
    sub_info = peak_info[peak_info$PeakID %in% rownames(normalized_data), need_fields] %>% unique() %>%
      split(., .[["complete_EMF"]])
    if (length(sub_info) > 1) {
      sub_info_multi = purrr::map_df(sub_info, ~ dplyr::mutate(.x, c13_diff = c13 - c13[1] + 1)) %>%
        dplyr::group_by(c13_diff) %>% dplyr::arrange(c13, .by_group = TRUE)
      sub_split = dplyr::ungroup(sub_info_multi) %>%
        split(., .$complete_EMF)
    } else {
      sub_split = sub_info
    }

    sum_df = purrr::map_df(seq(sub_split), function(in_sub){
      #message(in_sub)
      tmp_info = sub_split[[in_sub]]

      if ((nrow(tmp_info) == nrow(normalized_data)) && (all(tmp_info$PeakID %in% rownames(normalized_data)))) {
        w_matrix = matrix(tmp_info$c13, nrow = nrow(tmp_info), ncol = ncol(normalized_data), byrow = FALSE)
        tmp_norm = normalized_data[tmp_info$PeakID, , drop = FALSE]
        weight_norm = tmp_norm * w_matrix
        sums = colSums(weight_norm)

        sub_df = data.frame(sum = sums, sample = names(sums),
                            method = method, emf = emf, complete_EMF = tmp_info$complete_EMF[1],
                            in_emf = in_sub, stringsAsFactors = FALSE)

      } else {
        sub_df = data.frame(sum = NA, sample = colnames(normalized_data),
                            method = "NA", emf = emf,
                            complete_EMF = "NA",
                            in_emf = in_sub, stringsAsFactors = FALSE)
      }
      sub_df
    })

  }

  sum_df

}

#' normalize emf
#'
#' Given an EMF matrix, normalize and replace NAs with a threshold value
#'
#' @param emf the EMF matrix of values
#' @param normalization_factors the vector of normalization values
#' @param threshold_value the threshold value to replace NA's
#'
#' @export
#' @return matrix
mu_normalize_emf = function(emf, normalization_factors, threshold_value = NULL){
  tmp_norm = mu_apply_normalization(emf, normalization_factors)
  if (!is.null(threshold_value)) {
    tmp_norm[is.na(tmp_norm)] = threshold_value
  }
  tmp_norm
}