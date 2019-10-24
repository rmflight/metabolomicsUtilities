#' t-test
#'
#' runs t-tests on a vector, with classes of the vector supplied.
#'
#' @param data_vector the vector we are working with
#' @param class_data what class are each entry in
#' @param class_order what order should the classes be in for comparison?
#' @param ... other parameters for t.test
#'
#' @return data.frame
#' @importFrom stats t.test
#' @importFrom broom tidy
#' @export
ttest_vector <- function(data_vector, class_data, class_order = NULL, ...){
  na_res = data.frame(diff = 0, s1 = NA, s2 = NA, statistic = 1, p.value = 1,
                      parameter = NA, conf.low = NA, conf.high = NA,
                      method = "NA", alternative = "two.sided",
                      stringsAsFactors = FALSE)
  n_class = length(unique(class_data))
  if (n_class != 2) {
    stop_message = paste0("Your data has ", n_class, " classes. You must supply 2!")
    stop(stop_message)
  }
  split_data = split(data_vector, class_data)

  if (!is.null(class_order)) {
    split_data = split_data[class_order]
  }
  split_means = do.call(mean, split_data)

  # borrowed from stats:::t.test.default
  nx = length(split_data[[1]])
  mx = mean(split_data[[1]])
  vx = var(split_data[[1]])
  df = nx - 1
  stderr = sqrt(vx/nx)
  if (stderr < 10 * .Machine$double.eps * abs(mx)) {
    t_res = na_res
    t_res$s1 = split_means[[1]]
    t_res$s2 = split_means[[2]]
    names(t_res)[c(2,3)] = names(split_data)
  } else {
    t_res = stats::t.test(split_data[[1]], split_data[[2]])
    input_classes = names(split_data)
    t_res = broom::tidy(t_res)
    names(t_res)[1:3] = c("diff", input_classes)
  }

  # t_res2 = dplyr::rename_(t_res, diff = "estimate",
  #                        lazyeval::interp('x = estimate1', x = as.name(input_classes[1])),
  #                        lazyeval::interp(y = 'estimate2', y = as.name(input_classes[2])))
  t_res
}

#' matrix t-test
#'
#' @param data_matrix the matrix of data we are working with
#' @param class_data the classes of the columns
#' @param adjust_method how to adjust the p.values
#' @param ... other parameters for t.test
#'
#' @return data.frame
#' @importFrom stats p.adjust
#' @export
ttest_matrix <- function(data_matrix, class_data, adjust_method = "BH", ...){
  ttest_res <- lapply(seq_len(nrow(data_matrix)), function(in_row){
    ttest_vector(data_matrix[in_row, ], class_data, ...)
  })

  ttest_res <- do.call(rbind, ttest_res)
  ttest_res$p.adjust <- stats::p.adjust(ttest_res$p.value, method = adjust_method)
  ttest_res$feature <- rownames(data_matrix)
  ttest_res
}