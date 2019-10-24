#' create correlation plot
#'
#' Given matrix data, create a correlation heatmap with reordering by group.
#'
#' @param matrix_data the matrix of data, variables by samples (r x c)
#' @param groups the groups of the samples
#' @param min_correlation the minimum correlation value to color
#'
#' @export
#' @importFrom visualizationQualityControl visqc_heatmap globally_it_weighted_pairwise_correlation visqc_heatmap similarity_reorderbyclass
#' @importFrom RColorBrewer brewer.pal
#' @importFrom circlize colorRamp2
#' @return NULL
mu_correlation_plot = function(matrix_data, groups = NULL, min_correlation = 0.5){
  data_cor = globally_it_weighted_pairwise_correlation(t(matrix_data), exclude_0 = TRUE, zero_value = min(matrix_data))
  if (is.null(groups)) {
    groups = data.frame(groups = rep("G", ncol(matrix_data)))
    rownames(groups) = colnames(matrix_data)
    data_order = similarity_reorderbyclass(data_cor$cor, groups[, "groups", drop = FALSE], transform = "sub_1")

    data_legend = RColorBrewer::brewer.pal(nrow(unique(groups)), "Set1")[1]
    names(data_legend) = sort(unique(groups[, 1]))


  } else {
    data_order = similarity_reorderbyclass(data_cor$cor, groups, transform = "sub_1")

    data_legend = RColorBrewer::brewer.pal(nrow(unique(groups)), "Set1")
    names(data_legend) = sort(unique(groups[, 1]))
  }

  data_row_label = groups
  data_annotation = list(v1 = data_legend)
  names(data_annotation) = names(groups)
  cor_colormap = circlize::colorRamp2(seq(min_correlation, 1, length.out = 20), viridis::viridis(20))

  cor_vals = data_cor$cor
  #rownames(cor_vals) = colnames(cor_vals) = gsub("_13C6.*", "", rownames(cor_vals))

  correlation_heatmap = visqc_heatmap(cor_vals, cor_colormap, "Sample Correlations",
                                      row_color_data = data_row_label, row_color_list = data_annotation,
                                      col_color_data = data_row_label, col_color_list = data_annotation,
                                      row_order = data_order$indices, column_order = data_order$indices)
  correlation_heatmap
}
