
#' add annotations
#'
#' Given a data.frame with `feature_id` and `log2FoldChange`, and an annotation
#' object with lipid classes, `total_length`, `total_db`, `chain_length`, and `chain_db`,
#' constructs a data.frame of annotations, and prepares the log-fold-change for plotting.
#'
#' @param log_df data.frame of log-fold-changes
#' @param feature_annotations a categoryCompare feature annotations object
#'
#' @export
#' @return list
mu_add_annotations = function(log_df, feature_annotations)
{
  # log_df = readRDS("testing_stuff/example_up_down.rds")
  # feature_annotations = readRDS("testing_stuff/up_down_lipids.rds")

  annotation_list = feature_annotations@annotation_features

  annotation_names = names(annotation_list)


  class_annotations = grepl("^class\\:", annotation_names)
  total_length = grepl("^total_length\\:", annotation_names)
  total_db = grepl("^total_db\\:", annotation_names)
  chain_length = grepl("^chain[1|2|3]_length\\:|^chain_length\\:", annotation_names)
  names(annotation_list)[chain_length] = gsub("^chain[1|2|3]_length", "chain_length", annotation_names[chain_length])
  chain_db = grepl("^chain[1|2|3]_db\\:|^chain_db\\:", annotation_names)
  names(annotation_list)[chain_db] = gsub("^chain[1|2|3]_db", "chain_db", annotation_names[chain_db])

  class_table = purrr::imap(annotation_list[class_annotations], \(features, id){
    tibble::tibble(feature_id = features,
                   type = "class",
                   value = stringr::str_split_i(id, "\\:", 2))
  }) |>
    purrr::list_rbind()

  t_length_table = purrr::imap(annotation_list[total_length], \(features, id){
    tibble::tibble(feature_id = features,
                   type = "total_length",
                   value = stringr::str_split_i(id, "\\:", 2))
  }) |>
    purrr::list_rbind()

  t_db_table = purrr::imap(annotation_list[total_db], \(features, id){
    tibble::tibble(feature_id = features,
                   type = "total_db",
                   value = stringr::str_split_i(id, "\\:", 2))
  }) |>
    purrr::list_rbind()

  c_length_table = purrr::imap(annotation_list[chain_length], \(features, id){
    tibble::tibble(feature_id = features,
                   type = "chain_length",
                   value = stringr::str_split_i(id, "\\:", 2))
  }) |>
    purrr::list_rbind()

  c_db_table = purrr::imap(annotation_list[chain_db], \(features, id){
    tibble::tibble(feature_id = features,
                   type = "chain_db",
                   value = stringr::str_split_i(id, "\\:", 2))
  }) |>
    purrr::list_rbind()

  all_annotations = dplyr::bind_rows(class_table,
                                     t_length_table,
                                     t_db_table,
                                     c_length_table,
                                     c_db_table)

  log_df = log_df |>
    dplyr::mutate(direction = sign(log2FoldChange),
                  direction_char = dplyr::case_when(
                    direction == -1 ~ "neg",
                    direction == 1 ~ "pos"
                  ))

  return(list(up_down = log_df,
         annotations = all_annotations))

}


#' summarize the up and down-changed
#'
#' Given the output of `add_annotations`, gets the numbers of up- and down-changed
#' entries for each class, and then for the various `chain*` and `length*` within
#' each class.
#'
#' @param log_df_annotations list output from `add_annotations`
#'
#' @export
#' @return list
mu_summarize_up_down = function(log_df_annotations)
{
  annotation_df = log_df_annotations$annotations
  class_df = annotation_df |>
    dplyr::filter(type %in% "class")

  class_up_down = dplyr::inner_join(log_df_annotations$up_down, class_df, by = "feature_id")

  summary_class = class_up_down |>
    dplyr::group_by(value, direction_char) |>
    dplyr::summarise(n = sum(direction))

  use_classes = unique(summary_class$value)

  class_up_down = class_up_down |>
    dplyr::mutate(class = value,
                  type = NULL,
                  value = NULL)

  other_df = annotation_df |>
    dplyr::filter(!(type %in% "class"))
  class_up_down_other = dplyr::inner_join(class_up_down, other_df, by = "feature_id")

  summary_other = class_up_down_other |>
    dplyr::group_by(class, type, value, direction_char) |>
    dplyr::summarise(n = sum(direction))

  return(list(class = summary_class,
              other = summary_other))
}


#' create an up-down plot of classes
#'
#' Generate an up-down changed barplot for classes.
#'
#' @param class_summary the summary from `summarize_up_down`
#' @param class_order the subset / ordering of classes
#'
#' @export
#' @return ggplot2
mu_plot_up_down_classes = function(class_summary,
                              class_order = NULL)
{
  if (!require("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to create the plot!")
  }
  if (!is.null(class_order)) {
    class_summary = class_summary |>
      dplyr::filter(value %in% class_order)
    class_summary$value = factor(class_summary$value, levels = class_order,
                                 ordered = TRUE)
  } else {
    class_arrange = class_summary |>
      dplyr::group_by(value) |>
      dplyr::summarise(n_total = sum(abs(n))) |>
      dplyr::arrange(dplyr::desc(n_total))
    class_summary$value = factor(class_summary$value, levels = class_arrange$value,
                                 ordered = TRUE)
  }

  max_val = max(abs(class_summary$n))

  y_lim = c(-1 * max_val, max_val)


  out_plot = class_summary %>%
    ggplot(aes(x = value, y = n, fill = direction_char)) +
    scale_fill_discrete() +
    geom_bar(stat = "identity") +
    geom_hline(color = "black", yintercept = 0) +
    coord_cartesian(ylim = y_lim) +
    labs(subtitle = "All Classes", x = "Class", y = "N Upchanged / Downchanged") +
    theme(legend.position = "none")
  return(out_plot)
}

#' up-down plot within a class
#'
#' Generate sets of up-down plots within classes
#'
#' @param other_summary the summary from `summarize_up_down`
#' @param which_class which class to create the plots for
#'
#' @export
#' @return ggplot2
mu_plot_up_down_length_db = function(other_summary,
                                  which_class = NULL)
{
  if (!require("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to create the plot!")
  }
  if (!is.null(which_class)) {
    other_summary = other_summary |>
      dplyr::filter(class %in% which_class)
  }


  other_summary$type = factor(other_summary$type, levels = c("total_length", "total_db", "chain_length", "chain_db"), ordered = TRUE)

  split_type = split(other_summary, other_summary$type)

  out_types = purrr::map(split_type, \(in_type){
    if (nrow(in_type) > 0) {
      max_val = max(abs(in_type$n))
      in_type$value = as.numeric(in_type$value)
      in_type$direction_char = factor(in_type$direction_char, levels = c("pos", "neg"), ordered = TRUE)

      y_lim = c(-1 * max_val, max_val)

      if (grepl("db", in_type$type[1])) {
        x_lab = "# of Double Bonds"
      } else {
        x_lab = "# of Carbons"
      }
      if (grepl("total", in_type$type[1])) {
        sub_title = "Total"
      } else {
        sub_title = "Chain"
      }
      y_lab = NULL

      if (in_type$type[1] %in% "total_length") {
        use_breaks = seq(min(in_type$value), max(in_type$value), 4)
      } else if (in_type$type[1] %in% "total_db") {
        use_breaks = seq(min(in_type$value), max(in_type$value), 4)
      } else if (in_type$type[1] %in% "chain_length") {
        use_breaks = seq(min(in_type$value), max(in_type$value), 2)
      } else if (in_type$type[1] %in% "chain_db") {
        use_breaks = seq(min(in_type$value), max(in_type$value), 2)
      }
      out_plot = in_type %>%
        ggplot(aes(x = value, y = n, fill = direction_char)) +
        scale_fill_discrete() +
        geom_bar(stat = "identity") +
        geom_hline(color = "black", yintercept = 0) +
        coord_cartesian(ylim = y_lim) +
        labs(subtitle = sub_title, x = x_lab, y = y_lab) +
        theme(legend.position = "none")
      return(out_plot)
    } else {
      return(NULL)
    }

  })

  null_plots = purrr::map_lgl(out_types, is.null)
  out_types = out_types[!null_plots]

  return(out_types)
}