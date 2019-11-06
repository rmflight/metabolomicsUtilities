#' Import EMF Classifications
#'
#' Allows the reading in of EMF classification results from a JSON file.
#'
#' @param classification_file the file to read from
#' @param fix_json properly format the JSON so the JSON parser can read it?
#' @param rm_na_formula remove those entries that have a formula entry of `NA`??
#'
#' @return data.frame
#' @export
import_emf_classifications = function(classification_file, fix_json = FALSE){
  if (fix_json) {
    emf_json = gsub("None", "null",
                    gsub("'", '"',
                         scan(classification_file, what = character(), sep = "\n", quiet = TRUE)
                    )
    )
    emf_classes = purrr::map(emf_json, jsonlite::fromJSON)
  } else {
    emf_classes = jsonlite::fromJSON(classification_file)
  }

  class_data_names = unique(unlist(purrr::map(emf_classes, names)))

  class_list = as.list(rep(NA, length(class_data_names)))
  names(class_list) = class_data_names
  class_tmp_df = as.data.frame(class_list)

  zero_frame = data.frame(Categories = NA, Classes = NA, isotopologue_EMF = "NA",
                          stringsAsFactors = FALSE)
  zero_frame = zero_frame[0, ]

  has_category = purrr::map_lgl(emf_classes, ~ length(.x$Categories) > 0)
  process_classes = emf_classes[has_category]
  class_data <- purrr::map2_dfr(process_classes, names(process_classes), function(in_list, in_emf){
    #message(in_emf)
    in_list$Categories = unique(in_list$Categories)
    in_category = stringr::str_extract(in_list$Categories, "\\[.*\\]|not\\_lipid")
    in_category = gsub("\\[|\\]", "", in_category)

    n_classes = length(in_list$Classes)

    if (n_classes == 0) {
      in_list$Classes = "none"
      tmp_frame = as.data.frame(in_list, stringsAsFactors = FALSE)
    } else {
      tmp_frame = purrr::map_df(seq(1, length(in_list$Categories)), function(tmp_row){
        match_category = grepl(in_category[tmp_row], in_list$Classes)
        if (sum(match_category) > 0) {
          match_frame = data.frame(Categories = in_list$Categories[tmp_row],
                                   Classes = in_list$Classes[match_category],
                                   stringsAsFactors = FALSE)
        } else {
          match_frame = data.frame(Categories = in_list$Categories[tmp_row],
                                   Classes = "none",
                                   stringsAsFactors = FALSE)
        }
        match_frame
      })

    }
    tmp_frame$isotopologue_EMF = in_emf

    tmp_frame
  })

  class_data
}
