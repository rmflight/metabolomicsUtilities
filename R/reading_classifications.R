#' Import EMF Classifications
#'
#' Allows the reading in of EMF classification results from a JSON file.
#'
#' @param classification_file the file to read from
#' @param fix_json properly format the JSON so the JSON parser can read it?
#' @param remove_categories a named list of categories and classes to remove (see Details)
#'
#' @details `remove_categories` is used to define which categories and sub-classes of
#'   lipids to remove. The default is a list for two classes in *Sphingolipids* that
#'   are known to be over assigned, *neutral glycosphingolipics* and *acidic glycosphingolipids*.
#'   The top name of the list defines the *Category* level, and then the text entries
#'   underneath define the *Classes* to remove. See examples for different ways to
#'   define this list that should do different things.
#'
#' @examples
#' ## don't run these
#' \dontrun{
#'
#'   # turn off filtering behavior
#'   import_emf_classifications("categories.json", remove_categories = NULL)
#'
#'   # filter ALL Sphingolipids
#'   import_emf_classifications("categories.json", remove_categories = list(
#'     Sphingolipids = NULL))
#'
#'   # filter Ceramides
#'   import_emf_classifications("categories.json", remove_categories = list(Sphingolipids = "ceramides"))
#' }
#'
#' @return data.frame
#' @export
import_emf_classifications = function(classification_file, fix_json = FALSE,
                                      remove_categories =
                                        list(Sphingolipids = c("neutral glycosphingolipids",
                                                               "acidic glycosphingolipids"))){
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

  # attempts to remove categories and classes set by the user
  # Note that each one is done independently
  # We also add the "^" to the regex, because we assume that whatever it is
  # occurs should match at the beggining of the line
  if (!is.null(remove_categories)) {

    for (icategory in names(remove_categories)) {
      matches_removal = rep(FALSE, nrow(class_data))
      regex_category = paste0("^", icategory)

      if (length(remove_categories[[icategory]]) > 0) {
        remove_classes = remove_categories[[icategory]]
        for (iclass in remove_classes) {
          regex_classes = paste0("^", iclass)
          matches_removal = matches_removal | (grepl(regex_category, class_data$Categories, ignore.case = TRUE) & grepl(regex_classes, class_data$Classes, ignore.case = TRUE))
        }
      } else {
        matches_removal = matches_removal | grepl(regex_category, class_data$Categories, ignore.case = TRUE)
      }
      class_data = class_data[!matches_removal, ]
    }

  }
  class_data
}
