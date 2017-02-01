#' convert peaks
#'
#' converts an XLSX peak list from Xcalibur to a json peak format.
#'
#' @param in_xlsx the file to work on
#' @param save_loc the directory to save the json file
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select_ filter_
#' @importFrom assertthat assert_that
#' @export
#' @return NULL
#'
xlsx_to_json <- function(in_xlsx, save_loc){
  xl_data <- readxl::read_excel(in_xlsx, col_names = TRUE, skip = 6)

  names(xl_data)[1:2] <- c("ObservedMZ", "Intensity")
  xl_data <- dplyr::select_(xl_data, "ObservedMZ", "Intensity")
  xl_data <- dplyr::filter_(xl_data, "!is.na(ObservedMZ)")

  assertthat::assert_that(is.data.frame(xl_data))
  xl_data2 <- list(Peaks = xl_data)
  json_data <- jsonlite::toJSON(xl_data2, auto_unbox = TRUE, pretty = TRUE,
                                digits = 8)
  out_json <- file.path(save_loc, sub("xlsx", "json", basename(in_xlsx)))
  cat(json_data, file = out_json)
}

#' create class from features
#'
#' Given a data.frame, take a feature column, and create a class column from it.
#'
#' @param data the data.frame to use
#' @param in_column the column that has the features
#' @param new_column the column to store the data in
#' @param sep the separator
#' @param n_sep how many separators are expected?
#' @param position which position the class is in after separation
#'
#' @examples
#'
#' data <- data.frame(feature = c("FA.15.0.15...NH4.C15H30O2..pos", "FA.16.2.16...NH4.C16H28O2..pos",
#'   "FA.18.2.18...NH4.C18H32O2..pos", "FA.19.2.19...NH4.C19H34O2..pos",
#'   "FA.21.1.21...H.C21H40O2..pos", "MAG.18.1.21...NH4.C21H40O4..pos",
#'   "FA.24.5.24...Na.C24H38O2..pos", "MAG.20.2.23...NH4.C23H42O4..pos",
#'   "DAG.22.2.25...NH4.C25H44O5..pos", "LysoPC.16.1.24...H.C24H48N1O7P1..pos"),
#'     stringsAsFactors = FALSE)
#'
#' data <- class_from_feature(data)
#'
#' @export
#' @importFrom stringr fixed
#'
class_from_feature <- function(data, in_column = "feature", new_column = "class", sep = ".", n_sep = 10, position = 1){

  features <- as.character(data[[in_column]])

  data[[new_column]] <- stringr::str_split_fixed(features, stringr::fixed(sep), n = n_sep)[,position]
  data
}

#' check class
#'
#' runs a check that the class can actually be found in the feature
#'
#' @param data the data.frame to work on
#' @param feature_colum which column has the feature
#' @param class_column which columns has the class
#'
#' @export
#'
#' @importFrom stringr str_detect
#'
check_feature_class <- function(data, feature_column = "feature", class_column = "class"){
  feature_data <- as.character(data[[feature_column]])
  class_data <- as.character(data[[class_column]])

  class_in_feature <- stringr::str_detect(feature_data, class_data)

  if (sum(class_in_feature) == length(feature_data)) {
    message("All classes match to their features!")
  } else {
    warning("Some classes do NOT match to their features!")
  }
  data[!class_in_feature, ]
}