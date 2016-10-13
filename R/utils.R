#' convert peaks
#'
#' converts an XLSX peak list from Xcalibur to a json peak format.
#'
#' @param in_xlsx the file to work on
#' @param save_loc the directory to save the json file
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select_ filter_
#' @importFrom SIRM.FTMS.peakPickingMethods peak_list_2_json
#' @export
#' @return NULL
#'
xlsx_to_json <- function(in_xlsx, save_loc){
  xl_data <- readxl::read_excel(in_xlsx, col_names = TRUE, skip = 6)

  names(xl_data)[1:2] <- c("ObservedMZ", "Intensity")
  xl_data <- dplyr::select_(xl_data, "ObservedMZ", "Intensity")
  xl_data <- dplyr::filter_(xl_data, !is.na("ObservedMZ"))

  json_data <- SIRM.FTMS.peakPickingMethods::peak_list_2_json(xl_data)
  out_json <- file.path(save_loc, sub("xlsx", "json", basename(in_xlsx)))
  cat(json_data, file = out_json)
}
