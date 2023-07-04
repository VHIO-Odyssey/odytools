#' Get a version date for a render.
#'
#' @param file_name File name.
#' @param extension File extension.
#'
#' @details The function looks for the possible files named as file_name followed by today's date in the current wd. If none, the current date is returned as version indicator. If any, a number is added to the date.
#'
#' @return A character
#' @export
ody_add_version <- function(file_name, extension = "html") {

  today_num <- lubridate::today() |>
    stringr::str_remove_all("-")
  today_present <- list.files() |>
    stringr::str_detect(stringr::str_c(file_name, "_", today_num)) |>
    any()
  today_present_mult <- list.files() |>
    stringr::str_detect(stringr::str_c(file_name, "_", today_num, "_\\d")) |>
    any()

  if (today_present & !today_present_mult) {
    current_ver <- stringr::str_c("_", today_num, "_2")
  } else if (today_present_mult) {
    current_ver <- list.files() |>
      stringr::str_extract(stringr::str_c(today_num, "_\\d")) |>
      unique() |>
      na.omit() |>
      stringr::str_extract("\\d$") |>
      as.numeric() |>
      max()

    current_ver <- stringr::str_c("_", today_num, "_", current_ver + 1)
  } else {

    current_ver <- stringr::str_c("_", today_num)

  }

  stringr::str_c(file_name, current_ver, ".", extension)

}
