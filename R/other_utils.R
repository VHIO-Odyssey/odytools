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



#' Change column names
#'
#' Modify column names according to specified changes in a variables data frame.
#'
#' @param data_frame Data frame the names should by modified
#' @param names_df 2 columns data frame. The first column must contain the new names and the second one, the current ones.
#'
#' @return data_frame with names changes according to names_df.
#' @export
ody_change_names <- function(data_frame, names_df) { #CANDIDATE

  data_frame_names <- names(data_frame)

  names(names_df) <- c("new", "current")

  new_names <- purrr::map_chr(
    data_frame_names,
    function(x) {

      name <- dplyr::filter(names_df, .data$current == x) |>
        dplyr::pull("new")

      if (length(name) == 0) x else name

    }
  )

  names(data_frame) <- new_names

  data_frame

}

