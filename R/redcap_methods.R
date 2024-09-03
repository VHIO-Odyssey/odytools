#' Select variables from a RedCap import
#'
#' @param .data RedCap data imported with ody_rc_import.
#' @param ... Variable names to select. If the name of a form is provided, all the variables belonging to that form will be selected.
#'
#' @details
#' The funtion first tries to use dplyr::select.tbl_df to select the variables. If the variables are not found, the function uses ody_rc_select to extract the variables from the RedCap import.
#' See \link[odytools]{ody_rc_select} for further details on additional arguments.
#'
#' @return A tibble with the selected variables.
#' @exportS3Method dplyr::select
select.odytools_redcap <- function(.data, ...) {

  try <-
    .data |>
    dplyr::as_tibble() |>
    purrr::safely(dplyr::select)(...)

  if (is.null(try$result)) {
    ody_rc_select(.data, ...)
  } else {
    try$result
  }

}

#' Print a RedCap import
#'
#' @param x RedCap data imported with ody_rc_import.
#' @param ... Additional arguments (added for compatibility with base::print).
#'
#' @exportS3Method base::print
print.odytools_redcap <- function(x, ...) {

  project_name <- stringr::str_c(
    "REDCap Project {.strong ", attr(x, "project_info")$project_title, " (PID ",attr(x, "project_info")$project_id, ")}"
  )
  import_date <- attr(x, "import_date") |>
    stringr::str_extract("....-..-.. ..:..")
  import_text <- stringr::str_c("Imported on ", import_date)

  cli::cli_alert_info(project_name)
  cli::cli_alert_info(import_text)
  NextMethod("print", x)

}
