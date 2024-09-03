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

  NextMethod("print", x)
  cli::cli_alert_info(project_name)
  cli::cli_alert_info(import_text)

}
