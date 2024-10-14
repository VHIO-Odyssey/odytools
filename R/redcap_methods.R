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


#' Print a RedCap daraset
#'
#' @param x RedCap dataset created inside an Odytools REDCap project.
#' @param ... Additional arguments (added for compatibility with base::print).
#'
#' @exportS3Method base::print
print.odytools_dataset <- function(x, ...) {

  description <- attr(x, "description")

  if (is.null(description)) {
    NextMethod("print", x)
  } else {
    cli::cli_alert_info(description)
    NextMethod("print", x)
  }

}
