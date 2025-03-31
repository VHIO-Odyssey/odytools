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

#' Print a RedCap dataset list
#'
#' @param x RedCap dataset list created inside an Odytools REDCap project.
#' @param ... Additional arguments (added for compatibility with base::print).
#'
#' @exportS3Method base::print
print.odytools_datasets_list <- function(x, ...) {

  descriptions_v0 <- purrr::map(x, ~ attr(., "description"))
  descriptions_v1 <-
    purrr::map(
      descriptions_v0, ~ ifelse(is.null(.), "No description", .)
    )
  descriptions <-
    purrr::map2(
      names(x),
      descriptions_v1,
      ~ stringr::str_c(.x, ": ", .y)
    )

  description_vector <-
    stringr::str_c(
      "c(", stringr::str_c("'*' = '", descriptions, "'") |>
        stringr::str_c(collapse = ", "), ")"
    ) |>
    str2lang()


  if (!is.null(attr(x, "project_title"))) {

  import_date <- attr(x, "import_date") |>
    stringr::str_extract("....-..-.. ..:..")

  project_name <- attr(x, "project_title")

  cli::cli_alert_info("{project_name} datasets from data imported on {import_date}")
  cli::cli_alert_info("{length(x)} elements:")
  cli::cli_bullets(eval(description_vector))

  } else {

    cli::cli_alert_info("{get_project_name()} datasets")
    cli::cli_alert_info("{length(x)} elements:")
    cli::cli_bullets(eval(description_vector))

  }

}


#' Print a RedCap dataset
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
