# ody_rd_import helper to extract content
extract_data <- function(content, token, url) {
  httr::POST(
    url,
    body = list(
      "token" = token,
      content = content,
      format = "csv",
      returnFormat = "csv"
    ),
    encode = "form"
  ) |>
    httr::content()
}


#' Import a RedCap project
#'
#' @param token API token provided by RedCap.
#' @param url RedCap server. The default is the server of VHIO.
#'
#' @return A nested tibble
#' @export
ody_rd_import <- function(
    token = NULL, url = "https://redcap.vhio.net/redcap/api/"
  ) {

  if (is.null(token)) {
    token <- rstudioapi::askForPassword(
      prompt = "Please enter a RedCap token: "
    )
  }

  # Data import
  import_date <- Sys.time()

  formData <- list(
    "token"=token,
    content='record',
    action='export',
    format='csv',
    type='flat',
    csvDelimiter='',
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )

  redcap_data <- httr::POST(url, body = formData, encode = "form") |>
    httr::content(
      na = "", col_types = readr::cols(.default = readr::col_character())
    )

  # Metadata imports
  project_info <- extract_data("project", token, url)
  metadata <- extract_data("metadata", token, url)
  forms <- extract_data("instrument", token, url)
  events <- extract_data("event", token, url)
  forms_event_mapping <- extract_data("formEventMapping", token, url)
  repeating <- extract_data("repeatingFormsEvents", token, url)
  arms <- extract_data("arm", token, url)

  # Indentifying variable
  id_var <- colnames(redcap_data)[1]

  # Project-level missing codes
  missing_codes <- project_info$missing_data_codes |>
    stringr::str_split("\\|", simplify = TRUE) |>
    stringr::str_trim() |>
    stringr::str_split_fixed(",", n = 2) %>%
    {
      tibble::tibble(
        raw_value = .[, 1],
        label = stringr::str_trim(.[, 2])
      )
    } |>
    dplyr::mutate(
      raw_value = tidyr::replace_na(
        raw_value, "No missing codes in this project."
      )
    )

  # attributes
  attr(redcap_data, "project_info") <- project_info
  attr(redcap_data, "metadata") <- metadata
  attr(redcap_data, "forms") <- forms
  if (stringr::str_detect(colnames(events)[1], "ERROR", negate = TRUE)) {
    attr(redcap_data, "events") <- events
  }
  if (stringr::str_detect(
    colnames(forms_event_mapping)[1], "ERROR", negate = TRUE)
  ) {
    attr(redcap_data, "forms_events_mapping") <- forms_event_mapping
  }
  if (stringr::str_detect(colnames(repeating)[1], "ERROR", negate = TRUE)) {
    attr(redcap_data, "repeating") <- repeating
  }
  if (stringr::str_detect(colnames(arms)[1], "ERROR", negate = TRUE)) {
    attr(redcap_data, "arms") <- arms
  }
  attr(redcap_data, "phantom_variables") <- metadata |>
    dplyr::mutate(
      present = field_name %in% colnames(data)
    ) |>
    dplyr::filter(!present) |>
    dplyr::select(field_name, field_type, form_name)
  attr(redcap_data, "missing_codes") <- missing_codes
  attr(redcap_data, "id_var") <- id_var
  attr(redcap_data, "subjects") <- redcap_data |>
    dplyr::pull(dplyr::all_of(id_var)) |>
    unique()
  attr(redcap_data, "import_date") <- import_date

  # Delete unnecessary attributes
  attr(redcap_data, "names") <- NULL
  attr(redcap_data, "row.names") <- NULL
  attr(redcap_data, "spec") <- NULL
  attr(redcap_data, "problems") <- NULL

  redcap_data

}
