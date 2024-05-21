# Helper function to extract content in import_rc
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
    httr::content(show_col_types = FALSE) |>
    suppressWarnings()

}


# Helper function to import a RedCap project
import_rc <- function(
    token = NULL,  form = NULL, url = "https://redcap.vhio.net/redcap/api/"
  ) {

  # Data import
  import_date <- Sys.time()

  metadata <- extract_data("metadata", token, url)

  if (is.null(form)) {

    formData <- list(
      "token" = token,
      content = 'record',
      action = 'export',
      format = 'csv',
      type = 'flat',
      csvDelimiter = '',
      rawOrLabel = 'raw',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'true',
      returnFormat = 'json'
    )

  } else {

    id_var <- metadata[[1]][1]

    formData <- list(
      "token" = token,
      content = 'record',
      action = 'export',
      format = 'csv',
      type = 'flat',
      csvDelimiter = '',
      rawOrLabel = 'raw',
      rawOrLabelHeaders = 'raw',
      'fields[0]' = id_var,
      'forms[0]' = form,
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'true',
      returnFormat = 'json'
    )

  }

  redcap_data <- httr::POST(url, body = formData, encode = "form") |>
    httr::content(
      na = "", col_types = readr::cols(.default = readr::col_character())
    )

  if (!is.data.frame(redcap_data)) stop(redcap_data)

  # Metadata imports
  project_info <- extract_data("project", token, url)
  forms <- extract_data("instrument", token, url)
  events <- extract_data("event", token, url)
  forms_event_mapping <- extract_data("formEventMapping", token, url)
  repeating <- extract_data("repeatingFormsEvents", token, url)
  arms <- extract_data("arm", token, url)
  has_dag <- any(names(redcap_data) == "redcap_data_access_group")
  if (has_dag) {
    dag <- extract_data("dag", token, url)
    subjects_dag <- redcap_data |>
      dplyr::select(1, "redcap_data_access_group") |>
      unique()
    redcap_data <- redcap_data |>
      dplyr::select(-"redcap_data_access_group")
  }

  # Indentifying variable
  id_var <- colnames(redcap_data)[1]

  # Project-level missing codes
  missing_codes_v0 <- project_info$missing_data_codes |>
    stringr::str_split("\\|", simplify = TRUE) |>
    stringr::str_trim() |>
    stringr::str_split_fixed(",", n = 2)

  missing_codes <- tibble::tibble(
        raw_value = missing_codes_v0[, 1],
        label = stringr::str_trim(missing_codes_v0[, 2])
      ) |>
    dplyr::mutate(
      raw_value = tidyr::replace_na(
        .data[["raw_value"]], "No missing codes in this project."
      )
    )


  # Add to metadata complete_info variables to include them when nesting
  complete_vars <- stringr::str_c(forms$instrument_name, "_complete")
  avail_complete_vars <- names(redcap_data)[names(redcap_data) %in% complete_vars]
  complete_metadata <- tibble::tibble(
    field_name = avail_complete_vars,
    form_name = stringr::str_remove(avail_complete_vars, "_complete"),
    field_type = "complete_info",
    select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
  )
  metadata <- dplyr::bind_rows(metadata, complete_metadata)

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
      present = .data[["field_name"]] %in%
        stringr::str_remove(colnames(redcap_data), "___\\d+$")
    ) |>
    dplyr::filter(!.data[["present"]]) |>
    dplyr::select("field_name", "field_type", "form_name")
  attr(redcap_data, "checkbox_aux") <- names(redcap_data) |>
    stringr::str_subset("___.+$")
  attr(redcap_data, "missing_codes") <- missing_codes
  attr(redcap_data, "id_var") <- id_var
  attr(redcap_data, "subjects") <- redcap_data |>
    dplyr::pull(dplyr::all_of(id_var)) |>
    unique()
  if (has_dag) {
    attr(redcap_data, "dag") <- dag
    attr(redcap_data, "subjects_dag") <- subjects_dag
  }
  attr(redcap_data, "import_date") <- import_date

  # Delete unnecessary attributes
  attr(redcap_data, "spec") <- NULL
  attr(redcap_data, "problems") <- NULL

  redcap_data

}

# Helper function to label the imported dataframe from import_rc
label_rc_import <- function(rc_import) {

  metadata <- attr(rc_import, "metadata")
  missing_codes <- attr(rc_import, "missing_codes")
  id_var <- attr(rc_import, "id_var")


  # Dictionaries of all labeled variables
  field_dictionaries <- metadata |>
    dplyr::select("field_name", "select_choices_or_calculations") |>
    dplyr::filter(
      stringr::str_detect(
        .data[["select_choices_or_calculations"]], "\\d+,.+\\|"
      )
    ) |>
    dplyr::mutate(
      dictionary = purrr::map(
        .data[["select_choices_or_calculations"]],
        function(raw_dic) {
          codes <- stringr::str_replace_all(
            # trick to allow "|" inside the labels.
            raw_dic, "\\| *(\\d+ *,)", "[||] \\1"
          ) |>
            stringr::str_split("\\[\\|\\|\\]") |>
            unlist() |>
            stringr::str_split(",", n = 2) |>
            purrr::map(
              function(x) {
                x <- stringr::str_trim(x)
                c(
                  stringr::str_c("`", x[2], "`"),
                  stringr::str_c("'", x[1], "'")
                )
              }
            ) |>
            purrr::map(function(x) stringr::str_c(x, collapse = " = ")) |>
            stringr::str_c(collapse = ", ")
          stringr::str_c("c(", codes, ")") |> str2lang()
        }
      )
    ) |>
    dplyr::select(-"select_choices_or_calculations")


  # checkbox variables formating
  # All existing variables in the import
  available_vars <- names(rc_import) |>
    stringr::str_remove("___.+$") |>
    unique()

  checkbox_fields <- metadata |>
    dplyr::filter(
      .data[["field_type"]] == "checkbox",
      # Only variables present in the import are processed. This is needed because
      # the current import can contain the full databae or just one form.
      .data[[ "field_name"]] %in% available_vars
    ) |>
    dplyr::pull("field_name")

  for (checkbox_field in checkbox_fields) {
    cat(stringr::str_c("Processing checkbox variable ", checkbox_field, "\n"))
    # Columns involved in the definition of the selections
    field_cols <- colnames(rc_import) |>
      stringr::str_subset(
        stringr::str_c("^", checkbox_field, "___")
      )

    # Pulled selections (1s are replaced by the code value and 0s by NAs)
    pulled_selections <- rc_import |>
      dplyr::select(
        tidyselect::matches(
          stringr::str_c("^", checkbox_field, "___")
        )
      ) |>
      purrr::map2_dfc(
        field_cols,
        function(x, y) {
          ifelse(x == "1", stringr::str_split(y, "___")[[1]][2], NA)
        }
      ) |>
      apply(1, stringr::str_c, simplify = FALSE)

    # Translated values
    pulled_selections_char <- pulled_selections |>
      purrr::map_chr(
        function(x) {
          if (all(is.na(x))) {
            return(NA)
          }
          missing_selected <- stringr::str_to_lower(
            missing_codes$raw_value
          ) %in% x
          if (any(missing_selected)) {
            return(missing_codes$raw_value[missing_selected])
          }
          na.omit(x) |>
            stringr::str_c(collapse = ", ")
        }
      )

    # Checkbox dictionaries update
    ini_dic <- field_dictionaries |>
      dplyr::filter(.data[["field_name"]] == checkbox_field) |>
      dplyr::pull("dictionary") |>
      as.character()
    raw_dic <- metadata |>
      dplyr::filter(.data[["field_name"]] == checkbox_field) |>
      dplyr::pull("select_choices_or_calculations") |>
      stringr::str_split("\\|") |>
      unlist() |>
      stringr::str_trim() |>
      stringr::str_split(", ", n = 2) |>
      purrr::reduce(rbind)
    present_combinations_v0 <- pulled_selections_char |>
      na.omit() |>
      unique()

    present_combinations <- present_combinations_v0[
      stringr::str_detect(present_combinations_v0, ", ")
    ]

    if (length(present_combinations) != 0) {
      present_combinations_label <- purrr::map(
        present_combinations,
        function(x) {
          combination <- stringr::str_split(x, ", ")[[1]]
          purrr::map_chr(
            combination,
            function(x) raw_dic[raw_dic[, 1] == x, 2]
          ) |>
            stringr::str_c(collapse = ", ")
        }
      )
      added_dic <- stringr::str_c(
        "`", present_combinations_label, "` = '", present_combinations, "'"
      ) |>
        stringr::str_c(collapse = ", ") |>
        stringr::str_c(")")

      new_dic <- list(stringr::str_c(
        stringr::str_remove(ini_dic, "\\)$"),
        ", ",
        added_dic
      ) |> str2lang())
      field_dictionaries[
        field_dictionaries$field_name == checkbox_field, 2
      ][[1]] <- new_dic
    }
    # New variable is added.
    # It's named as the original variable so it will be picked up during
    # nesting.
    rc_import <- rc_import |>
      dplyr::mutate(
        "{checkbox_field}" := pulled_selections_char
      )
  }

  # Labeling
  for (field in metadata$field_name) {
    if (is.null(rc_import[[field]]) | field == id_var) next
    cat(stringr::str_c("Labelling ", field, "\n"))
    # Variable label
    form <- metadata |>
      dplyr::filter(.data[["field_name"]] == field) |>
      dplyr::pull("form_name")
    type <- metadata |>
      dplyr::filter(.data[["field_name"]] == field) |>
      dplyr::pull("field_type")
    format <- metadata |>
      dplyr::filter(.data[["field_name"]] == field) |>
      dplyr::pull("text_validation_type_or_show_slider_number")
    type_format <- stringr::str_c(na.omit(c(type, format)), collapse = ":")
    rc_import[[field]] <- labelled::labelled(
      rc_import[[field]],
      label = stringr::str_c(
        form,
        stringr::str_c(field, " (", type_format, ")"),
        sep = "/"
      )
    )

    # Values labels
    ## Defined by dictionary
    if (field %in% field_dictionaries$field_name) {
      dic_to_use <- field_dictionaries |>
        dplyr::filter(.data[["field_name"]] == field) |>
        dplyr::pull("dictionary")
      labelled::val_labels(rc_import[[field]]) <- eval(dic_to_use[[1]])
    }

    ## YesNo variables
    is_yes_no <- metadata |>
      dplyr::filter(.data[["field_name"]] == field) |>
      dplyr::pull("field_type") == "yesno"
    if (is_yes_no) {
      labelled::val_labels(rc_import[[field]]) <- c(No = "0", Yes = "1")
    }

    # Missing codes labels
    present_missing_codes_index <- missing_codes$raw_value %in%
      rc_import[[field]]
    if (any(present_missing_codes_index)) {
      present_missing_codes <- missing_codes$raw_value[
        present_missing_codes_index
      ]
      labelled::na_values(rc_import[[field]]) <- present_missing_codes
    }
  }

  rc_import
}


# Helper function to nest the imported project
nest_rc <- function(rc_raw) {

  id_var <- attr(rc_raw, "id_var")
  metadata <- attr(rc_raw, "metadata")
  repeating <- attr(rc_raw, "repeating")

  message("Nesting the project...\n")

  # Added structure for projects with no repating forms
  if (is.null(repeating)) {
    rc_raw <- rc_raw |>
      dplyr::mutate(
        redcap_event_name = "classic_project",
        redcap_repeat_instrument = NA,
        redcap_repeat_instance = NA,
        redcap_instance_type = "unique",
        redcap_instance_number = NA,
        .after = 1
      )
    repeating <- tibble::tibble(
      event_name = NA,
      form_name = NA
    )
  }

  # If the project has no events, the dummy event "classic_project" is added.
  if (names(rc_raw)[2] != "redcap_event_name") {
    rc_raw <- rc_raw |>
      dplyr::mutate(
        redcap_event_name = "classic_project", .after = 1
      )
    repeating <- repeating |>
      dplyr::mutate(
        event_name = "classic_project", .before = 1
      )

  }

  rc_raw <- rc_raw |>
    dplyr::mutate(
      redcap_instance_type = dplyr::case_when(
        is.na(.data[["redcap_repeat_instance"]]) ~ "unique",
        is.na(.data[["redcap_repeat_instrument"]]) ~ "event",
        TRUE ~ "form"
      ),
      redcap_instance_number = .data[["redcap_repeat_instance"]]
    )

  redcap_data <- rc_raw |>
    tidyr::nest(data_raw = -"redcap_event_name") |>
    dplyr::mutate(
      repeating_event = .data[["redcap_event_name"]] %in% (
        repeating |>
          dplyr::filter(is.na(.data[["form_name"]])) |>
          dplyr::pull("event_name")
      ),
      .before = "data_raw"
    ) |>
    dplyr::mutate(
      event_data = purrr::map2(
        .data[["data_raw"]], .data[["redcap_event_name"]],
        function(event_data, redcap_event_name) {
          purrr::map_dfr(
            unique(metadata$form_name),
            function(form) {
              form_fields <- metadata |>
                dplyr::filter(.data[["form_name"]] == form) |>
                dplyr::pull("field_name")
              # Variables selection
              if (event_data |>
                  dplyr::select(dplyr::any_of(form_fields)) |>
                  # id_var is extracted
                  dplyr::select(-dplyr::any_of(id_var)) |>
                  is.na() |>
                  all()
              ) {
                return(
                  tibble::tibble(
                    form_name = NA,
                    repeating_form = NA,
                    form_data = NA
                  )
                )
              }
              raw_result <- event_data |>
                dplyr::select(
                  # These variables are always selected.
                  dplyr::all_of(id_var),
                  "redcap_repeat_instrument",
                  "redcap_instance_type", "redcap_instance_number",
                  # list of variables from the current form.
                  dplyr::any_of(form_fields),
                  # Possible checkbox original variables
                  dplyr::any_of(
                    tidyselect::starts_with(
                      stringr::str_c(form_fields, "___")
                    )
                  )
                ) |>
                dplyr::mutate(
                  dplyr::across(
                    tidyselect::contains("___"),
                    function(x) as.logical(as.numeric(x))
                  )
                )
              # Rows selection
              is_repeating <- form %in% (
                repeating |>
                  dplyr::filter(
                    .data[["event_name"]] == redcap_event_name
                  ) |>
                  dplyr::pull("form_name")
              )
              if (is_repeating) {
                filtered_result <- raw_result |>
                  dplyr::filter(.data[["redcap_repeat_instrument"]] == form) |>
                  dplyr::select(-"redcap_repeat_instrument")
              } else {
                filtered_result <- raw_result |>
                  dplyr::filter(is.na(.data[["redcap_repeat_instrument"]])) |>
                  dplyr::select(-"redcap_repeat_instrument")
              }
              filtered_result |>
                dplyr::mutate(
                  form_name = form,
                  repeating_form = is_repeating,
                  .after = dplyr::all_of(id_var)
                ) |>
                tidyr::nest(form_data = c(-"form_name", -"repeating_form"))
            }
          ) |>
            dplyr::filter(!is.na(.data[["form_name"]]))
        }
      )
    ) |>
    dplyr::select(-"data_raw")

  # Clean artifacts
  # Need to know the complete variables to exclude them from the empty-form checks.
  complete_vars <- metadata |>
    dplyr::filter(field_type == "complete_info") |>
    dplyr::pull(field_name)
  redcap_data <- redcap_data |>
    dplyr::mutate(
      cleaned_data = purrr::map(
        .data[["event_data"]],
        function(rc_raw) {
          rc_raw |>
            dplyr::mutate(
              empty_index = purrr::map(
                .data[["form_data"]],
                function(vars) {
                  vars |>
                    dplyr::select(
                      -dplyr::any_of(
                        c(
                          id_var,
                          "redcap_instance_type",
                          "redcap_instance_number",
                          complete_vars
                        )
                      ),
                      -tidyselect::where(is.logical)
                    ) |>
                    apply(1, function(x) all(labelled::is_regular_na(x)))
                }
              ),
              variables_clean = purrr::map2(
                .data[["form_data"]], .data[["empty_index"]],
                function(x, y) dplyr::filter(x, !y)
              )
            ) |>
            dplyr::select("form_name", "repeating_form", "variables_clean") |>
            dplyr::rename(form_data = "variables_clean")
        }
      )
    ) |>
    dplyr::select(-"event_data") |>
    dplyr::rename(
      event_data = "cleaned_data",
      event_name = "redcap_event_name"
    )

  # aAdd redcap_ preffix to reduce coincidence chances with vars names.
  names(redcap_data) <- stringr::str_c("redcap_", names(redcap_data))
  for (i in 1:nrow(redcap_data)) {
    names(redcap_data$redcap_event_data[[i]]) <- stringr::str_c(
      "redcap_", names(redcap_data$redcap_event_data[[i]])
    )
  }

  if (nrow(redcap_data) == 1) {
    return(
      redcap_data |>
        dplyr::select("redcap_event_data") |>
        tidyr::unnest(cols = "redcap_event_data")
    )
  } else {
    return(redcap_data)
  }
}

# Helper function to pass the original import attibutes to the nested final
# data base. Also used in ody_rc_view to restore attributes to clasic projects.
restore_attributes <- function(rc_nested, rc_raw) {

  present_attributes <- names(attributes(rc_raw))

  possible_attributes <- c(
    "project_info", "metadata", "forms", "events",
    "forms_events_mapping", "repeating", "arms",
    "phantom_variables", "checkbox_aux", "missing_codes", "id_var",
    "subjects","subjects_dag", "dag", "import_date"
  )

  needed_attributes <-  possible_attributes[
    possible_attributes %in% present_attributes
  ]

  for (attribute in needed_attributes) {
    attr(rc_nested, attribute) <- attr(rc_raw, attribute)
  }

  rc_nested
}


# Helper funcion to clean the form directly from the import
rc_clean_single_form <- function(rc_import, form) {

  fields <- attr(rc_import, "metadata") |>
    dplyr::filter(.data[["form_name"]] == form) |>
    dplyr::pull("field_name")

  id_var <- attr(rc_import, "id_var")

  rows_with_data <- rc_import |>
    dplyr::select(tidyselect::all_of(fields)) |>
    apply(1, function(x) !all(is.na(x)))

  if (any(names(rc_import) == "redcap_repeat_instance")) {

  rc_import[rows_with_data, ] |>
    dplyr::mutate(
      redcap_form_name = form,
      redcap_instance_type = dplyr::case_when(
        is.na(.data[["redcap_repeat_instance"]]) ~ "unique",
        is.na(.data[["redcap_repeat_instrument"]]) ~ "event",
        TRUE ~ "form"
      ),
      redcap_instance_number = .data[["redcap_repeat_instance"]]
    ) |>
    dplyr::select(
      tidyselect::all_of(id_var),
      tidyselect::any_of("redcap_event_name"), "redcap_form_name",
      "redcap_instance_type", "redcap_instance_number",
      tidyselect::all_of(fields)
    )
  } else {

    rc_import[rows_with_data, ] |>
      dplyr::mutate(
        redcap_form_name = form,
        redcap_instance_type = "unique",
        redcap_instance_number = NA
      ) |>
      dplyr::select(
        tidyselect::all_of(id_var),
        "redcap_form_name",
        "redcap_instance_type", "redcap_instance_number",
        tidyselect::all_of(fields)
      )

  }

}


#' Import a RedCap Project
#'
#' @param token Project token. If not provided, a dialog promp will ask for it
#' @param form Form name to import. If NULL, all the forms will be imported.
#' @param url URL of the RedCap server (VHIO server by default).
#' @param label Logical. Should the variables be labelled according to the metadata?
#' @param nest Logical. Should the data be nested?
#'
#' @return A Tibble wiht metadata attributes (nested if nest = TRUE)
#' @export
ody_rc_import <- function(
    token = NULL, form = NULL, url = "https://redcap.vhio.net/redcap/api/",
    label = TRUE, nest = TRUE
  ) {

  if (is.null(token)) {
    token <- rstudioapi::askForPassword(
      prompt = "Please enter a RedCap token:"
    )
  }

  message("Importing data from RedCap\n")
  rc_raw_import <- import_rc(token, form, url)

  if (!label && !nest && is.null(form)) return(rc_raw_import)

  if (label) {
    rc_import <- label_rc_import(rc_raw_import)
  } else {
    rc_import <- rc_raw_import
  }

  if (!is.null(form)) return(rc_clean_single_form(rc_import, form))

  if (nest) {
    rc_import <- nest_rc(rc_import) |>
      restore_attributes(rc_raw_import)
  }

  rc_import
}


# Helper function of ody_rc_select to select variables in a longitudinal
# project.
select_rc_long <- function(rc_data, var_name, metadata, checkbox_aux) {

  if (
    sum(metadata$field_name == var_name) == 0 &&
    sum(checkbox_aux == var_name) == 0
  ) {
    stop(var_name, " does not exist.")
  }

  id_var <- attr(rc_data, "id_var")

  form_name <- metadata |>
    dplyr::filter(
      .data[["field_name"]] == stringr::str_remove(var_name, "___.+$")) |>
    dplyr::pull("form_name")

  rc_data |>
    dplyr::select("redcap_event_name", "redcap_event_data") |>
    tidyr::unnest(cols = "redcap_event_data") |>
    dplyr::filter(.data[["redcap_form_name"]] == form_name) |>
    tidyr::unnest(cols = "redcap_form_data") |>
    dplyr::select(
      dplyr::all_of(id_var), "redcap_event_name", "redcap_form_name",
      "redcap_instance_type", "redcap_instance_number", dplyr::all_of(var_name)
    )
}
# Helper function of ody_rc_select to select variables in a classic project
# with no events
select_rc_classic <- function(rc_data, var_name, metadata, checkbox_aux) {

  if (
    sum(metadata$field_name == var_name) == 0 &&
    sum(checkbox_aux == var_name) == 0
  ) {
    stop(var_name, " does not exist.")
  }

  id_var <- attr(rc_data, "id_var")

  form_name <- metadata |>
    dplyr::filter(
      .data[["field_name"]] == stringr::str_remove(var_name, "___.+$")) |>
    dplyr::pull("form_name")

  rc_data |>
    dplyr::filter(.data[["redcap_form_name"]] == form_name) |>
    tidyr::unnest(cols = "redcap_form_data") |>
    dplyr::select(
      dplyr::all_of(id_var),"redcap_form_name",
      "redcap_instance_type", "redcap_instance_number", dplyr::all_of(var_name)
    )

}


#' Select variables from a RedCap import
#'
#' @param rc_data RedCap data imported with ody_rc_import.
#' @param ... Variable names to select. If the name of a form is provided, all the variables belonguing to that form will be selected.
#' @param .is_vector Logical. If TRUE, the first element of ... is considered a character vector with the names of the variables to be selected.
#' @param .if_different_forms What action take if the selected variables belong to different forms.
#'    - list: It returns a list with an element for each form so only variables belonging to the same form are joinned in the same data frame.
#'    - join: Join all variables creating artifact NAs.
#' @param .include_aux When a form name is provided, all auxiliar checkbox variables will be added if .include_aux = TRUE
#'
#' @return A tibble with the selected variables.
#' @export
ody_rc_select <- function(rc_data,
                          ...,
                          .is_vector = FALSE,
                          .if_different_forms = c("list", "join"),
                          .include_aux = FALSE) {

  .if_different_forms <- rlang::arg_match(.if_different_forms)

  sel_vars <- purrr::map(
    rlang::enquos(...),
    rlang::quo_get_expr
  ) |> purrr::map(as.character) |>
    purrr::reduce(c) |>
    unique()

  # trick to permit character vectors
  if (.is_vector) {
      sel_vars <- get(sel_vars[1])
  }

  if (names(rc_data)[1] == "redcap_event_name") {
    select_rc_function <- select_rc_long
  } else {
    select_rc_function <- select_rc_classic
  }

  metadata <- attr(rc_data, "metadata")
  checkbox_aux <- attr(rc_data, "checkbox_aux")

  # If a form name is provided, all the variables of the form are extracted
  if (length(sel_vars) == 1 && sel_vars %in% unique(metadata$form_name)) {
    current_form <- metadata |>
      dplyr::filter(
        .data[["form_name"]] == sel_vars
      ) |>
      dplyr::pull("form_name") |>
      unique()

    # If the form contains phantom variables, the must be excluded since they do
    # not actually exist and the selection function would fail.
    phantom_vars <- attr(rc_data, "phantom_variables") |>
      dplyr::pull("field_name")

    sel_vars <- metadata |>
      dplyr::filter(
        .data[["form_name"]] == current_form,
        !(.data[["field_name"]] %in% phantom_vars)
      ) |>
      dplyr::pull("field_name")

    if (.include_aux) {
      checkbox_vars <- metadata |>
        dplyr::filter(
          .data$field_name %in% sel_vars,
          .data$field_type == "checkbox"
        ) |>
        dplyr::pull("field_name") |>
        stringr::str_c(collapse = "|")

      needed_aux <- stringr::str_subset(checkbox_aux, checkbox_vars)

      sel_vars <- c(sel_vars, needed_aux)

    }

  }

  if (.if_different_forms == "join") {

    purrr::map(
      sel_vars,
      function(x) select_rc_function(rc_data, x, metadata, checkbox_aux)
    ) |>
      purrr::reduce(dplyr::full_join) |>
      suppressMessages()

  } else {

    extracted_vars <- tibble::tibble(
      variables = purrr::map(
        sel_vars,
        function(x) select_rc_function(rc_data, x, metadata, checkbox_aux)
      ),
      form = purrr::map(
        .data$variables, ~.$redcap_form_name |> unique()
      )
    ) |>
      tidyr::nest(data = .data$variables)

    extracted_list <- purrr::map(
      extracted_vars[[2]],
      ~purrr::reduce(.[[1]], dplyr::full_join)
    ) |> suppressMessages()


    names(extracted_list) <- extracted_vars$form

    if (length(extracted_list) == 1) {
      extracted_list[[1]]
    } else {
      extracted_list
    }

  }

}

# Helper function used in viewer apps that directly assume variable is
# a vector
rc_select_viewer <- function(rc_data,
                             sel_vars,
                             .if_different_forms = c("list", "join"),
                             .include_aux = FALSE) {

  .if_different_forms <- rlang::arg_match(.if_different_forms)

  if (names(rc_data)[1] == "redcap_event_name") {
    select_rc_function <- select_rc_long
  } else {
    select_rc_function <- select_rc_classic
  }

  metadata <- attr(rc_data, "metadata")
  checkbox_aux <- attr(rc_data, "checkbox_aux")

  # If a form name is provided, all the variables of the form are extracted
  if (length(sel_vars) == 1 && sel_vars %in% unique(metadata$form_name)) {
    current_form <- metadata |>
      dplyr::filter(
        .data[["form_name"]] == sel_vars
      ) |>
      dplyr::pull("form_name") |>
      unique()

    # If the form contains phantom variables, the must be excluded since they do
    # not actually exist and the selection function would fail.
    phantom_vars <- attr(rc_data, "phantom_variables") |>
      dplyr::pull("field_name")

    sel_vars <- metadata |>
      dplyr::filter(
        .data[["form_name"]] == current_form,
        !(.data[["field_name"]] %in% phantom_vars)
      ) |>
      dplyr::pull("field_name")

    if (.include_aux) {
      checkbox_vars <- metadata |>
        dplyr::filter(
          .data$field_name %in% sel_vars,
          .data$field_type == "checkbox"
        ) |>
        dplyr::pull("field_name") |>
        stringr::str_c(collapse = "|")

      needed_aux <- stringr::str_subset(checkbox_aux, checkbox_vars)

      sel_vars <- c(sel_vars, needed_aux)

    }

  }

  if (.if_different_forms == "join") {

    purrr::map(
      sel_vars,
      function(x) select_rc_function(rc_data, x, metadata, checkbox_aux)
    ) |>
      purrr::reduce(dplyr::full_join) |>
      suppressMessages()

  } else {

    extracted_vars <- tibble::tibble(
      variables = purrr::map(
        sel_vars,
        function(x) select_rc_function(rc_data, x, metadata, checkbox_aux)
      ),
      form = purrr::map(
        .data$variables, ~.$redcap_form_name |> unique()
      )
    ) |>
      tidyr::nest(data = .data$variables)

    extracted_list <- purrr::map(
      extracted_vars[[2]],
      ~purrr::reduce(.[[1]], dplyr::full_join)
    ) |> suppressMessages()


    names(extracted_list) <- extracted_vars$form

    if (length(extracted_list) == 1) {
      extracted_list[[1]]
    } else {
      extracted_list
    }

  }

}

#' Filter a RedCap import by subject id
#'
#' @param redcap_data RedCap data import.
#' @param subjects_vector Vector with the subjects id's to be kept.
#'
#' @return A filtered RedCap import.
#' @export
ody_rc_filter_subject <- function(redcap_data, subjects_vector) {

  id_var <- attr(redcap_data, "id_var")

  if (names(redcap_data)[1] == "redcap_event_name") {

    result <- tidyr::unnest(redcap_data, cols = "redcap_event_data") |>
      dplyr::mutate(
        redcap_form_data = purrr::map(
          .data$redcap_form_data,
          ~dplyr::filter(., .data[[id_var]] %in% subjects_vector)
        )
      ) |>
      tidyr::nest(redcap_event_data = "redcap_form_name":"redcap_form_data")

  } else {

    result <- redcap_data |>
      dplyr::mutate(
        redcap_form_data = purrr::map(
          .data$redcap_form_data,
          ~dplyr::filter(., .data[[id_var]] %in% subjects_vector)
        )
      )

  }

  attributes(result) <- attributes(redcap_data)
  attr(result, "subjects") <- subjects_vector

  result

}


#' Format RedCap variables
#'
#' Format all variables from an ody_rc_select dataframe according to the
#' variable metadata.
#'
#' @param rc_df Dataframe derived from a RedCap import with ody_rc_select
#'
#' @details
#' Formating proceeds as follows:
#' - Values defined as numeric in redcap -> as.numeric (also redcap_repeat_instance).
#' - Values defined as date in redcap -> ymd
#' - Values defined as datetime in redcap -> ymd_hm
#' - Values labelled in redcap -> to_factor
#' - Other -> as.character
#' - User defined missing values -> NA
#'
#' @return A tibble
#' @export
ody_rc_format <- function(rc_df) {

  dplyr::mutate(
    rc_df,
    dplyr::across(
      tidyselect::everything(),
      function(x) {
        label <- attr(x, "label")
        labels <- labelled::val_labels(x)

        if (is.null(label)) {
          return(x)
        }

        x_no_user_na <- labelled::user_na_to_na(x)

        if (
          stringr::str_detect(label, "(number\\)$)|(integer\\)$)|(calc\\)$)")
        ) {
          result <- labelled::unlabelled(x_no_user_na) |>
            as.numeric()
        } else if (!is.null(labels)) {
          result <- labelled::to_factor(x_no_user_na)
          attr(result, "label") <- NULL
        } else if (stringr::str_detect(label, ":date_.+\\)$")) {
          result <- lubridate::ymd(x_no_user_na)
        } else if (stringr::str_detect(label, ":datetime_.+\\)$")) {
          result <- stringr::str_c(x_no_user_na, ":00") |>
            lubridate::ymd_hms()
        } else if (stringr::str_detect(label, "truefalse\\)$")) {
          result <- unclass(x_no_user_na) |>
            as.numeric() |>
            as.logical()
        } else {
          result <- as.character(x_no_user_na)
        }
        result
      }
    )
  )
}

#' View a RedCap project
#'
#' @param data_app Imported data by ody_rc_import (must be labelled and nested). If no data provided, the function calls ody_rc_import to download it from RedCap.
#'
#' @return An html viewer
#'
#' @details
#' "RedCap Viewer" addin calls to this function
#'
#'
#' @export
ody_rc_view <- function(data_app = NULL) {

  rlang::check_installed(c(
    "DT",
    "bsicons",
    "shiny",
    "bslib",
    "shinycssloaders",
    "reactablefmtr",
    "htmltools"
  ))

  if (is.null(data_app)) {
    if (exists("redcap_data")) {
      data_app <- get("redcap_data")
    } else {
      data_app <- ody_rc_import()
    }
  }

  # If the project has no events, data_app is restructured to fit
  if (names(data_app)[1] == "redcap_form_name") {
    data_app <- tibble::tibble(
      redcap_event_name = "No events",
      redcap_repeating_event = FALSE,
      redcap_event_data = list(data_app)
    ) |>
      restore_attributes(data_app)
  }

  viewer_location <- system.file("redcap_data_viewer", package = "odytools")
  save(
    data_app, file = stringr::str_c(viewer_location, "/data_app.RData")
  )

  rstudioapi::jobRunScript(
    stringr::str_c(viewer_location, "/data_viewer_runner.R")
  )

  rstudioapi::viewer("http://127.0.0.1:5921")

}

# Helper functions to create a conditions_list from redcap metadata.
get_conditions_from_metadata <- function(data_frame,
                                         metadata,
                                         missing_codes) {

  needed_meta <- metadata |>
    dplyr::filter(
      .data$field_name %in% names(data_frame),
      !is.na(.data$branching_logic)
    )

  missing_value <- stringr::str_c(
    missing_codes$raw_value, collapse = "|")

  if (nrow(needed_meta) > 0) {

    external_branching <- needed_meta |>
      dplyr::filter(
        stringr::str_detect(
          .data$branching_logic,
          "\\[.+\\]\\[.+\\]|event-name|current-instance"
        )
      ) |> dplyr::pull("field_name")

    if(length(external_branching) > 0) {
      warning(
        "External branching detected for variables\n",
        stringr::str_c(external_branching, collapse = "\n"),
        "\nExternal branching is still not implemented"
      )
    }

    pre_list <- needed_meta |>
      dplyr::filter(!(.data$field_name %in% external_branching)) |>
      dplyr::select("field_name", "branching_logic") |>
      dplyr::mutate(
        # RedCap logic is translated into R languaje
        r_branch = stringr::str_replace_all(
          .data$branching_logic,  missing_value, "user_na"
        ) |>
          # Checkbox variables to especific check box column
          stringr::str_replace_all( "\\((\\d+)\\)", "___\\1") |>
          stringr::str_replace_all(
            # RedCap empty to regular R na
            "\\[([^\\[]+)\\] *<> *['\"]{2}",
            "!labelled::is_regular_na\\(\\1\\)"
          ) |>
          stringr::str_replace_all(
            # RedCap declared missing to user defined R na
            "\\[([^\\[]+)\\] *<> *['\"]user_na['\"]",
            "!labelled::is_user_na\\(\\1\\)"
          ) |>
          #Some easy symbol translations
          stringr::str_remove_all("\\[|\\]") |>
          stringr::str_replace_all("=", "==") |>
          stringr::str_replace_all("<>", "!=") |>
          stringr::str_replace_all(" or ?| ?or ", " | ") |>
          stringr::str_replace_all(" and ?| ?and ", " & ") |>
          # Delete possible duplicates of is_user_na
          stringr::str_replace_all("(.*labelled::is_user_na.+)\\1+", "\\1"),
        cond = stringr::str_c(
          .data$field_name, " = ", "\"", .data$r_branch, "\""
        )
      ) |>
      dplyr::pull("cond")

    conditions_list <- stringr::str_c(
      "list(",
      stringr::str_c(pre_list, collapse = ", "),
      ")"
    ) |> str2lang() |> eval()

  } else {

    conditions_list <- NULL

  }


  conditions_list

}


#' Verify Completeness of a RedCap-derived dataframe
#'
#' @param data_frame The data frame to verify.
#' @param id_var Variable used as ID. By default the functions looks at the "id_var" attribute of redcap_data.
#' @param count_user_na If FALSE (the default) only regular missing values are count as missing values. Set this argument to TRUE to also take into account the explicit user defined missing values.
#' @param conditions_list A list to define the conditional presence of the variables. Needed when the presence of a variable depends on the value of other variable. By default the list is based on the branching logic defined in the metadata.
#' @param metadata Used metadata if conditions_list = "from_metadata". By default, the function gets it from the attribute "metadata" of the redcap_data.
#' @param missing_codes Redcap_data missing codes. Only needed if conditions_list is based on the metadata. By default is the attribute "missing_codes".
#' @param report Render a report or output a tibble?
#' @param opt_reactable Extra options provided by ody_options. To ody_rc_completeness it only applies border_color.
#'
#' @return An html report or a tibble
#' @export
ody_rc_completeness <- function(
    data_frame,
    id_var = attr(get("redcap_data"), "id_var"), # Use get to avois check warnings.
    count_user_na = FALSE,
    conditions_list = "from_metadata",
    metadata = attr(get("redcap_data"), "metadata"),
    missing_codes = attr(get("redcap_data"), "missing"),
    report = TRUE,
    opt_reactable = ody_options()
) {

  rlang::check_installed(c(
    "htmltools",
    "reactablefmtr"
  ))

  data_frame <- data_frame |>
    dplyr::select(-dplyr::starts_with("redcap_")) |>
    dplyr::mutate(
      # All auxiliary checkbox variables (which are the only pure logical
      # variables) must be set as numeric so the condition filtering is done
      # properly.
      dplyr::across(dplyr::where(is.logical), as.numeric)
    )

  if (!count_user_na) {
    # easy trick, if user NAs must not be counted, all variables are set as
    # simple character so user NAs become a character.
    data_frame <- data_frame |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      )
  }

  if (!is.null(conditions_list) && conditions_list == "from_metadata") {

    conditions_list <-  get_conditions_from_metadata(
      data_frame, metadata, missing_codes
    )

  }

  completeness <- ody_verify_completeness(
    data_frame,
    conditions_list = conditions_list,
    id_var = id_var
  )

  # We exclude auxiliary checkbox variables
  completeness_final_vars <- completeness$variable |>
    stringr::str_remove("___.+$") |>
    unique()
  completeness <- completeness |>
    dplyr::filter(.data$variable %in% completeness_final_vars)

  # Change back R translated conditions to RedCapian
  needed_meta <- metadata |>
    dplyr::filter(
      .data$field_name %in% names(data_frame),
      !is.na(.data$branching_logic)
    )
  if (nrow(needed_meta) > 0) {

    completeness <- completeness |>
      dplyr::left_join(
        needed_meta |>
          dplyr::select("field_name", "branching_logic"),
        by = c("variable" = "field_name")
      ) |>
      dplyr::mutate(
        condition = .data$branching_logic
      ) |>
      dplyr::select(-"branching_logic")

  }

  if (report) {
    report_completeness(completeness, opt_reactable = opt_reactable)
  } else {
    completeness
  }

}



#' Spread a Classic REDCap project into a 2D table
#'
#' The function ody_rc_spread takes a classic project (with no events) and spreads it into a tibble with one row per subject. This is useful for creating Excel exports.
#'
#' @param rc_data The object to spread.
#'
#' @details If no data provided, the function checks whether there is a redcap_data object in the environment.
#'
#' @return A tibble
#' @export
ody_rc_spread <- function(rc_data = NULL) {

  if (is.null(rc_data)) {
    if (exists("redcap_data")) {
      rc_data <- get("redcap_data")
    } else {
      stop("No data provided.")
    }
  }

  if (names(rc_data)[2] != "redcap_repeating_form") {
    stop("This function only supports classic projects with no events.")
  }

  fields <- attr(rc_data, "metadata")$field_name
  id_var <- attr(rc_data, "id_var")

  has_repeating <- any(rc_data$redcap_repeating_form)
  has_unique <- any(!rc_data$redcap_repeating_form)

  if (has_unique) {
    spread_unique <- rc_data |>
      dplyr::filter(!.data$redcap_repeating_form) |>
      dplyr::pull(.data$redcap_form_data) |>
      purrr::map(
        ~. |>
          dplyr::select(
            -redcap_instance_type,
            -redcap_instance_number,
            -tidyselect::matches("___")
          ) |>
          ody_rc_format()
      ) |>
      purrr::reduce(dplyr::full_join, by = id_var)

  }

  if (has_repeating) {
    spread_repeating <- rc_data |>
      dplyr::filter(.data$redcap_repeating_form) |>
      dplyr::pull(.data$redcap_form_data) |>
      purrr::map(
        ~.|>
          ody_rc_format() |>
          dplyr::select(
            -redcap_instance_type,
            -tidyselect::matches("___")
          ) |>
          tidyr::pivot_wider(
            names_from = redcap_instance_number,
            values_from = c(
              -tidyselect::all_of(id_var), -redcap_instance_number
            ),
            names_vary = "slowest"
          )
      ) |>
      purrr::reduce(dplyr::full_join, by = id_var)
  }

  if (has_unique && has_repeating) {
    dplyr::full_join(spread_unique, spread_repeating, by = id_var) |>
      dplyr::select(tidyselect::starts_with(fields))
  } else if (has_unique) {
    spread_unique |>
      dplyr::select(tidyselect::starts_with(fields))
  } else {
    spread_repeating |>
      dplyr::select(tidyselect::starts_with(fields))
  }

}

#' Add the redcap_data import date to a file name
#'
#' @param file_name The name of the file to be saved.
#' @param extension The extension of the file. Default is "csv".
#'
#' @return A string with the new file name.
#' @export
ody_rc_add_import_date <- function(file_name, extension = "csv") {

  loaded_date <- ody_rc_current(as_list = TRUE)$loaded |>
    stringr::str_remove_all("-|:") |>
    stringr::str_replace_all(" ", "_")

  stringr::str_c(file_name, "_", loaded_date, ".", extension)

}

#' Add the sites to a RedCap table
#'
#' @param tbl The table to add the site to.
#' @param redcap_data The redcap_data object with the sites information as attribute (the dag attribute of an ody_rc_import output). By default, the function looks for a redcap_data object in the environment.
#' @param position The position of the site column. Default is 1.
#'
#' @return The same tbl with the site column added.
#' @export
ody_rc_add_site <- function(tbl,
                              redcap_data = redcap_data,
                              position = 1) {

  id_var <- attr(redcap_data, "id_var")
  sites <- attr(redcap_data, "subjects_dag") |>
    dplyr::left_join(
      attr(redcap_data, "dag"),
      by = c(redcap_data_access_group =  "unique_group_name")
    ) |>
    dplyr::select(
      tidyselect::all_of(id_var),
      site = "data_access_group_name"
    )

  tbl |>
    dplyr::left_join(sites) |>
    dplyr::relocate("site", .before = position)

}
