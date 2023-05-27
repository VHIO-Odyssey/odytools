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
    httr::content(show_col_types = FALSE) |>
    suppressWarnings()

}


# Helper function to import a RedCap project
import_rc <- function(
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
      present = .data[["field_name"]] %in% colnames(redcap_data)
    ) |>
    dplyr::filter(!.data[["present"]]) |>
    dplyr::select("field_name", "field_type", "form_name")
  attr(redcap_data, "missing_codes") <- missing_codes
  attr(redcap_data, "id_var") <- id_var
  attr(redcap_data, "subjects") <- redcap_data |>
    dplyr::pull(dplyr::all_of(id_var)) |>
    unique()
  attr(redcap_data, "import_date") <- import_date

  # Delete unnecessary attributes
  attr(redcap_data, "spec") <- NULL
  attr(redcap_data, "problems") <- NULL

  redcap_data

}

# Helper function to label the imported dataframe from ody_rc_import
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
  checkbox_fields <- metadata |>
    dplyr::filter(.data[["field_type"]] == "checkbox") |>
    dplyr::pull("field_name")

  for (checkbox_field in checkbox_fields) {
    print(stringr::str_c("Processing checkbox variable ", checkbox_field))
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
        ~ ifelse(.x == "1", stringr::str_split(.y, "___")[[1]][2], NA)
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
    present_combinations <- pulled_selections_char |>
      na.omit() |>
      unique() %>%
      {
        .[stringr::str_detect(., ", ")]
      }

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
    print(stringr::str_c("Labelling ", field))
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

# Helper function to nest a classic project with no events
nest_rc_classic <- function(rc_raw) {

  id_var <- attr(rc_raw, "id_var")
  metadata <- attr(rc_raw, "metadata")
  repeating <- attr(rc_raw, "repeating")

  redcap_data <- purrr::map_df(
    unique(metadata$form_name),
    function(form) {
      form_fields <- metadata |>
        dplyr::filter(form_name == form) |>
        dplyr::pull(field_name)
      # Variables selection
      raw_result <- rc_raw |>
        dplyr::select(
          # These 3 variables are always selected.
          dplyr::all_of(id_var),
          redcap_repeat_instrument, redcap_repeat_instance,
          # list of variables from the current form.
          dplyr::any_of(form_fields),
          # Possible checbox original variables
          dplyr::any_of(
            tidyselect::starts_with(
              stringr::str_c(form_fields, "___")
            )
          )
        ) |>
        dplyr::mutate(
          dplyr::across(contains("___"), ~ as.logical(as.numeric(.)))
        )
      # Rows selection
      is_repeating <- form %in% repeating$form_name
      if (is_repeating) {
        filtered_result <- raw_result |>
          dplyr::filter(redcap_repeat_instrument == form) |>
          dplyr::select(-redcap_repeat_instrument)
      } else {
        filtered_result <- raw_result |>
          dplyr::filter(is.na(redcap_repeat_instrument)) |>
          dplyr::select(-redcap_repeat_instance, -redcap_repeat_instrument)
      }

      filtered_result |>
        dplyr::mutate(
          form_name = form,
          is_repeating = is_repeating,
          .after = dplyr::all_of(id_var)
        ) |>
        tidyr::nest(variables = c(-form_name, -is_repeating))
    }
  )

  # Clean artifacts
  redcap_data <- redcap_data |>
    dplyr::mutate(
      empty_index = purrr::map(
        variables,
        function(vars) {
          vars |>
            dplyr::select(
              -dplyr::any_of(c(id_var, "redcap_repeat_instance", "redcap_event_name")),
              -where(is.logical)
            ) |>
            apply(1, function(x) all(labelled::is_regular_na(x)))
        }
      ),
      variables_clean = purrr::map2(
        variables, empty_index,
        ~ dplyr::filter(.x, !.y)
      )
    ) |>
    dplyr::select(form_name, is_repeating, variables_clean) |>
    dplyr::rename(variables = variables_clean)

  # redcap_repeat_instance labelling (must be done after nesting)
  repeating_forms <- redcap_data |>
    dplyr::filter(is_repeating) |>
    dplyr::pull(form_name)

  for (form_name in repeating_forms) {
    redcap_repeat_instance <- redcap_data[
      redcap_data$form_name == form_name,
    ]$variables[[1]]$redcap_repeat_instance

    redcap_repeat_instance <- labelled::labelled(
      redcap_repeat_instance,
      label = stringr::str_c(form_name, "/redcap_repeat_instance")
    )

    redcap_data[
      redcap_data$form_name == form_name,
    ]$variables[[1]]$redcap_repeat_instance <- redcap_repeat_instance
  }

  redcap_data
}

# Helper function to nest a longitudinal project with events
nest_rc_long <- function(rc_raw) {

  id_var <- attr(rc_raw, "id_var")
  metadata <- attr(rc_raw, "metadata")
  repeating <- attr(rc_raw, "repeating")

  rc_raw <- rc_raw |>
    dplyr::mutate(
      redcap_instance_type = dplyr::case_when(
        is.na(redcap_repeat_instance) ~ "unique",
        is.na(redcap_repeat_instrument) ~ "event",
        TRUE ~ "form"
      ),
      redcap_instance_number = redcap_repeat_instance
    )

  redcap_data <- rc_raw |>
    tidyr::nest(data_raw = -redcap_event_name) |>
    dplyr::mutate(
      repeating_event = redcap_event_name %in% (
        repeating |>
          dplyr::filter(is.na(form_name)) |>
          dplyr::pull(event_name)
      ),
      .before = "data_raw"
    ) |>
    dplyr::mutate(
      event_data = purrr::map2(
        data_raw, redcap_event_name,
        function(event_data, redcap_event_name) {
          purrr::map_dfr(
            unique(metadata$form_name),
            function(form) {
              form_fields <- metadata |>
                dplyr::filter(form_name == form) |>
                dplyr::pull(field_name)
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
                  redcap_repeat_instrument,
                  redcap_instance_type, redcap_instance_number,
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
                  dplyr::across(contains("___"), ~ as.logical(as.numeric(.)))
                )
              # Rows selection
              is_repeating <- form %in% (
                repeating |>
                  dplyr::filter(
                    event_name == redcap_event_name
                  ) |>
                  dplyr::pull(form_name)
              )
              if (is_repeating) {
                filtered_result <- raw_result |>
                  dplyr::filter(redcap_repeat_instrument == form) |>
                  dplyr::select(-redcap_repeat_instrument)
              } else {
                filtered_result <- raw_result |>
                  dplyr::filter(is.na(redcap_repeat_instrument)) |>
                  dplyr::select(-redcap_repeat_instrument)
              }
              filtered_result |>
                dplyr::mutate(
                  form_name = form,
                  repeating_form = is_repeating,
                  .after = dplyr::all_of(id_var)
                ) |>
                tidyr::nest(form_data = c(-form_name, -repeating_form))
            }
          ) |>
            dplyr::filter(!is.na(form_name))
        }
      )
    ) |>
    dplyr::select(-data_raw)

  # Clean artifacts
  redcap_data <- redcap_data |>
    dplyr::mutate(
      cleaned_data = purrr::map(
        event_data,
        function(rc_raw) {
          rc_raw |>
            dplyr::mutate(
              empty_index = purrr::map(
                form_data,
                function(vars) {
                  vars |>
                    dplyr::select(
                      -dplyr::any_of(
                        c(
                          id_var,
                          "redcap_instance_type",
                          "redcap_instance_number"
                        )
                      ),
                      -where(is.logical)
                    ) |>
                    apply(1, function(x) all(labelled::is_regular_na(x)))
                }
              ),
              variables_clean = purrr::map2(
                form_data, empty_index,
                ~ dplyr::filter(.x, !.y)
              )
            ) |>
            dplyr::select(form_name, repeating_form, variables_clean) |>
            dplyr::rename(form_data = variables_clean)
        }
      )
    ) |>
    dplyr::select(-event_data) |>
    dplyr::rename(
      event_data = cleaned_data,
      event_name = redcap_event_name
    )

  # aAdd redcap_ preffix to reduce coincidence chances with vars names.
  names(redcap_data) <- stringr::str_c("redcap_", names(redcap_data))
  for (i in 1:nrow(redcap_data)) {
    names(redcap_data$redcap_event_data[[i]]) <- stringr::str_c(
      "redcap_", names(redcap_data$redcap_event_data[[i]])
    )
  }
  redcap_data
}

# Helper function to pass the original import attibutes to the nested final
# data base.
restore_attributes <- function(rc_nested, rc_raw) {

  attr_names <- names(attributes(rc_raw))[3:14]

  for (attribute in attr_names) {
    attr(rc_nested, attribute) <- attr(rc_raw, attribute)
  }

  rc_nested
}

