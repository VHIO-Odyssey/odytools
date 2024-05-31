library(dplyr)
library(tidyr)
library(shiny)
library(shinycssloaders)
library(bslib)
library(bsicons)
library(forcats)
library(labelled)
library(stringr)
library(lubridate)
library(DT)
library(purrr)
library(reactable)
library(odytools)

load("data_app.RData")
# After data loading, the file is removed so the residual copy with potential
# sensible information does not remain hiden in the package folder.
file.remove("data_app.RData")
app_title <- str_c(
  attr(data_app, "project_info")$project_title, " - ",
  str_extract(attr(data_app, "import_date"), "....-..-.. ..:..") |>
    str_replace_all("-", "/")
)

# Puede haber events 100% vacíos. Se eliminan.
data_app_events <- attr(data_app, "events")

# DAGS
dag <- attr(data_app, "dag")
if (is.null(dag)) {
  dag_choices <- "No sites"
} else {
  dag_choices <- c("All", dag$unique_group_name)
  names(dag_choices) <- c("All", dag$data_access_group_name)
}

# Eventos
if (is.null(data_app_events)) {
  events_choices <- "No events"
} else {
  present_events <- data_app_events |>
    filter(unique_event_name %in% data_app$redcap_event_name)
  events_choices <- c("All", present_events$unique_event_name)
  names(events_choices) <- c("All", present_events$event_name)
}

# Forms
data_app_forms <- data_app |>
  unnest(cols = redcap_event_data) |>
  pull(redcap_form_name) |>
  unique()
present_forms <- attr(data_app, "forms") |>
  filter(instrument_name %in% data_app_forms)
forms_choices <- present_forms$instrument_name
names(forms_choices) <- present_forms$instrument_label


ui <- page_sidebar(
  title = app_title,
  sidebar = sidebar(
    selectInput(
      "dag", HTML("<b>Site</b>"), dag_choices,
      width = "100%"
    ),
    selectInput(
      "event", HTML("<b>Event</b>"), events_choices,
      width = "100%"
    ),
    selectInput(
      "form", HTML("<b>Form</b>"), forms_choices,
      width = "100%"
    ),
    textAreaInput(
      "vars", HTML("<b>Variable Merger</b>"),
      height = 375,
      placeholder = "Type the names of the variables you wish to join in this box, following this pattern:\n\nvar1;\nvar2[1, 2];\nvar3[0, -1]\n\nThe values in brackets [] are used to select specific instances. The final result will be filtered according to the selected site and shown in the 'Merged Data' tab.",
    ),
    checkboxInput("hide_loc", "Hide __loc columns"),
    checkboxInput("pivot", "Pivot repeating instances"),
    actionButton("submit_join", HTML("<b>Merge</b>")),
    width = "15%"
  ),
  navset_card_tab(
    nav_panel(
      popover(
        tagList("Data", bs_icon("gear", class = "ms-auto")),
        radioButtons(
          "data_type",  HTML("<b>Field Type</b>"),
          c("Labels", "Raw", "Raw [Label]"),
          inline = TRUE
        )
      ), DTOutput("table") |> withSpinner()
    ),
    nav_panel("Metadata", dataTableOutput("metadata") |> withSpinner()),
    nav_panel(
      popover(
        tagList("Completeness", bs_icon("gear", class = "ms-auto")),
        checkboxInput(
          "count_user_na",
          "Consider user-defined missing values as regular missing values.",
          value = FALSE
        )
      ),
      reactableOutput("completeness") |> withSpinner()
    ),
    nav_panel("Descriptive", reactableOutput("descriptive") |> withSpinner()),
    nav_panel("Merged Data", DTOutput("joined_data") |> withSpinner())
  )
)


server <- function(input, output, session) {

  # Forms Selector Update
  observeEvent(
    input$event,
    {
      if (input$event != "All") {

        data_app_forms <- data_app |>
          filter(redcap_event_name == input$event) |>
          unnest(cols = redcap_event_data) |>
          pull(redcap_form_name) |>
          unique()

      } else {

        data_app_forms <- data_app |>
          unnest(cols = redcap_event_data) |>
          pull(redcap_form_name) |>
          unique()
      }

      present_forms <- attr(data_app, "forms") |>
        filter(instrument_name %in% data_app_forms)
      forms_choices <- present_forms$instrument_name
      names(forms_choices) <- present_forms$instrument_label

      updateSelectInput(
        session, "form", choices = forms_choices
      )
    }
  )

  filtered_data <- eventReactive(
    input$dag,
    {

      if (input$dag %in% c("All", "No sites")) {

        data_app

      } else {

        site_subjects <- attr(data_app, "subjects_dag") |>
          filter(redcap_data_access_group == input$dag) |>
          pull(1)
        ody_rc_filter_subject(data_app, site_subjects)

      }

    })

  raw_table <- eventReactive(
    c(filtered_data(), input$form, input$event),
    {

      if (input$event != "All") {

        event_data <- filtered_data() |>
          filter(redcap_event_name == input$event)

      } else {

        event_data <- filtered_data()

      }

      event_data |>
        unnest(cols = redcap_event_data) |>
        filter(redcap_form_name == input$form) |>
        select(redcap_event_name, redcap_form_name, redcap_form_data) |>
        unnest(cols = redcap_form_data)

    })

  table_to_show <- reactive({

    formatted_form <- raw_table() |>
      mutate(
        across(
          everything(),
          function(x) {

            if (input$data_type == "Raw" | is.null(var_label(x))) return(x)

            if (input$data_type == "Labels") {

              if (str_detect(var_label(x), "(text:integer)|(text:number)")) {
                return(as.numeric(unclass(x)))
              }

              if (str_detect(var_label(x), "(text:date_dmy)")) {
                return(ymd(unclass(x)))
              }

              if (str_detect(var_label(x), "(text:datetime_dmy)")) {
                return(ymd_hm(unclass(x)))
              }

              if (!is.null(val_labels(x))) return(to_factor(x))

              return(x)

            }

            if (!is.null(val_labels(x))) {
              # truco para que los missings declarados salgan sin etiqueta
              x_labels <- as_factor(x) |> as.character()
              x_labels2 <- ifelse(
                is.na(x) & !is.na(as_factor(x)), "", x_labels
              )
              return(
                str_c(
                  x,
                  str_c(
                    " [",
                    as_factor(x_labels2),
                    "]"
                  )
                ) |>
                  str_remove(" \\[\\]$") |>
                  as_factor()
              )
            }

            return(x)

          }
        ),
        across(1:5, factor)
      ) |>
      select(where(~ !is.logical(.)))

    if (data_app[1, 1] == "No events") formatted_form <- formatted_form[ ,-1]

    current_meddra_fields <- names(formatted_form)[
      names(formatted_form) %in% attr(data_app, "meddra_fields")
    ]

    if (input$data_type != "Raw" && length(current_meddra_fields) > 0) {

      formatted_form |>
        mutate(
          across(
            all_of(current_meddra_fields),
            function(x) {
              code_tbl <- tibble(code = x)
              if (input$data_type == "Raw [Label]") {
                label_tbl <- attr(data_app, "meddra_codes") |>
                  mutate(
                    label = str_c(code, " [", label, "]")
                  )
              } else {
                label_tbl <- attr(data_app, "meddra_codes")
              }
              left_join(code_tbl, label_tbl, by = "code") |>
                pull("label")
            }
          )
        )

    } else {
      formatted_form
    }

  })

  output$table <- renderDT({

    validate(
      need(
      nrow(raw_table()) > 0,
      "No data available for the selected site.")
  )

  datatable(
    table_to_show(),
    filter = "top",
    extensions = "Buttons",
    class = "compact hover",
    options = list(
      paging = FALSE,
      dom = "Bt",
      buttons = c("copy", "csv", "excel")
    )
  )
})

output$metadata <- renderDataTable({
  attr(data_app, "metadata") |>
    filter(field_name %in% names(raw_table())[-(1:5)]) |>
    select(
      field_name, field_label, field_type,
      choices_calculations = select_choices_or_calculations,
      branching_logic,
      validation_type = text_validation_type_or_show_slider_number,
      validation_min = text_validation_min,
      validation_max = text_validation_max
    ) |>
    mutate(
      choices_calculations = str_replace_all(
        choices_calculations, "\\|", "<br>"
      )
    ) |>
    datatable(
      escape = FALSE,
      class = "compact hover",
      options = list(paging = FALSE)
    )

})


raw_table_comp <- reactive({

  id_var <- attr(data_app, "id_var")

  id_column <- raw_table() |>
    dplyr::select(dplyr::all_of(id_var))

  id_rep <- nrow(id_column) != nrow(unique(id_column))

  if (id_rep) {

    comp_table <- raw_table() |>
      mutate(
        "{id_var}" := str_c(
          .data[[id_var]], "(", redcap_instance_number, ")"
        )
      )

  } else {

    raw_table()

  }

})


output$completeness <- renderReactable({

  validate(
    need(
      nrow(raw_table()) > 0,
      "No data available for the selected site.")
  )

  ody_rc_completeness(
    raw_table_comp(),
    id_var = attr(data_app, "id_var"),
    count_user_na = input$count_user_na,
    conditions_list = "from_metadata",
    metadata = attr(data_app, "metadata"),
    missing_codes = attr(data_app, "missing"),
    report = TRUE
  )

})

output$descriptive <- renderReactable({

  validate(
    need(
      nrow(raw_table()) > 0,
      "No data available for the selected site.")
  )

  raw_table_v0 <- raw_table() |>
    dplyr::select(-(1:5))

  # 100% empty vars can not be descrived
  empty_vars_index <- raw_table_v0 |>
    purrr::map_lgl(~all(is.na(.)))
  needed_vars <- names(raw_table_v0)[!empty_vars_index]

  raw_table_v0 |>
    dplyr::select(dplyr::all_of(needed_vars)) |>
    ody_rc_format() |>
    ody_summarise_df(
      show_completeness = FALSE, searchable = TRUE,
      pagination = FALSE
    )

})

variables_join <- eventReactive(
  input$submit_join , {
    raw_vars <- str_split(input$vars, ";")  |>
      map(str_trim) |>
      unlist()

    raw_vars <- raw_vars[raw_vars != ""]

    tibble(
      vars = str_remove(raw_vars,"\\[.+\\]$"),
      index = str_extract(raw_vars, "\\[.+\\]$") |>
        str_remove_all("\\[|\\]") |>
        str_split(",") |>
        map(str_trim)
    ) |>
      left_join(
        attr(data_app, "metadata") |>
          select(field_name, form_name),
        by = c("vars" = "field_name")
      ) |>
      nest(indexes = c(vars, index))
  }
)

output$joined_data <- renderDT({

  req(variables_join())

  chossen_variables <- variables_join() |> unnest(indexes) |>
    pull(vars)

  wrong_variables <- chossen_variables[
    !(chossen_variables %in% attr(data_app, "metadata")$field_name)
  ]

  validate(
    need(
      length(wrong_variables) == 0,
      str_c("Wrong variable names: ", str_c(wrong_variables, collapse = ", "))
    )
  )

  id_var <- attr(data_app, "id_var")

  available_cases <- filtered_data() |>
    odytools:::rc_select_viewer(
      variables_join() |>
        unnest(indexes) |>
        pull(vars)
    )

  if (is.data.frame(available_cases)) {

    there_are_cases <- nrow(available_cases) > 0

  } else {

    there_are_cases <- TRUE

  }

  validate(
    need(
      there_are_cases,
      "No data available for the selected site."
    )
  )

  extracted_vars <- map(
    variables_join()$indexes,
    function(x) {
      selected_var <- filtered_data() |>
        odytools:::rc_select_viewer(unique(x$vars)) |>
        group_by(.data[[id_var]], redcap_event_name) |>
        mutate(
          double_index = str_c(
            redcap_instance_number, " (-",
            n() - as.numeric(redcap_instance_number) + 1, ")"
          ),
          double_index = if_else(is.na(double_index), "0", double_index),
          .after = redcap_instance_number
        )

      index <- x$index |> unlist() |> unique() |> na.omit() |> as.numeric()

      index_pos <- index[index > 0]
      index_neg <- index[index < 0]
      index_0 <- index[index == 0]


      if (length(index_pos) == 0 && length(index_neg) == 0 && length(index_0) == 0) {

        return(
          selected_var |>
            select(-redcap_instance_number) |>
            ungroup()
        )

      }

      if (length(index_pos) > 0) {

        pos_filter <- selected_var |>
          group_by(.data[[id_var]], redcap_event_name) |>
          filter(redcap_instance_number %in% index_pos)

      } else {

        pos_filter <- selected_var |> slice(0)

      }

      if (length(index_neg) > 0) {

        neg_filter <- selected_var |>
          group_by(.data[[id_var]], redcap_event_name) |>
          filter(redcap_instance_number %in% (n() + index_neg + 1))

      } else {

        neg_filter <- selected_var |> slice(0)

      }

      if (length(index_0) > 0) {

        zero_filter <- selected_var |>
          group_by(.data[[id_var]], redcap_event_name) |>
          filter(is.na(redcap_instance_number))

      } else {

        zero_filter <- selected_var |> slice(0)

      }


      bind_rows(zero_filter, pos_filter, neg_filter) |>
        select(-redcap_instance_number) |>
        arrange(.data[[id_var]]) |>
        ungroup()
    }

  )

  extracted_df_v0 <-  map2(
    extracted_vars,
    variables_join()$form_name,
    function(x, y) {
      x |>
        mutate(
          "{y}__loc" := str_c(
            redcap_event_name, "[", str_replace_na(double_index), "]"
          ) |> str_remove_all("\\[NA\\]$") |>
            str_remove("^No events"),
          .after = 1
        ) |>
        select(-(3:6))
    }
  )

  if (!input$pivot) {

    extracted_df <- extracted_df_v0 |>
      reduce(full_join, by = id_var) |>
      select(where(~!all(is.na(.)))) |>
      ody_rc_format() |>
      unique() # need in case the same instance is choosen with both
    # positive and negative index

  } else {

    extracted_df <- extracted_df_v0 |>
      map(
        function(x) {

          n_rep <- count(x, .data[[id_var]]) |>
            filter(n > 1) |>
            nrow()

          if (n_rep == 0) {

            x |> select(!ends_with("__loc"))

          } else {

            x |>
              group_by(.data[[id_var]]) |>
              mutate(id_instance = row_number(), .after = 1) |>
              select(-3) |>
              pivot_wider(
                values_from = -(1:2),
                names_from = id_instance,
                names_glue = "{.value}__{id_instance}",
                names_vary = "slowest"
              )
          }
        }
      ) |>
      reduce(full_join, by = id_var) |>
      select(where(~!all(is.na(.)))) |>
      ody_rc_format()

  }

  extracted_df[[1]] <- factor(extracted_df[[1]])

  # Location columns are excluded if checked
  if (input$hide_loc) {

    extracted_df <- extracted_df |>
      select(!ends_with("__loc"))

  }

  current_meddra_fields <- names(extracted_df)[
    names(extracted_df) %in% attr(data_app, "meddra_fields")
  ]

  if (length(current_meddra_fields) > 0) {

    extracted_df <- extracted_df |>
      mutate(
        across(
          all_of(current_meddra_fields),
          function(x) {
            code_tbl <- tibble(code = x)
            label_tbl <- attr(data_app, "meddra_codes")
            left_join(code_tbl, label_tbl, by = "code") |>
              pull("label")
          }
        )
      )
  }

  extracted_df |>
    mutate(
      across(ends_with("__loc"), factor)
    ) |>
    datatable(
      filter = "top",
      extensions = "Buttons",
      class = "compact hover",
      options = list(
        paging = FALSE,
        dom = "Bt",
        buttons = c("copy", "csv", "excel")
      )
    )

})


}

shinyApp(ui = ui, server = server)
