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
    width = "15%"
  ),
  navset_card_tab(
    nav_panel(
      popover(
        tagList("Data", bs_icon("gear", class = "ms-auto")),
        radioButtons(
          "data_type",  HTML("<b>Field Type</b>"), c("Labels", "Raw", "Raw + Labels"),
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
    nav_panel("Descriptive", reactableOutput("descriptive") |> withSpinner())
  )
)


server <- function(input, output, session) {

  # observeEvent(
  #   input$event,
  #   {
  #     if (input$event != "All") {
  #
  #     temp <- data_app |>
  #       filter(redcap_event_name == input$event) |>
  #       unnest(cols = redcap_event_data) |>
  #       pull(redcap_form_name)
  #     } else {
  #       temp <- data_app |>
  #         unnest(cols = redcap_event_data) |>
  #         pull(redcap_form_name)
  #     }
  #     updateSelectInput(session,"form",choices = unique(temp))
  #   }
  # )

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

  # raw_table <- reactive({
  #
  #   if (input$event != "All") {
  #
  #     event_data <- data_app |>
  #       filter(redcap_event_name == input$event)
  #
  #   } else {
  #
  #     event_data <- data_app
  #
  #   }
  #
  #   event_data |>
  #     unnest(cols = redcap_event_data) |>
  #     filter(redcap_form_name == input$form) |>
  #     select(redcap_event_name, redcap_form_name, redcap_form_data) |>
  #     unnest(cols = redcap_form_data)
  #
  # })

  raw_table <- eventReactive(
    c(input$dag, input$form, input$event),
    {

      if (input$dag %in% c("All", "No sites")) {

        filtered_data <- data_app

      } else {

        site_subjects <- attr(data_app, "subjects_dag") |>
          filter(redcap_data_access_group == input$dag) |>
          pull(1)
        filtered_data <- ody_rc_filter_subject(data_app, site_subjects)

      }

      if (input$event != "All") {

        event_data <- filtered_data |>
          filter(redcap_event_name == input$event)

      } else {

        event_data <- filtered_data

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

    if (data_app[1, 1] == "No events") formatted_form[ ,-1] else formatted_form


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

}

shinyApp(ui = ui, server = server)
