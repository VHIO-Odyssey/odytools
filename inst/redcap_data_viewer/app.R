library(dplyr)
library(tidyr)
library(shiny)
library(bslib)
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
data_app <- data_app[map_dbl(data_app[[3]], nrow) != 0, ]


ui <- page_sidebar(
  title = app_title,
  sidebar = sidebar(
    selectInput(
      "event", HTML("<b>Event</b>"), c("All", data_app$redcap_event_name),
      width = "100%"
    ),
    selectInput(
      "form", HTML("<b>Form</b>"), data_app |>
        unnest(cols = redcap_event_data) |>
        pull(redcap_form_name) |>
        unique(),
      width = "100%"
    ),
    width = "25%"
  ),
  navset_card_tab(
    nav_panel(
      popover(
        tagList("Data", bsicons::bs_icon("gear", class = "ms-auto")),
        radioButtons(
          "data_type",  HTML("<b>Field Type</b>"), c("Labels", "Raw", "Raw + Labels"),
          inline = TRUE
        )
      ), DTOutput("table")
    ),
    nav_panel("Metadata", dataTableOutput("metadata")),
    nav_panel(
      popover(
        tagList("Completeness", bsicons::bs_icon("gear", class = "ms-auto")),
        checkboxInput(
          "count_user_na",
          "Consider user-defined missing values as regular missing values.",
          value = FALSE
        )
      ),
      reactableOutput("completeness")
    )
  )
)


server <- function(input, output, session) {

  observeEvent(
    input$event,
    {
      if (input$event != "All") {

      temp <- data_app |>
        filter(redcap_event_name == input$event) |>
        unnest(cols = redcap_event_data) |>
        pull(redcap_form_name)
      } else {
        temp <- data_app |>
          unnest(cols = redcap_event_data) |>
          pull(redcap_form_name)
      }
      updateSelectInput(session,"form",choices = unique(temp))
    }
  )

  raw_table <- reactive({

    if (input$event != "All") {

      event_data <- data_app |>
        filter(redcap_event_name == input$event)

    } else {

      event_data <- data_app

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

output$table <- renderDT(
  datatable(
    table_to_show(),
    filter = "top",
    fillContainer = TRUE,
    extensions = "Buttons",
    class = "compact hover",
    options = list(
      paging = FALSE,
      dom = "Bt",
      buttons = c("copy", "csv", "excel")
    )
  )
)

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
      escape = FALSE, fillContainer = TRUE,
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

}

shinyApp(ui = ui, server = server)
