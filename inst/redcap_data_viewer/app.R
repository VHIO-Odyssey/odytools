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
load("data_app.RData")
# After data loading, the file is removed so the residual copy with potential
# sensible information does not remain hiden in the package folder.
file.remove("data_app.RData")
app_title <- str_c(
  attr(data_app, "project_info")$project_title, " - Export date: ",
  str_extract(attr(data_app, "import_date"), "....-..-.. ..:..")
)

# Puede haber events 100% vacíos. Se eliminan.
data_app <- data_app[map_dbl(data_app[[3]], nrow) != 0, ]


ui <- page_sidebar(
  title = app_title,
  sidebar = sidebar(
    selectInput(
      "data_type", "Field Type", c("Labels", "Raw", "Raw + Labels"),
      width = "100%"
    ),
    selectInput(
      "event", "Event", c("All", data_app$redcap_event_name),
      width = "100%"
    ),
    selectInput(
      "form", "Form", data_app |>
        unnest(cols = redcap_event_data) |>
        pull(redcap_form_name) |>
        unique(),
      width = "100%"
    ),
    width = "33%"
  ),
  card(
    DTOutput("table")
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

  table_to_show <- reactive({

    if (input$event != "All") {

      event_data <- data_app |>
        filter(redcap_event_name == input$event)

    } else {

      event_data <- data_app

    }

    selected_form <- event_data |>
      unnest(cols = redcap_event_data) |>
      filter(redcap_form_name == input$form) |>
      select(redcap_event_name, redcap_form_name, redcap_form_data) |>
      unnest(cols = redcap_form_data) |>
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

    if (data_app[1, 1] == "No events") selected_form[ ,-1] else selected_form


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


}

shinyApp(ui = ui, server = server)
