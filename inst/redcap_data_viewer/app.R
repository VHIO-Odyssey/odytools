library(dplyr)
library(tidyr)
library(shiny)
library(forcats)
library(labelled)
library(stringr)
library(lubridate)
library(DT)
library(purrr)
load("data_app.RData")
app_title <- str_c(
  attr(data_app, "project_info")$project_title, " ",
  attr(data_app, "export_date")
)

# Puede haber events 100% vacíos. Se eliminan.
data_app <- data_app[map_dbl(data_app[[3]], nrow) != 0, ]

ui <- fluidPage(
  titlePanel(app_title),
  fluidRow(
    column(3,
           selectInput(
             "event", "Event", c("All", data_app$redcap_event_name),
             width = "100%"
           )

    ),
    column(6,
           selectInput(
             "form", "Form", data_app |>
               unnest(cols = redcap_event_data) |>
               pull(redcap_form_name) |>
               unique(),
             width = "100%"
           )
    ),
    column(12, DTOutput("table"))
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

    data_app |>
      filter(redcap_event_name == input$event) |>
      unnest(cols = redcap_event_data) |>
      filter(redcap_form_name == input$form) |>
      select(redcap_form_data) |>
      unnest(cols = redcap_form_data) |>
      mutate(
        across(
          everything(),
          function(x) {

            if(raw) return(x)

            if(is.null(var_label(x))) return(x)

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

            if (str_detect(var_label(x), "(text:integer)|(text:number)")) {
              return(as.numeric(unclass(x)))
            }

            if (str_detect(var_label(x), "(text:date_dmy)")) {
              return(ymd(unclass(x)))
            }

            if (str_detect(var_label(x), "(text:datetime_dmy)")) {
              return(ymd_hm(unclass(x)))
            }

            return(x)

          }
        ),
        across(1:3, factor)
      ) |>
      select(where(~ !is.logical(.)))
    } else {

      data_app |>
        unnest(cols = redcap_event_data) |>
        filter(redcap_form_name == input$form) |>
        select(redcap_event_name, redcap_form_data) |>
        unnest(cols = redcap_form_data) |>
        mutate(
          across(
            everything(),
            function(x) {

              if(raw) return(x)

              if(is.null(var_label(x))) return(x)

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

              if (str_detect(var_label(x), "(text:integer)|(text:number)")) {
                return(as.numeric(unclass(x)))
              }

              if (str_detect(var_label(x), "(text:date_dmy)")) {
                return(ymd(unclass(x)))
              }

              if (str_detect(var_label(x), "(text:datetime_dmy)")) {
                return(ymd_hm(unclass(x)))
              }

              return(x)

            }
          ),
          across(1:4, factor)
        ) |>
        select(where(~ !is.logical(.)))
    }
  })

  output$table <- renderDT(DT::datatable(
    table_to_show(), filter = "top", fillContainer = FALSE, options = list(paging = FALSE)
  ))

}

shinyApp(ui = ui, server = server)
