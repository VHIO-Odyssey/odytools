library(dplyr)
library(tidyr)
library(shiny)
library(shinyjs)
library(shinytitle)
library(shinyalert)
library(bslib)
library(forcats)
library(labelled)
library(stringr)
library(lubridate)
library(DT)
library(purrr)
library(reactable)
library(odytools)
library(shinycssloaders)
library(waiter)
library(openxlsx2)

# These packages are needed for odytools but are declared as Suggest and
# internally checked with rlang::check_installed().
# This ensures rsconnect::appDependencies() detects them.
library(reactablefmtr)
library(htmltools)
library(ggridges)

ui <- page_sidebar(
    theme = bs_theme(version = 5),
    title = "Odyssey REDCap Data Viewer",
    use_shiny_title(),
    useShinyjs(),
    use_waiter(),
    sidebar = sidebar(
      tabsetPanel(
        id = "params",
        type = "hidden",
        tabPanel(
          "token",
          passwordInput("token", HTML("<b>Enter API Token:</b>")),
          actionButton("submit",HTML("<b>Submit</b>"), icon = icon("download"))
        ),
        tabPanel(
          "selectors",
          selectizeInput(
            "dag", HTML("<b>Filter Site</b>"), choices = NULL,
            width = "100%", multiple = TRUE
          ),
          selectizeInput(
            "group", HTML("<b>Project Specific Filters</b>"), choices = NULL,
            width = "100%", multiple = TRUE
          ),
          checkboxInput(
            "intersect", "All selected project specific filters must be met."
          ),
          fileInput(
            "external_table", HTML("<b>Filter From External Table</b>"),
            accept = ".xlsx"
          ),
          actionButton("reset", HTML("<b>Reset External Filter</b>")),
          selectizeInput(
            "patient", HTML("<b>Filter Patient</b>"), choices = NULL,
            width = "100%", multiple = TRUE
          ),
          selectInput(
            "event", HTML("<b>Select Event</b>"), choices = NULL,
            width = "100%"
          ),
          selectInput(
            "form", HTML("<b>Select Form</b>"), choices = NULL,
            width = "100%"
          ),
          textAreaInput(
            "vars", HTML("<b>Variable Merger</b>"),
            height = 375,
            placeholder = "Type the names of the variables you wish to join in this box, following this pattern:\n\nvar1;\nvar2[1, 2];\nvar3[0, -1]\n\nThe values in brackets [] are used to select specific instances. The final result will be filtered according to the selected site and shown in the 'Merged Data' tab.",
          ),
          checkboxInput("hide_loc", "Hide __loc columns"),
          checkboxInput("pivot", "Pivot repeating instances"),
          actionButton("submit_join", HTML("<b>Merge</b>"))
        )
      ),
      width = "15%"
    ),
    navset_card_tab(
        nav_panel(
            popover(
                tagList("Data", bsicons::bs_icon("gear", class = "ms-auto")),
                radioButtons(
                    "data_type",  HTML("<b>Field Type</b>"),
                    c("Raw [Label]", "Labels", "Raw"),
                    inline = TRUE
                )
            ), DTOutput("table") |> withSpinner()
        ),
        nav_panel("Metadata", dataTableOutput("metadata") |> withSpinner()),
        nav_panel(
            popover(
                tagList("Completeness", bsicons::bs_icon("gear", class = "ms-auto")),
                checkboxInput(
                    "count_user_na",
                    "Consider user-defined missing values as regular missing values.",
                    value = FALSE
                )
            ),
            downloadButton("download_completeness", "Download Completeness table", width = "25%"),
            reactableOutput("completeness") |> withSpinner()
        ),
        nav_panel("Descriptive", reactableOutput("descriptive") |> withSpinner()),
        nav_panel("Merged Data", DTOutput("joined_data") |> withSpinner())
    )
)

server <- function(input, output, session) {

    data_app <- eventReactive(
        input$submit, {
            waiter_show( # show the waiter
                html = tagList(
                    spin_fading_circles(),
                    "Please wait while the data is being loaded.
                    This process may take a few minutes."
                )
            )

          if (length(list.files(pattern = "^data_app.RData$")) == 1) {

            load("data_app.RData")
            # After data loading, the file is removed so the next time the new
            # data is properly uploaded
            file.remove("data_app.RData")

          } else {

            data_app <- try(ody_rc_import(input$token))

          }

            if (class(data_app)[1] == "try-error") {
                waiter_hide()
                shinyalert(
                  "Invalid Token", "Reload the page and try again.",
                  showConfirmButton = FALSE,
                  type = "error"
                )
            }

            # Dummy event column for non-longitudinal projects
            if (names(data_app)[1] == "redcap_form_name") {
              data_app <- odytools:::restore_attributes(
                  tibble(
                    redcap_event_name = "No events",
                    redcap_repeating_event = FALSE,
                    redcap_event_data = list(data_app)
                  ), data_app
                )
            }

            waiter_hide()

            data_app
        })

    observeEvent(data_app(), {
      title <- str_c(
        attr(data_app(), "project_info")$project_title,
        ", PID ", attr(data_app(), "project_info")$project_id
      )

      change_window_title(session, title)
    })

    observeEvent(data_app(), {
      updateTabsetPanel(inputId = "params", selected = "selectors")
    })

    # DAG Selector Update
    observeEvent(data_app(), {
      dag <- attr(data_app(), "dag")

      if (is.null(dag)) {
        dag_choices <- "No sites"
        default_dag <- "No sites"
      } else {
        dag_choices <- c("All", dag$unique_group_name)
        names(dag_choices) <- c("All", dag$data_access_group_name)
        default_dag <- "All"
      }

      updateSelectizeInput(
        session,"dag",
        choices = dag_choices,
        selected = default_dag
      )

    })

    # Group Selector Update
    observeEvent(data_app(),{

      # BOB specific patient filtering
      if (attr(data_app(), "project_info")$project_id == 123) {

        group_choices <- c(
          "All",
          "BOB - Module 1",
          "BOB - Module 2",
          "BOB - Module 3",
          "BOB - Treatment ended",
          "BOB - SAE"
        )

        updateSelectizeInput(
          session,"group",
          choices = group_choices,
          selected = "All"
        )

      } else {

        group_choices <- c("No predefined filters")

        updateSelectizeInput(
          session, "group",
          choices = group_choices,
          selected = "No predefined filters"
        )

      }

    })

    # Candidate patients according to selected dag
    patients_dag <- reactive({

      if (any(input$dag %in% c("All", "No sites")) || is.null(input$dag)) {

        attr(data_app(), "subjects")

      } else {

        attr(data_app(), "subjects_dag") |>
          filter(redcap_data_access_group %in% input$dag) |>
          pull(1)

      }


    })

    # Candidate patients according to selected group
    patients_group <- reactive({

      if (
        any(input$group %in% c("All", "No predefined filters")) || is.null(input$group)
        ) {

        attr(data_app(), "subjects")

      } else if (attr(data_app(), "project_info")$project_id == 123) {
        # BOB specific groups

        patients_group_list <- map(
          input$group,
          function(group) {

            if (group == "BOB - Module 1") {

              ody_rc_select(
                data_app(), mtb_registration_module
              ) |>
                filter(mtb_registration_module == "1") |>
                pull(record_id)

            } else if (group == "BOB - Module 2") {

              ody_rc_select(
                data_app(), mtb_registration_module
              ) |>
                filter(mtb_registration_module == "2") |>
                pull(record_id)

            } else if (group == "BOB - Module 3") {

              ody_rc_select(
                data_app(), mtb_registration_module
              ) |>
                filter(mtb_registration_module == "3") |>
                pull(record_id)

            } else if (group == "BOB - Treatment ended") {

              data_app() |>
                ody_rc_select(eot_reason) |>
                filter(!is.na(eot_reason)) |>
                pull(record_id)

            } else if (group == "BOB - SAE") {

              data_app() |>
                ody_rc_select(ae_sae) |>
                filter(ae_sae == "1") |>
                pull(record_id) |>
                unique()

            }
          }
        )

        if (input$intersect) {

          reduce(patients_group_list, intersect)

        } else {

          reduce(patients_group_list, union)

        }

      }

    })

    # Candidate patients according to external table
    values <- reactiveValues(
      upload_state = NULL
    )

    observeEvent(input$external_table, {
      values$upload_state <- "uploaded"
    })

    observeEvent(input$reset, {
      values$upload_state <- "reset"
      reset("external_table")
    })

    patients_external <- reactive({

      if (is.null(input$external_table) || values$upload_state == "reset") {
        return(attr(data_app(), "subjects"))
      }

      external_table <- readxl::read_xlsx(input$external_table$datapath)

      id_var <- attr(data_app(), "id_var")

      if (!id_var %in% colnames(external_table)) {
        shinyalert(
          title = str_c("No ", id_var,  " column detected"),
          text = "The external table must have a column with the same name as the id variable of the project. No filtering will be applied.",
          type = "warning"
        )

        attr(data_app(), "subjects")

      } else {
        unique(external_table[[id_var]])
      }
    })



    patients_list <- reactive({

      c(patients_dag(), patients_group(), patients_external())

    })

    # Patient Selector Update
    observeEvent(patients_list(), {

        patient_choices <- c(
          "All",
          intersect(patients_dag(), patients_group()) |>
            intersect(patients_external())
        )

      updateSelectizeInput(
        session,"patient",
        choices = patient_choices,
        server = TRUE, selected = "All"
      )

    })

    # Events Selector Update
    observeEvent(data_app(), {

      data_app_events <- attr(data_app(), "events")
      data_app_arms <- attr(data_app(), "arms")

      # If there are two or more arms in the project,
      # the arm name is added to the event name
      if (!is.null(data_app_arms) &&
          nrow(data_app_arms) > 1 &&
          !is.null(data_app_events)
        ) {
        data_app_events <- left_join(
          data_app_events,
          data_app_arms,
          by = join_by(arm_num)
        ) |>
          mutate(
            event_name = str_c(name, " - ", event_name)
          )
      }

      if (is.null(data_app_events)) {

        events_choices <- "No events"

      } else {

        present_events <-
          data_app_events |>
          filter(unique_event_name %in% data_app()$redcap_event_name)
        events_choices <- c("All", present_events$unique_event_name)
        names(events_choices) <- c("All", present_events$event_name)
      }

      updateSelectInput(
        session,"event",
        choices = events_choices
      )

    })

    # Forms Selector Update
    observeEvent(
        input$event,
        {
            if (input$event != "All") {

              data_app_forms <- data_app() |>
                    filter(redcap_event_name == input$event) |>
                unnest(cols = redcap_event_data) |>
                pull(redcap_form_name) |>
                unique()

            } else {

              data_app_forms <- data_app() |>
                unnest(cols = redcap_event_data) |>
                pull(redcap_form_name) |>
                unique()
            }

          present_forms <- attr(data_app(), "forms") |>
            filter(instrument_name %in% data_app_forms)
          forms_choices <- present_forms$instrument_name
          names(forms_choices) <- present_forms$instrument_label

          updateSelectInput(
            session, "form", choices = forms_choices,
            selected = ""
          )
        }
    )

    filtered_data <- reactive({

        req(data_app())

        if (any(input$patient == "All")) {

          ody_rc_filter_subject(
            data_app(),
            intersect(patients_dag(), patients_group()) |>
              intersect(patients_external())
          )

        } else {

          ody_rc_filter_subject(data_app(), input$patient)

        }



      })


    raw_table <- reactive({

      req(filtered_data())

      if (input$form == "") {

        tibble()

      } else {

        if (input$event %in% c("All", "No events", "")) {

          event_data <- filtered_data()

        } else {

          event_data <- filtered_data() |>
            filter(redcap_event_name == input$event)

        }

        event_data |>
          unnest(cols = redcap_event_data) |>
          filter(redcap_form_name == input$form) |>
          select(redcap_event_name, redcap_form_name, redcap_form_data) |>
          unnest(cols = redcap_form_data)

      }

    })

    table_to_show <- reactive({

        req(input$form, input$event)

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

        if (data_app()[1, 1] == "No events") formatted_form[ ,-1] else formatted_form

        current_meddra_fields <- names(formatted_form)[
          names(formatted_form) %in% attr(data_app(), "meddra_fields")
        ]

        if (input$data_type != "Raw" && length(current_meddra_fields) > 0) {

          formatted_form <-
            formatted_form |>
            mutate(
              across(
                all_of(current_meddra_fields),
                function(x) {
                  code_tbl <- tibble(code = x)
                  if (input$data_type == "Raw [Label]") {
                    label_tbl <- attr(data_app(), "meddra_codes") |>
                      mutate(
                        label = str_c(code, " [", label, "]")
                      )
                  } else {
                    label_tbl <- attr(data_app(), "meddra_codes")
                  }
                  left_join(code_tbl, label_tbl, by = "code") |>
                    pull("label")
                }
              )
            )

        }

        current_atc_fields <- names(formatted_form)[
          names(formatted_form) %in% attr(data_app(), "atc_fields")
        ]

        if (input$data_type != "Raw" && length(current_atc_fields) > 0) {

          formatted_form <-
            formatted_form |>
            mutate(
              across(
                all_of(current_atc_fields),
                function(x) {
                  code_tbl <- tibble(code = x)
                  if (input$data_type == "Raw [Label]") {
                    label_tbl <- attr(data_app(), "atc_codes") |>
                      mutate(
                        label = str_c(code, " [", label, "]")
                      )
                  } else {
                    label_tbl <- attr(data_app(), "atc_codes")
                  }
                  left_join(code_tbl, label_tbl, by = "code") |>
                    pull("label")
                }
              )
            )

        }

        formatted_form

    })


    output$table <- renderDT({

      validate(
        need(
          nrow(raw_table()) > 0,
          "No data available or no form selected.")
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
        attr(data_app(), "metadata") |>
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

        id_var <- attr(data_app(), "id_var")

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
          "No data available or no form selected.")
      )

      ody_rc_completeness(
        raw_table_comp(),
        id_var = attr(data_app(), "id_var"),
        count_user_na = input$count_user_na,
        conditions_list = "from_metadata",
        metadata = attr(data_app(), "metadata"),
        missing_codes = attr(data_app(), "missing"),
        report = TRUE
      )

    })

    output$download_completeness <- downloadHandler(
        filename = function() {


          str_c(
              attr(data_app(), "project_info")$project_title,
              "_completeness_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".xlsx"
            )
        },
        content = function(file) {
          compl_data <-
            ody_rc_completeness(
                raw_table_comp(),
                id_var = attr(data_app(), "id_var"),
                count_user_na = input$count_user_na,
                conditions_list = "from_metadata",
                metadata = attr(data_app(), "metadata"),
                missing_codes = attr(data_app(), "missing"),
                report = FALSE
            )

          missing_data <- compl_data |>
            mutate(
              issue = "Missing value"
            ) |>
            select(
              field_name = variable,
              condition,
              issue,
              n = n_missing,
              ids =  ids_missing)

          antimissing_data <-
            compl_data |>
            mutate(
              issue = "Unexpected present value"
            ) |>
            select(
              field_name = variable,
              condition,
              issue,
              n = n_antimissing,
              ids = ids_antimissing
            )

          options(openxlsx2.na.strings = "")
          bind_rows(missing_data, antimissing_data) |>
            filter(n > 0) |>
            left_join(
              attr(data_app(), "metadata") |>
                select(field_name, form_name)
            ) |>
            relocate(form_name, .before = field_name) |>
            write_xlsx(file, as_table = TRUE)
        }
    )

    output$descriptive <- renderReactable({

      validate(
        need(
          nrow(raw_table()) > 0,
          "No data available or no form selected.")
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
            attr(data_app(), "metadata") |>
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
        !(chossen_variables %in% attr(data_app(), "metadata")$field_name)
      ]

      validate(
        need(
         length(wrong_variables) == 0,
          str_c("Wrong variable names: ", str_c(wrong_variables, collapse = ", "))
        )
      )

      id_var <- attr(data_app(), "id_var")

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
          "No data available or no form selected."
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
