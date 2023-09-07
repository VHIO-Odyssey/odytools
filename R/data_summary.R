#' @importFrom rlang .data :=
#' @importFrom stats quantile


# Helper function to complete a list. Useful to complete a conditions_list.
# It returns the same list with any empty ("") value replaced by the previous
# non-empty one.
complete_list <- function(list_to_complete) {

  list_to_complete_df <- tibble::tibble(
    var = names(list_to_complete),
    cond = list_to_complete |> unlist()
  ) |>
    dplyr::mutate(
      cond = replace(.data$cond, .data$cond == "", NA)
    ) |>
    tidyr::fill(.data$cond)

  list_completed <- as.list(list_to_complete_df$cond)
  names(list_completed) <- names(list_to_complete)

  list_completed

}

# Helper function to create a list of the variables to be described. It returns
# a list where each element is a data.frame with a variable. If required, the
# variable is filtered according to conditions_list. Grouping variables are also
# added if required.
make_var_list <- function(data_frame,
                          conditions_list = NULL,
                          grouping_var = NULL,
                          exclude = NULL) {

  if (!is.null(grouping_var) &
     (!is.character(grouping_var) | length(grouping_var) != 1)) {
    stop("grouping_var must be a length 1 character vector")
  }

  data_nested <- tibble::tibble(
    name_var = names(data_frame),
    rel_index = .data$name_var %in% names(conditions_list),
    data_list = purrr::map(data_frame, ~data_frame)
  )

  if (!is.null(conditions_list)) {

    conditions_df <- tibble::tibble(
      name_var = names(conditions_list),
      condition = unlist(conditions_list)
    )

    data_nest_cond <- dplyr::left_join(
      data_nested, conditions_df,
      by = "name_var"
    )

  } else {
    data_nest_cond <- data_nested |>
      dplyr::mutate(condition = NA)
  }

  filtered_list <- purrr::pmap(
    data_nest_cond,
    function(data_list, rel_index, name_var, condition, grouping_var) {
      if (rel_index) {
        data_list |>
          dplyr::filter(eval(str2lang(condition))) |>
          dplyr::select(tidyselect::any_of(c(name_var, grouping_var)))
      } else {
        data_list |>
          dplyr::select(tidyselect::any_of(c(name_var, grouping_var)))
      }
    },
    grouping_var
  )

  names(filtered_list) <- data_nest_cond$name_var

  # If any grouping_var, it is moved to the first location
  if (is.null(grouping_var)) {
    rearanged_list <- filtered_list
  } else {
    rearanged_list <- c(
      filtered_list[grouping_var],
      filtered_list[names(filtered_list) != grouping_var]
    )
  }

  # Exclude undesired variables
  if (!is.null(exclude)) {
    rearanged_list[names(rearanged_list) != exclude]
} else {
    rearanged_list
  }
}

# Helper function to create a tibble-like summary for both numeric or date
# variables.
summary_tibble <- function(x) {

  if (lubridate::is.Date(x)) {

    tibble::tibble(
      n = sum(!is.na(x)),
      min = min(x, na.rm = TRUE),
      q1 = stats::quantile(x, 0.25, na.rm = TRUE, type = 1),
      median = stats::median(x, na.rm = TRUE),
      q3 = stats::quantile(x, 0.75, na.rm = TRUE, type = 1),
      max = max(x, na.rm = TRUE),
      `<NA>` = sum(is.na(x))
    )

  } else {

    tibble::tibble(
      n = sum(!is.na(x)),
      min = min(x, na.rm = TRUE),
      q1 = stats::quantile(x, 0.25, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      q3 = stats::quantile(x, 0.75, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      `<NA>` = sum(is.na(x))
    )

  }

}

# Helper function to summarise continuous variables (including dates). If there
# is a grouping_var, the summary is performed by each level of grouping_var.
summarise_continous_var <- function(cont_var,
                                    use_NA = c("no", "ifany", "always")) {

  use_NA <- rlang::arg_match(use_NA)

  if (ncol(cont_var) == 1) {
    full_summary <- summary_tibble(cont_var[[1]]) |>
      dplyr::select(-"n")
    if (use_NA == "no" | (use_NA == "ifany" & full_summary[["<NA>"]] == 0)) {
      return(dplyr::select(full_summary, -"<NA>"))
    } else {
       return(full_summary)
    }
  }

  if (ncol(cont_var) == 2) {
    # Grouping var (which is in column 2 of cont_var) is forced as a factor to
    # have control over the order its levels are shown.
    cont_var[, 2] <- factor(cont_var[[2]])
    overall_data <- cont_var[[1]]
    by_group_data <- tidyr::nest(cont_var, data = 1) |>
      dplyr::filter(!is.na(.data[[names(cont_var)[2]]]))
    factor_order <- order(as.numeric(by_group_data[[1]]))
    missing_data <- cont_var[is.na(cont_var[[2]]), ]

    full_summary <- dplyr::bind_rows(
      # Overall summary
      summary_tibble(overall_data),
      # By levels summaries
      purrr::map_dfr(
        by_group_data$data, ~summary_tibble(.[[1]])
      )[factor_order, ],
      # Missing level summary
      summary_tibble(cont_var[is.na(cont_var[[2]]), ][[1]]) |>
        suppressWarnings()
    ) |>
      dplyr::mutate(
        "{names(cont_var)[2]}" := c(
          "Overall", levels(by_group_data[[1]]), "<NA>"
        ),
        .before = "n"
      )

    missing_levels <- utils::tail(full_summary$n, 1) != 0
    missing_values <- full_summary$`<NA>`[1] != 0

  }

  if (
    use_NA == "no" |
    (use_NA == "ifany" & !missing_levels & !missing_values)
  ) {
    return(
      full_summary |>
        dplyr::filter(.data[[colnames(cont_var)[2]]] != "<NA>") |>
        dplyr::select(-.data$`<NA>`)
    )
  } else if (use_NA == "always") {
    return(full_summary)
  } else {
    if (!missing_levels) {
      full_summary <- full_summary |>
        dplyr::filter(.data[[colnames(cont_var)[2]]] != "<NA>")
    }
    if (!missing_values) {
      full_summary <- full_summary |>
        dplyr::select(-.data$`<NA>`)
    }
    return(full_summary)
  }

}

# Helper function to count and calculate proportions of discrete variables.
# 1D table if no grouping_var is suplied to make_var_list. Otherwise, a 2d table
# including an overall.
summarise_discrete_var <- function(disc_var,
                                   use_NA = c("no", "ifany", "always")) {

  if (ncol(disc_var) == 1) {
    # 1D count
    raw_count <- table(disc_var, useNA = use_NA)

    count_prop_table <- tibble::tibble(
      "{colnames(disc_var)}" := names(raw_count),
      n = as.numeric(raw_count),
      prop = as.numeric(prop.table(raw_count))
    )
  }

  if (ncol(disc_var) == 2) {
    # 2D count
    raw_table <- table(disc_var, useNA = use_NA)
    raw_n_table <- tibble::as_tibble(raw_table) |>
      dplyr::mutate(
        # NA is replacd by <NA> which is a more improbable wild level name.
        "{colnames(disc_var)[2]}" := tidyr::replace_na(
          .data[[colnames(disc_var)[2]]], "<NA>"
        )
      )
    raw_prop_table <- prop.table(raw_table, 2) |>
      tibble::as_tibble() |>
      dplyr::rename(prop = "n") |>
      dplyr::mutate(
        # NA is replacd by <NA> which is a more improbable wild level name.
        "{colnames(disc_var)[2]}" := tidyr::replace_na(
          .data[[colnames(disc_var)[2]]], "<NA>"
        )
      )
    # Unstratified overall count
    raw_overall <- table(disc_var[, 1], useNA = use_NA)
    count_prop_overall <- tibble::tibble(
      "{colnames(disc_var[1])}" := names(raw_overall),
      "{colnames(disc_var[2])}" := "overall",
      n = as.numeric(raw_overall),
      prop = as.numeric(prop.table(raw_overall))
    )

    count_prop_table <- dplyr::bind_rows(
      count_prop_overall,
      dplyr::left_join(raw_n_table, raw_prop_table)
    )|>
      tidyr::pivot_wider(
        names_from = colnames(disc_var[2]),
        values_from = c("n", "prop"),
        names_vary = "slowest"
      ) |>
      suppressMessages()
  }

  count_prop_table

  # NA to "<NA>" and NaN to 0
  count_prop_table |>
    dplyr::mutate(
      "{colnames(disc_var[1])}" := tidyr::replace_na(
        .data[[colnames(disc_var[1])]], "<NA>"
      ),
      dplyr::across(
        tidyselect::everything(),
        function(x) {
          replace(x, is.nan(x), 0)
          # replace(x, is.na(x), "<NA>")
        }
      )
    )
}


# Helper function to create the detail subtable of a discrete variable in the
# reactable summary. It handles both unique and by var tables.
make_discrete_detail_tbl <- function(detail_tbl,
                                     details_tbl,
                                     var_list_case,
                                     grouping_var,
                                     opt_reactable = opt_reactable) {

  prop_names <- names(detail_tbl) |>
    stringr::str_subset("^Percentage") |>
    purrr::map_chr(
      ~stringr::str_c("`", ., "`")
    )
  n_names <- names(detail_tbl) |>
    stringr::str_subset("^N_|^N$") |>
    purrr::map_chr(
      ~stringr::str_c("`", ., "`")
    )

  # Código para hacer barras
  detail_tbl_code <- stringr::str_c(
    "list(",
    "No = reactable::colDef(minWidth = 40), Level = reactable::colDef(minWidth = opt_reactable$minwidth_level, resizable = TRUE), ",
    stringr::str_c(n_names, " = ", "reactable::colDef(name = 'N', minWidth = 70)") |>
      stringr::str_c(collapse = ", "),
    ", ",
    stringr::str_c(
      prop_names, " = ",
      "reactable::colDef(name = 'Percentage', cell = reactablefmtr::data_bars(detail_tbl, text_position = 'above',number_fmt = scales::percent, max_value = 1), width = opt_reactable$width_bar)"
    ) |>
      stringr::str_c(collapse = ", "),
    ")"
  ) |>
    str2lang()

  if (ncol(detail_tbl) > 4) {

    grouping_var_levels <- c("Overall", var_list_case[[2]] |> levels())

    grouping_var_num <- detail_tbl |>
      dplyr::select(dplyr::starts_with("N_")) |>
      purrr::map_dbl(sum)

    grouping_var_cols <- purrr::map(
      grouping_var_levels,
      ~ detail_tbl |>
        dplyr::select(
          dplyr::matches(stringr::str_c("_", ., "$"))
        ) |>
        names() %>%
        {stringr::str_c("'", ., "'")} |>
        stringr::str_c(collapse = ", ") %>%
        {stringr::str_c("c(", ., ")")}
    )
    grouping_colGroups <- stringr::str_c(
      "reactable::colGroup(name = ", stringr::str_c("'", stringr::str_c(grouping_var, ": ", grouping_var_levels, " (", grouping_var_num, ")"), "'"), ", columns = ", grouping_var_cols, ")"
    ) |>
      stringr::str_c(collapse = ", ")

    if (opt_reactable$full_group_label) {

      grouping_colGroups <- grouping_colGroups |>
        stringr::str_remove(stringr::str_c(grouping_var, ": "))

    } else {

      grouping_colGroups <- grouping_colGroups |>
        stringr::str_remove_all(stringr::str_c(grouping_var, ": "))

    }

    grouping_colGroups_code <- stringr::str_c(
      "list(", grouping_colGroups, ")") |>
      str2lang()

  } else {

    grouping_colGroups_code <- NULL

  }

  reactable::reactable(
    detail_tbl,
    columns = eval(detail_tbl_code),
    columnGroups = eval(grouping_colGroups_code),
    fullWidth = FALSE, highlight = TRUE,
    pagination = FALSE, wrap = FALSE
  )

}

# Helper function to create the detail subtable of a continuous variable in the
# reactable summary. It handles both unique and by var tables.
make_continuous_detail_table <- function(detail_tbl,
                                         var_list_case,
                                         opt_reactable = opt_reactable,
                                         output) {

  if (output == "table") {

    detail_tbl |>
      reactable::reactable(
        columns = stringr::str_c(
          "list(",
          names(detail_tbl)[2],
          " = reactable::colDef(minWidth = opt_reactable$minwidth_level), No = reactable::colDef(minWidth = 40), Median = reactable::colDef(minWidth = 75))"
          ) |>  str2lang() |> eval(),
        defaultColDef = reactable::colDef(
          minWidth = (
            opt_reactable$width_density_plot - opt_reactable$minwidth_level
            ) / (ncol(detail_tbl) - 1)
        ),
        fullWidth = FALSE, highlight = TRUE, resizable = TRUE
      )

  } else {

    if (ncol(var_list_case[[1]]) == 1) {

      plot_data <-  var_list_case[[1]] |>
        dplyr::rename(x = 1) |>
        na.omit()

      ggplot2::ggplot(plot_data, ggplot2::aes(.data$x)) +
        ggridges::geom_density_line() +
        ggplot2::labs(
          x = names(var_list_case[[1]])[1]
        ) +
        ggplot2::xlim(min(plot_data$x), max(plot_data$x)) +
        ggridges::theme_ridges(font_size = 16) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())

    } else {

      plot_data <- var_list_case[[1]] |>
      dplyr::rename(x = 1, y = 2) |>
      na.omit()

      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(.data$x, .data$y, fill = ggplot2::after_stat(quantile))) +
        ggridges::stat_density_ridges(quantile_lines = FALSE,
                                      calc_ecdf = TRUE, scale = 2,
                                      geom = "density_ridges_gradient",
                                      show.legend = FALSE) +
        ggplot2::scale_fill_brewer(name = "")+
        ggplot2::labs(
          x = names(var_list_case[[1]])[1],
          y = names(var_list_case[[1]])[2]
        ) +
        ggplot2::xlim(min(plot_data$x), max(plot_data$x)) +
        ggridges::theme_ridges(font_size = 16)

    }

  }
}

#' ody_summarise_df
#'
#' @param data_frame data frame
#' @param grouping_var A grouping variable
#' @param conditions_list Conditions list to filter variables
#' @param exclude Variables that should be excluded
#' @param compare_groups Compare groups?
#' @param use_NA Add missing values in the report?
#' @param min_distinct Minimal number of distinct cases in a numeric variable to be described as numeric. If the number of distinct cases is lower, the variable is described as it was a factor.
#' @param raw_summary If TRUE, the function returns a raw summary instead of the defaiult reactable report.
#' @param show_conditions If TRUE, the filtering conditions defined by conditions_list ( if any) appear at each filtered variable.
#' @param opt_reactable Reactable options. A call to ody_options
#' @param ... Further aditional options to be directly passed to reactable
#'
#' @return A list
#' @details
#' If grouping_var exists and compare_groups = TRUE, the code runs compareGroups::compareGroups(grouping_var ~ variable).
#'
#' @export
ody_summarise_df <- function(data_frame,
                             grouping_var = NULL,
                             conditions_list = NULL,
                             exclude = NULL,
                             compare_groups = c(
                               "no", "p_value", "q_value", "both"
                              ),
                             use_NA = c("no", "ifany", "always"),
                             min_distinct = 5,
                             raw_summary = FALSE,
                             show_conditions = TRUE,
                             opt_reactable = ody_options(),
                             ...) {

  use_NA <- rlang::arg_match(use_NA)
  compare_groups <- rlang::arg_match(compare_groups)

  if (!is.null(conditions_list)) {
    conditions_list <- complete_list(conditions_list)
  }

  data_frame <- data_frame |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        \(x) if (length(unique(x)) >= min_distinct) x else factor(x)
      )
    )

  var_list <- make_var_list(
    data_frame, conditions_list, grouping_var, exclude
  )

  raw_details_tbl <- purrr::map(
    var_list,
    function(x) {

      # Descriptive
      if (is.numeric(x[[1]]) | lubridate::is.Date(x[[1]])) {
        descriptive <- summarise_continous_var(x, use_NA)
      } else {
        descriptive <- summarise_discrete_var(x, use_NA)
      }

    }

  )

  if (raw_summary) return(raw_details_tbl)

  main_tbl <- purrr::map2_dfr(
    names(var_list), var_list,
    ~dplyr::tibble(
      Variable = .x,
      Observed = sum(!is.na(.y[[1]])),
      Missing = sum(is.na(.y[[1]])),
      Completeness = Observed / nrow(.y)
    )
  ) |>
    dplyr::mutate(
      labels = labelled::var_label(
        dplyr::select(data_frame, dplyr::all_of(names(var_list))), unlist = TRUE
      ),
      No = 1:dplyr::n(), .before = 1
    )

  # Group comparisons
  if (!is.null(grouping_var) && compare_groups != "no") {

    gt_summary <- gtsummary::tbl_summary(data_frame, by = grouping_var) |>
      gtsummary::add_p()

    comparisons <- gt_summary$meta_data$test_result |>
      purrr::map_dfr(
        ~.$df_result |>
          dplyr::select(method, p.value)
      ) |>
      dplyr::mutate(
        variable = gt_summary$meta_data$variable, .before = 1
      ) |>
      dplyr::mutate(
        q.value = p.adjust(p.value, "fdr"),
        dplyr::across(
          dplyr::where(is.numeric),
          ~dplyr::if_else(. < 0.001, "<0.001", round(., 3) |> as.character())
        )
      ) |>
      dplyr::rename(
        Test = "method",
        `p-value` = "p.value",
        `q-value` = "q.value"
      )

    if (compare_groups == "both") {
      selected_vars <- c("variable", "Test", "p-value", "q-value")
    }

    if (compare_groups == "p_value") {
      selected_vars <- c("variable", "Test", "p-value")
    }

    if (compare_groups == "q_value") {
      selected_vars <- c("variable", "Test", "q-value")
    }

    comparisons <- comparisons |>
      dplyr::select(tidyselect::all_of(selected_vars))

  } else {
    comparisons <- NULL
  }

  # Conditions df to show in details
  if (!is.null(conditions_list) && show_conditions) {

    conditions_df <- purrr::map2_dfr(
      names(conditions_list), conditions_list,
      ~dplyr::tibble(variable = .x, Condition = .y)
    )

  } else {

    conditions_df <- NULL

  }

  # Tables used as expandable details
  details_tbl <- purrr::map2(
    names(raw_details_tbl), raw_details_tbl,
    function(x, y) {
      if (x == names(y)[1]) {

        names(y) <- names(y) |> stringr::str_replace("^n", "N") |>
          stringr::str_replace("^prop", "Percentage") |>
          stringr::str_replace("overall$", "Overall")

        y |>
          dplyr::rename(
            Level = 1
          ) |>
          dplyr::mutate(
            No = 1:dplyr::n(), .before = 1
          )

      } else {
        tbl <- y |>
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.numeric), ~round(., opt_reactable$n_dec)
            )
          )

        names(tbl) <- names(tbl) |>
          stringr::str_replace("^min$", "Min.") |>
          stringr::str_replace("^n$", "N") |>
          stringr::str_replace("^q1$", "Q1") |>
          stringr::str_replace("^median$", "Median") |>
          stringr::str_replace("^mean$", "Mean") |>
          stringr::str_replace("^sd$", "SD") |>
          stringr::str_replace("^q3$", "Q3") |>
          stringr::str_replace("^max$", "Max.")

        if (names(tbl)[1] != "Min.") {

          tbl |>
            dplyr::mutate(No = 1:dplyr::n(), .before = 1)

        } else {

          tbl
        }
      }
    }
  )

  # Definición de que se ha de tratar como descriptivo y qué como continuo
  details_type <- purrr::map2(
    names(raw_details_tbl), raw_details_tbl,
    ~dplyr::if_else(.x == names(.y)[1], "discrete", "continuous")
  )

  reactable::reactable(
    main_tbl,
    columns = list(
      No = reactable::colDef(width = 50),
      Variable = reactable::colDef(
        cell = function(value, index) {
          label_text <- main_tbl$labels[[index]]
          if (label_text == "") {
            htmltools::div(style = "font-weight: 600", value)
          } else {
            htmltools::div(
              htmltools::div(style = "font-weight: 600", value),
              htmltools::div(
                style = stringr::str_c(
                  "font-size:", opt_reactable$label_size, "rem"
                ),
                label_text
              )
            )
          }
        },
        resizable = TRUE, minWidth = opt_reactable$minwidth_var
      ),
      Observed = reactable::colDef(width = 90),
      Missing =  reactable::colDef(width = 90),
      Completeness = reactable::colDef(
        cell = reactablefmtr::data_bars(
          main_tbl, text_position = "above",
          max_value = 1, number_fmt = scales::percent
        ), width = 150
      ),
      labels = reactable::colDef(show = FALSE)
    ),
    details = function(index) {

      # Condition to show if any
      if (!is.null(conditions_df)) {

        condition <- conditions_df |>
          dplyr::filter(
            variable == names(var_list)[index]
          ) |>
          dplyr::select(
            -variable
          )

        if (nrow(condition) == 0) {
          condition <- NULL
        } else {
          condition <- reactable::reactable(
            condition, opt_reactable$width_density_plot,
            highlight = TRUE, sortable = FALSE, fullWidth = FALSE
          )
        }

      } else {

        condition <- NULL

      }

      # Comparison to show if any
      if (!is.null(comparisons)) {

        comparison <- comparisons |>
          dplyr::filter(
            variable == names(var_list)[index]
          ) |>
          dplyr::select(
            -variable
          )

        if (nrow(comparison) == 0) {
          comparison <- NULL
        } else {
          comparison <- reactable::reactable(
            comparison,
            columns = list(
              Test = reactable::colDef(minWidth = 200, resizable = TRUE)
            ), highlight = TRUE, sortable = FALSE, fullWidth = FALSE
          )
        }

      } else {

        comparison <- NULL

      }


      if (details_type[[index]] == "discrete") {

        details_reactable <- make_discrete_detail_tbl(
          details_tbl[[index]],
          details_tbl,
          var_list[[index]],
          grouping_var,
          opt_reactable = opt_reactable
        )

        details_expr <- htmltools::div(
          style = list(margin = "12px 45px"), details_reactable
        ) |> dplyr::expr()

        density_expr <- htmltools::div(
          htmltools::div(
            style = list(margin = "12px 45px"), NULL
          )
        ) |> dplyr::expr()

      } else {

        details_reactable <- make_continuous_detail_table(
          details_tbl[[index]], var_list[index],
          opt_reactable = opt_reactable, output = "table"
        )

        density_reactable <- make_continuous_detail_table(
          details_tbl[[index]], var_list[index],
          opt_reactable = opt_reactable, output = "plot"
        )

        details_expr <- htmltools::div(
          style = list(margin = "12px 45px"), details_reactable
        ) |> dplyr::expr()

        density_expr <- htmltools::div(
          htmltools::div(
            style = list(margin = "12px 45px"),
            htmltools::plotTag(
              density_reactable, alt = "by_var_plot",
              height = opt_reactable$groups_plot_height,
              width = opt_reactable$width_density_plot
            )
          )
        ) |> dplyr::expr()

      }

      htmltools::div(
        htmltools::div(
          style = list(margin = "12px 45px"), condition
        ),
        eval(details_expr),
        eval(density_expr),
        htmltools::div(
          style = list(margin = "12px 45px"), comparison
        )
      )


    },
    theme = reactable::reactableTheme(
      borderWidth = 1, borderColor =c("#000000")
    ),
    onClick = "expand", highlight = TRUE, wrap = FALSE,...
  )

}
