#' @importFrom rlang .data :=

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
make_var_list <- function(data_frame, conditions_list = NULL, grouping_var = NULL) {

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

  purrr::pmap(
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

}

# Helper function to create a tibble-like summary for both numeric or date
# variables.
summary_tibble <- function(x, num_dec = 3) {

  if (lubridate::is.Date(x)) {

    tibble::tibble(
      N = sum(!is.na(x)),
      `Min.` = min(x, na.rm = TRUE),
      `1st Qu.` = stats::quantile(x, 0.25, na.rm = TRUE, type = 1),
      Median = stats::median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE) |> round(num_dec),
      `3rd Qu.` = stats::quantile(x, 0.75, na.rm = TRUE, type = 1),
      `Max.` = max(x, na.rm = TRUE),
      `NA` = sum(is.na(x))
    )

  } else {

    tibble::tibble(
      N = sum(!is.na(x)),
      `Min.` = min(x, na.rm = TRUE),
      `1st Qu.` = stats::quantile(x, 0.25, na.rm = TRUE),
      Median = stats::median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE) |> round(num_dec),
      SD = stats::sd(x, na.rm = TRUE) |> round(num_dec),
      `3rd Qu.` = stats::quantile(x, 0.75, na.rm = TRUE),
      `Max.` = max(x, na.rm = TRUE),
      `NA` = sum(is.na(x))
    )

  }

}

# Helper function to summarise continuous variables (including dates). If there
# is a grouping_var, the summary is performed by each level of grouping_var.
summarise_continous_var <- function(cont_var,
                                    use_NA = c("no", "ifany", "always"),
                                    num_dec = 3) {

  use_NA <- rlang::arg_match(use_NA)

  if (ncol(cont_var) == 1) {
    full_summary <- summary_tibble(cont_var[[1]]) |>
      dplyr::select(-N)
    if (use_NA == "no" | (use_NA == "ifany" & full_summary[["NA"]] == 0)) {
      return(dplyr::select(full_summary, -`NA`))
    } else {
       return(full_summary)
    }
  }

  if (ncol(cont_var) == 2) {
    # Grouping var (which is in column 2 of cont_var) is forced as a factor to
    # have control over the order its levels are shown.
    cont_var[, 2] <- factor(cont_var[, 2])
    overall_data <- cont_var[[1]]
    by_group_data <- tidyr::nest(cont_var, data = 1) |>
      dplyr::filter(!is.na(.data[[names(cont_var)[2]]]))
    factor_order <- order(as.numeric(by_group_data[[1]]))
    missing_data <- cont_var[is.na(cont_var[[2]]), ]

    full_summary <- dplyr::bind_rows(
      # Overall summary
      summary_tibble(overall_data, num_dec),
      # By levels summaries
      purrr::map_dfr(
        by_group_data$data, ~summary_tibble(.[[1]]), num_dec
      )[factor_order, ],
      # Missing level summary
      summary_tibble(cont_var[is.na(cont_var[[2]]), ][[1]]) |>
        suppressWarnings()
    ) |>
      dplyr::mutate(
        "{names(cont_var)[2]}" := c(
          "Overall", levels(by_group_data[[1]]), "NA"
        ),
        .before = "N"
      )

    missing_levels <- tail(full_summary$N, 1) != 0
    missing_values <- full_summary$`NA`[1] != 0

  }

  if (
    use_NA == "no" |
    (use_NA == "ifany" & !missing_levels & !missing_values)
  ) {
    return(
      full_summary |>
        dplyr::filter(.data[[colnames(cont_var)[2]]] != "NA") |>
        dplyr::select(-`NA`)
    )
  } else if (use_NA == "always") {
    return(full_summary)
  } else {
    if (!missing_levels) {
      full_summary <- full_summary |>
        dplyr::filter(.data[[colnames(cont_var)[2]]] != "NA")
    }
    if (!missing_values) {
      full_summary <- full_summary |>
        dplyr::select(-`NA`)
    }
    return(full_summary)
  }

}

# Helper function to count and calculate proportions of discrete variables.
# 1D table if no grouping_var is suplied to make_var_list. Otherwise, a 2d table
# including an overall.
summarise_discrete_var <- function(disc_var,
                                   use_NA = c("no", "ifany", "always"),
                                   prop_dec = 3) {

  if (ncol(disc_var) == 1) {
    # 1D count
    raw_count <- table(disc_var, useNA = use_NA)

    count_prop_table <- tibble::tibble(
      "{colnames(disc_var)}" := names(raw_count),
      n = as.numeric(raw_count),
      prop = round(prop.table(raw_count), prop_dec) |> as.numeric()
    )
  }

  if (ncol(disc_var) == 2) {
    # 2D count
    raw_table <- table(disc_var, useNA = use_NA)
    raw_n_table <- tibble::as_tibble(raw_table)
    raw_prop_table <- prop.table(raw_table, 2) |>
      tibble::as_tibble() |>
      dplyr::rename(prop = "n") |>
      dplyr::mutate(
        prop = round(.data$prop, prop_dec)
      )
    # Unstratified overall count
    raw_overall <- table(disc_var[, 1], useNA = use_NA)
    count_prop_overall <- tibble::tibble(
      "{colnames(disc_var[1])}" := names(raw_overall),
      "{colnames(disc_var[2])}" := "overall",
      n = as.numeric(raw_overall),
      prop = round(prop.table(raw_overall), prop_dec) |> as.numeric()
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

  # NA to "NA" and NaN to 0
  count_prop_table |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        function(x) {
          x <- replace(x, is.nan(x), 0)
          replace(x, is.na(x), "NA")
        }
      )
    )
}

# Function to run summarise_ functions for each variable of a data.frame
summarise_df <- function(data_frame,
                         grouping_var = NULL,
                         conditions_list = NULL,
                         use_NA = c("no", "ifany", "always"),
                         num_dec = 3, prop_dec = 3) {

  use_NA <- rlang::arg_match(use_NA)

  if (!is.null(conditions_list)) {
    conditions_list <- complete_list(conditions_list)
  }

  var_list <- make_var_list(data_frame, conditions_list, grouping_var)

  summaries <- purrr::map(
    var_list,
    function(x) {
      if (is.numeric(x[[1]]) | lubridate::is.Date(x[[1]])) {
        summarise_continous_var(x, use_NA, num_dec)
      } else {
        summarise_discrete_var(x, use_NA, prop_dec)
      }
    }
  )

  names(summaries) <- purrr::map_chr(var_list, ~colnames(.)[1])

  summaries

}
