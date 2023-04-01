#' @importFrom rlang .data
#' @importFrom rlang :=

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
# variable is filtered according to conditions_list. by_var variables are also
# added if required.
make_var_list <- function(data_frame, conditions_list = NULL, by_var = NULL) {

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
    function(data_list, rel_index, name_var, condition, by_var) {
      if (rel_index) {
        data_list |>
          dplyr::filter(eval(str2lang(condition))) |>
          dplyr::select(tidyselect::any_of(c(name_var, by_var)))
      } else {
        data_list |>
          dplyr::select(tidyselect::any_of(c(name_var, by_var)))
      }
    },
    by_var
  )

}


# Helper function to create a tibble-like summary
summary_tibble <- function(x) {
  tibble::tibble(
    `Min.` = min(x, na.rm = TRUE),
    `1st Qu.` = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    `3rd. Qu.` = quantile(x, 0.75, na.rm = TRUE),
    `Max.` = max(x, na.rm = TRUE),
    "NA" = sum(is.na(x))
  )
}

summarise_continous_var <- function(cont_var,
                                    use_NA = c("no", "ifany", "always"),
                                    num_dec = 3) {
  "go!"
}

# Helper function to count and calculate proportions of discrete variables.
# Previous controls should ensure that this function is not feed with
# continuous variables.
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
  } else if (ncol(disc_var) == 2) {
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
      )
  } else{
    stop("disc_var has more than 2 columns")
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

