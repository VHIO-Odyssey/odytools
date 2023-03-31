#' @importFrom rlang .data

# Helper function to count and calculate proportions of discrete variables.
# Previous controls should ensure that this function is not feed with
# continuous variables.
count_prop <- function(disc_var,
                       use_NA = c("no", "ifany", "always"),
                       prop_dec = 3) {

  if (ncol(disc_var) == 1) {
    raw_count <- table(disc_var, useNA = use_NA)
    tibble::tibble(
      level_name = names(raw_count),
      n = as.numeric(raw_count),
      prop = round(prop.table(raw_count), prop_dec) |> as.numeric()
    )
  } else {
    table(disc_var, useNA = use_NA)
  }

}

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

# Helper function to filter each variable of a data.frame according to a
# conditions_list. It returns a list where each element is a data.frame with
# a variable. If required, the variable is filtered according to
# conditions_list. by_var variables are also added if required.
filter_with_conditions_list <- function(data_frame,
                                        conditions_list = NULL,
                                        by_var = NULL) {

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
