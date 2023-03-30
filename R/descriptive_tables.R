# Helper function to count and calculate proportions of discrete variables
count_prop <- function(disc_var,
                       use_NA = c("no", "ifany", "always"),
                       prop_dec = 3) {

  if (!is.factor(disc_var)) warning("The variable is not a factor.")

  raw_count <- table(disc_var, useNA = use_NA)
  tibble::tibble(
    level_name = names(raw_count),
    n = as.numeric(raw_count),
    prop = round(n / sum(n), prop_dec)
  )

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
      cond = replace(cond, cond == "", NA)
    ) |>
    tidyr::fill(cond)

  list_completed <- as.list(list_to_complete_df$cond)
  names(list_completed) <- names(list_to_complete)

  list_completed

}

# Helper function to filter each variable of a data.frame or list according to
# a conditions_list.
