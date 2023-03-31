data_frame <- medicaldata::smartpill |>
  janitor::clean_names() |>
  dplyr::select(group:age) |>
  dplyr::mutate(gender = factor(gender, labels = c("M", "F")))

conditions_list <- list(
  gender = "group == '1'",
  race = "",
  height = "",
  weight = "",
  age = ""
)


disc_var <- filter_with_conditions_list(
  data_frame,
  complete_list(conditions_list), "gender"
)[[1]]

# disc_var <- filter_with_conditions_list(
#   data_frame,
#   complete_list(conditions_list)
# )[[1]]

raw_table <- count_prop(disc_var) |>
  tibble::as_tibble()

raw_prop_table <- count_prop(disc_var) |>
  prop.table(2) |>
  tibble::as_tibble() |>
  dplyr::rename(prop = n)

final_table <- dplyr::left_join(raw_table, raw_prop_table) |>
  tidyr::pivot_wider(
    names_from = colnames(disc_var[2]),
    values_from = c(n, prop),
    names_vary = "slowest"
  )


disc_var
compared <- compareGroups::compareGroups(gender ~ group, disc_var)
compareGroups::createTable(compared)
summary(compared)
