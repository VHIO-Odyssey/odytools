data_frame <- medicaldata::smartpill |>
  janitor::clean_names() |>
  dplyr::select(group:age) |>
  dplyr::mutate(
    gender = factor(gender, labels = c("M", "F")),
    date = stringr::str_c(
      sample(28, nrow(medicaldata::smartpill), replace = TRUE),
      sample(12, nrow(medicaldata::smartpill), replace = TRUE),
      sample(1983:2023, nrow(medicaldata::smartpill), replace = TRUE),
      sep = "-"
    ) |>
      lubridate::dmy()
  )

conditions_list <- list(
  gender = "age <= 80",
  race = "",
  height = "",
  weight = "",
  age = ""
)

foo_vars <- make_var_list(
  data_frame,
  complete_list(conditions_list), "gender"
)
foo_vars <- make_var_list(
  data_frame,
  complete_list(conditions_list)
)

cont_var <- foo_vars[[6]]
cont_var[, 2] <- factor(cont_var[, 2], levels = c("F", "M"))
cont_var[sample(87, 5), 2] <- NA
cont_var[sample(87, 15), 1] <- NA
summarise_continous_var(cont_var, use_NA = "ifany")

disc_var <- foo_vars[[3]]
disc_var[, 2] <- factor(disc_var[, 2], levels = c("F", "M"))
disc_var[sample(87, 15), 2] <- NA
disc_var[sample(87, 15), 1] <- NA
summarise_discrete_var(disc_var)

date_var <- foo_vars[[7]]
date_var[, 2] <- factor(date_var[, 2], levels = c("F", "M"))
date_var[sample(87, 5), 2] <- NA
date_var[sample(87, 15), 1] <- NA
summarise_continous_var(date_var, use_NA = "no")

medicaldata::smartpill |>
  janitor::clean_names() |>
  dplyr::select(group:age)

data_frame |>
  dplyr::mutate(
    race = factor(race)
  ) |>
  summarise_df()


