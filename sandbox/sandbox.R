# data_summary ----
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

data_frame$gender[sample(95, 15)] <- NA
data_frame$race[sample(95, 15)] <- NA

data_frame |>
  dplyr::mutate(
    group = factor(group),
    race = factor(race)
  ) |>
  ody_summarise_df(
    grouping_var = "gender",
    compare_groups = TRUE,
    exclude = "group",
    use_NA = "ifany"
  )


data_frame |>
  dplyr::group_by(gender) |>
  dplyr::summarise(
    altura = mean(height)
  )
