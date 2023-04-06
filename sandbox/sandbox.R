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

(result <- data_frame |>
  dplyr::mutate(
    group = factor(group),
    race = factor(race)
  ) |>
  ody_summarise_df(
    grouping_var = "gender",
    conditions_list = list(
      race = "group == 1",
      height = "",
      weight = "",
      age = "",
      date = ""
    ),
    exclude = "group",
    use_NA = "ifany"))

result$age$mean


comparison <- compareGroups::compareGroups(
  gender ~ height + age + weight + group, data_frame, na.action = "na.exclude"
)

compareGroups::createTable(comparison)

data_frame |>
  dplyr::select(gender, height, age, weight, group) |>
  make_var_list(grouping_var = "gender") |>
  purrr::map(
    function(data_frame) {
      if (ncol(data_frame) == 1) return("No  groups")
      form <- call(
        "as.formula",
        stringr::str_c(
          colnames(data_frame)[2], " ~ ",
          colnames(data_frame)[1]
        )
      )

      compareGroups::compareGroups(eval(form), data_frame)
    }
  )


