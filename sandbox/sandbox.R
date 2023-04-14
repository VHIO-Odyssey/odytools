# pdx_response_percentage ----
data_frame <- openxlsx::read.xlsx(
  "C:/Users/eduardogarcia/Documents/garcia_galea/t_monserrat/data/230322 table example for r script.xlsx"
) |>
  janitor::clean_names() |>
  select(pdx_id, sensitivity, group, percent_ki67)


names_df <- names(data_frame)

rnd_fct <- stringr::str_c(
  "- PDX random factor:",
  stringr::str_c("'", names_df[1], "'"), "with",
  unique(data_frame[[1]]) |> length(),
  "subjects", sep = " "
)

sen_fct <- stringr::str_c(
  "- Sensitivity fixed factor:",
  stringr::str_c("'", names_df[2], "'"), "with levels",
  stringr::str_c(
    stringr::str_c("'", unique(data_frame[[2]]), "'"),
    collapse = " and "
  ),
  sep = " "
)

trt_fct <- stringr::str_c(
  "- Treatment fixed factor:",
  stringr::str_c("'", names_df[3], "'"), "with levels",
  stringr::str_c(
    stringr::str_c("'", unique(data_frame[[3]]), "'"),
    collapse = " and "
  ),
  sep = " "
)

resp <- stringr::str_c(
  "- Response variable:", stringr::str_c("'", names_df[4], "'"), sep = " "
)

cat(
 str_c(
    c("Data Structure:", rnd_fct, sen_fct, trt_fct, resp), collapse = "\n"
  )
)

ok <- readline("Please, confirm the data is correct to proceed (y/n): ")

if (ok == "n") stop("Analysis interrupted")


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
