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


disc_var <- make_var_list(
  data_frame,
  complete_list(conditions_list), "gender"
)

disc_var <- make_var_list(
  data_frame,
  complete_list(conditions_list)
)

x <- disc_var[[6]][[1]]
x[c(85, 78, 45)] <- NA


cont_var <-  make_var_list(
  data_frame[, 4:6]
)[[1]]

summary_tibble(cont_var[,1])
summary(cont_var[,1])

