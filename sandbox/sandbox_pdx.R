

# pdx_model_sensitivity ----
data_frame <- openxlsx::read.xlsx(
  "./sandbox/sandbox_data/230322 table example for r script.xlsx"
) |>
  janitor::clean_names() |>
  dplyr::select(pdx_id, sensitivity, group, percent_ki67)

data_frame$percent_ki67 <- rnorm(161, mean = 500)


data_model <- data_frame |>
  mutate(
    "{df_names[1]}" := factor(.data[[df_names[1]]]),
    "{df_names[2]}" := factor(data_frame[[df_names[2]]]),
    "{df_names[3]}" := factor(data_frame[[df_names[3]]]),
  )

ody_pdx_model_sensitivity(
  data_frame, file_dir = "C:/Users/eduardogarcia/Desktop"
)



sys_time_num <- Sys.time() |> stringr::str_remove_all("[^\\d]")

stringr::str_c(
  stringr::str_sub(sys_time_num, 1, 8),
  stringr::str_sub(sys_time_num, 9, 12), sep = "_")

lubridate::minute(time)

lubridate::hour(Sys.time())

as.integer(data_frame$percent_ki67)

perc <- c(5, 58, 75, 22)
num <- c(4.5, 12, 1.3, 69.8)

all(as.integer(perc) == perc)

all(as.integer(num) == num)

no_dec <- all(as.integer(perc) == perc)
from_0_to_100 <- all(between(perc, 0, 100))
