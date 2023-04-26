

# pdx_response_percentage ----
data_frame <- openxlsx::read.xlsx(
  "C:/Users/eduardogarcia/Documents/garcia_galea/t_monserrat/data/230322 table example for r script.xlsx"
) |>
  janitor::clean_names() |>
  dplyr::select(pdx_id, sensitivity, group, percent_ki67)

ody_pdx_model_sensitivity(data_frame)


