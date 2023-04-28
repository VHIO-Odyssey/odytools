load("sandbox/sandbox_data/timepoints_data.RData")

extr <- extracciones |>
  dplyr::slice(1:3)

result <- ody_define_timepoints(extr, eventos)



result |>
  dplyr::filter(is.na(timepoint))


extracciones |>
  dplyr::filter(sap == "10091041") |>
  ody_define_timepoints(eventos)
