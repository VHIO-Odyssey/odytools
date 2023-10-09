
suppressMessages({

  library(odytools)
  library(rstudioapi)
  library(here)
  library(dplyr)
  library(readr)
  library(forcats)
  library(stringr)
  library(ggplot2)
  library(tibble)
  library(lubridate)
  library(tidyr)
  library(purrr)
  library(labelled)

  # Conflicts resolution
  conflicted::conflicts_prefer(
    dplyr::filter,
    dplyr::lag
  )

})

load(here(list.files(here(), ".RData$")))

here("functions", list.files(here("functions"), ".R$")) |>
  walk(source)

ody_rc_current()
