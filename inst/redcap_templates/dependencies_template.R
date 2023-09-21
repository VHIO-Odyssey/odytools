
suppressMessages({

  library(odytools)
  library(rstudioapi)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(labelled)
  library(here)

  # Conflicts resolution
  conflicted::conflicts_prefer(
    dplyr::filter
  )

})

load(here(list.files(here(), ".RData$")))

source(here(list.files(here(), "functions.R$")))

ody_rc_current()
