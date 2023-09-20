
suppressMessages({

  library(odytools)
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

load(list.files(here(), ".RData$"))

ody_rc_current(redcap_data)
