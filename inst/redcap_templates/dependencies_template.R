library(odytools)
library(dplyr)
library(stringr)
library(purrr)
library(labelled)
library(here)


# Conflicts resolution
conflicted::conflicts_prefer(
  dplyr::filter
)

#.RData
load(list.files(here(), ".RData$"))
