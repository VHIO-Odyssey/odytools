load("sandbox/timepoints_data.RData")





#asume que las fechas están ordenadas de menor a mayor
get_timepoint <- function(extr_date, events_subject) {

  # If no events_subject data, an empty tibble is returned
  if (is.null(events_subject) | is.na(extr_date)) {
    timepoint <- tibble::tibble(
      timepoint = NA, timepoint_date = NA
    )
    next_timepoint <- tibble::tibble(
      next_timepoint = NA, next_timepoint_date = NA
    )
    return(cbind(timepoint, next_timepoint))
  }

  time_dif <- events_subject$event_date - extr_date

  # If all events_subject are after the extraction, an empty tibble is returned
  if (all(time_dif > 0)) {
    timepoint <- tibble::tibble(
      timepoint = NA, timepoint_date = NA
    )
    next_timepoint <- tibble::tibble(
      next_timepoint = NA, next_timepoint_date = NA
    )
    return(cbind(timepoint, next_timepoint))
  }

  timepoint_position <- tail(which(time_dif <= 0), 1)

  timepoint <- events_subject |>
    dplyr::slice(timepoint_position) |>
    dplyr::rename(timepoint = 1, timepoint_date = 2)

  if (timepoint_position < length(time_dif)) {
    next_timepoint <- events_subject |>
      dplyr::slice(timepoint_position + 1) |>
      dplyr::rename(next_timepoint = 1, next_timepoint_date = 2)
  } else {
    next_timepoint <- tibble::tibble(
      next_timepoint = NA, next_timepoint_date = NA
    )
  }

  cbind(timepoint, next_timepoint)

}




events <- eventos
extractions <- extracciones

ody_find_timepoints <- function(extractions, events) {

  # Which column is the extraction date?
  extractions_date_index <- purrr::map_lgl(extractions, lubridate::is.Date)
  extractions_col_name <- names(extractions)[extractions_date_index]

  events_nested <- tidyr::pivot_longer(
    events, -1, names_to = "event_name", values_to = "event_date"
  ) |>
    dplyr::filter(!is.na(event_date)) |>
    # This arrangement is VERY IMPORTANT because get_timepoints assumes dates are
    # in increasing order.
    dplyr::arrange(event_date) |>
    tidyr::nest(events = c("event_name", "event_date"))

  sap_extractions <- names(extractions)[1]
  sap_events <- names(events)[1]

  extr_events <- dplyr::left_join(
    extractions, events_nested,
    by = dplyr::join_by(!!sap_extractions == !!sap_events)
  )

  extr_events_timepoints <- extr_events |>
    dplyr::mutate(
      timepoints = purrr::map2(
        .data[[extractions_col_name]], events, get_timepoint,
        .progress = "Searching timepoints"
      )
    )


  extr_events_timepoints |>
    tidyr::unnest(timepoints) |>
    dplyr::select(-events) |>
    dplyr::mutate(
      timepoint_days = timepoint_date - .data[[extractions_col_name]],
      .after = timepoint_date
    ) |>
    dplyr::mutate(
      next_timepoint_days = next_timepoint_date - .data[[extractions_col_name]],
      .after = next_timepoint_date
    )

}

extr <- extracciones |>
  dplyr::slice(1:1000)

timepoints <- ody_find_timepoints(extr, eventos)




