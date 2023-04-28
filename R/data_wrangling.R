
# Helper of:
#   - ody_find_timepoints
# extr_date: A lenght 1 date vector. Is the date to locate
# events_subject: data frame with the coulmn event_date, an ordered sequence
# of dates.
# CRITICAL: dates in events_subject must be ordered in increasing order.
get_timepoint <- function(extr_date, events_subject) {

  # If no events_subject data or extraction date, an empty tibble is returned
  if (is.null(events_subject) | is.na(extr_date)) {
    timepoint <- tibble::tibble(
      timepoint = NA_character_, timepoint_date = as.Date(NA)
    )
    next_timepoint <- tibble::tibble(
      next_timepoint = NA_character_, next_timepoint_date = as.Date(NA)
    )
    return(cbind(timepoint, next_timepoint))
  }

  time_dif <- events_subject$event_date - extr_date

  # If all events_subject are after the extraction,
  # only next_timepoint is returned
  if (all(time_dif > 0)) {
    timepoint <- tibble::tibble(
      timepoint = NA_character_, timepoint_date = as.Date(NA)
    )
    next_timepoint <- tibble::tibble(
      next_timepoint = events_subject$event_name[1],
      next_timepoint_date = events_subject$event_date[1]
    )
    return(cbind(timepoint, next_timepoint))
  }

  timepoint_position <- tail(which(time_dif <= 0), 1)

  timepoint <- events_subject |>
    dplyr::slice(timepoint_position) |>
    dplyr::rename(timepoint = 1, timepoint_date = 2)


  # If the timepoint is not the last event, the code searches the next timepint
  if (timepoint_position < length(time_dif)) {
    next_timepoint <- events_subject |>
      dplyr::slice(timepoint_position + 1) |>
      dplyr::rename(next_timepoint = 1, next_timepoint_date = 2)
  } else {
    next_timepoint <- tibble::tibble(
      next_timepoint = NA_character_, next_timepoint_date = as.Date(NA)
    )
  }

  cbind(timepoint, next_timepoint)

}


#' Define the extraction timepoint from a sequence of events
#'
#' @param extractions Extractions data frame:
#'  -  Each row represents an extraction.
#'  -  The first column must be the patient id used to join with the events data frame.
#'  -  The extraction date is assumed to be the first date column.
#' @param events Events data frame:
#'  -  Each row represents the history of a single patient.
#'  -  The first column must be the patient id used to join with the extractions data frame.
#'
#' @return A tibble
#' @export
ody_define_timepoints <- function(extractions, events) {

  # In extractions, which column is the extraction date?
  extractions_date_index <- purrr::map_lgl(extractions, lubridate::is.Date)
  extractions_col_name <- names(extractions)[extractions_date_index][1]

  events_nested <- tidyr::pivot_longer(
    events, -1, names_to = "event_name", values_to = "event_date"
  ) |>
    dplyr::filter(!is.na(event_date)) |>
    # This arrangement is VERY IMPORTANT because get_timepoints assumes dates
    # are in increasing order.
    dplyr::arrange(event_date) |>
    tidyr::nest(events = c("event_name", "event_date"))

  # ids to join are assume to be in the first column
  id_extractions <- names(extractions)[1]
  id_events <- names(events)[1]

  extr_events <- dplyr::left_join(
    extractions, events_nested,
    by = dplyr::join_by(!!id_extractions == !!id_events)
  )

  extr_events_timepoints <- extr_events |>
    dplyr::mutate(
      timepoints = purrr::map2(
        .data[[extractions_col_name]], events, get_timepoint,
        .progress = "Defining timepoints"
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
