# Función para calcular los scores de cada escala del EORTC QLQ-C30

# Function to check if the items have the expected values.
check_values <- function(variable, values) {
  variable <- na.omit(variable)
  all(variable %in% values)
}

# QLQ-C30 transformation functons
qlq_c30_scores_transform <- function(rs, range) ((rs - 1) / range) * 100

qlq_c30_scores_transform_1 <- function(rs, range) (1 - (rs - 1) / range) * 100

qlq_c30_scores_scale <- function(
    qlq_c30,
    items,
    range,
    transformation,
    completeness) {

  n_missing <- sum(is.na(qlq_c30[items]))
  n_items <- length(items)

  # Always Na if no items available.
  if (n_missing == n_items) return(NA_real_)
  # completenes = "half": NA if half or more of the items are missing
  if (completeness == "half" && n_missing > (n_items / 2)) return(NA_real_)
  # completeness = "all": NA if any item is missing
  if (completeness == "all" && n_missing > 0) return(NA_real_)

  qlq_c30[items] |>
    mean(na.rm = TRUE) |>
    transformation(range)
}

qlq_c30_scores <- function(qlq_c30, scoring_tbl, completeness) {

  purrr::pmap(
    scoring_tbl,
    function(scale, items, range, transformation) {
      tibble::tibble(
        "{stringr::str_to_upper(scale)}" := qlq_c30_scores_scale(
          qlq_c30, items, range, transformation, completeness
        )
      )
    }
  ) |>
    purrr::reduce(dplyr::bind_cols)

}

#' Calculate QLQ-C30 Version 3 Scores
#'
#' This function takes a QLQ-C30 Version 3 questionnaire dataset and calculates
#' the scores for each scale according to the predefined scoring rules. The dataset
#' should have at least 30 columns representing the questionnaire items.
#'
#' @param qlq_c30_df A dataframe containing the QLQ-C30 questionnaire responses.
#' @param group_delta An optional character specifying the column name of the
#' grouping variable to calculate the deltas. The function assumes that the first
#' row of each group is the baseline so make sure the data is properly sorted
#' before using this option. If NULL, the default, no deltas are calculated.
#' @param items_index A numeric vector specifying the column indexes of the items
#' in the QLQ-C30 questionnaire. If NULL, the last 30 columns are assumed to be the items sorted as they are presented in the questionnaire.
#' @param completeness A character vector specifying the completeness criteria:
#' - "half" (default): At least half of the items must be available to calculate the score.
#' - "all": All items must be available to calculate the score.
#' - "none": The score is always calculated as long as at least one item is available.
#'
#' @section Global Health Status:
#'
#' A high score represent a high quality of life (the HIGHER the BETTER).
#'
#' - QL2: Global health status / Quality of life.
#'

#' @section Functional scales:
#'
#' High scores represent a high level of functioning (the HIGHER the BETTER)
#'
#' - PF2: Physical functioning.
#' - RF2: Role functioning.
#' - EF: Emotional functioning.
#' - CF: Cognitive functioning.
#' - SF: Social functioning.
#'
#' @section Symptom scales:
#'
#' High scores represent a high level of symptoms/problems (the HIGHER the WORSE).
#'
#' - FA: Fatigue.
#' - NV: Nausea and vomiting.
#' - PA: Pain.
#' - DY: Dyspnoea.
#' - SL: Insomnia.
#' - AP: Appetite loss.
#' - CO: Constipation.
#' - DI: Diarrhoea.
#' - FI: Financial difficulties.
#'
#' @return A dataframe with the calculated scores for each scale (see below) in the
#'         QLQ-C30 questionnaire,binded with any extra column. The item mapping, a
#'         relation between the item number, the scale it belongs to and the column
#'         name in the input data frame, is stored as an attribute called "item_mapping".
#'
#' @export
ody_qlq_c30_v3 <- function(
    qlq_c30_df,
    group_delta = NULL,
    items_index = NULL,
    completeness = c("half", "all", "none")
  ) {

  completeness <- rlang::arg_match(completeness)

  # Previous checks
  ## qlq_30 checks
  if (!is.data.frame(qlq_c30_df)) {
    stop("qlq_c30_df must be a data frame")
  }
  if (ncol(qlq_c30_df) < 30) {
    stop("The provided data frame must have at least 30 columns")
  }
  ## items_index checks
  if (!is.null(items_index) && length(items_index) != 30) {
      stop("The items_index argument must have 30 elements")
  }
  if (is.null(items_index)) {
  # If no items_index is provided, assume the last 30 columns are the items.
    items_index <- (ncol(qlq_c30_df) - 29):ncol(qlq_c30_df)
  }
  if (!all(items_index %in% 1:ncol(qlq_c30_df))) {
    stop("Item indexes out of range")
  }

  # This is the final items dataframe properly sorted
  qlq_c30 <- qlq_c30_df[items_index] |>
    dplyr::mutate(
      # Factors must be transformed to integer before the checking.
      dplyr::across(tidyselect::where(is.factor), as.integer)
    )
  extra_cols <- qlq_c30_df[-items_index]

  # Item mapping message
  stringr::str_c(
    "Item mapping:\n",
    stringr::str_c(
      "item ", 1:30, ": " , names(qlq_c30_df[items_index]),
      collapse = "\n"
    )
  ) |>
    message()

  # Verify that all item columns have expected values
  expected_values <- c(
    rep(list(1:4), 28),
    rep(list(1:7), 2)
  )

  have_expected <- purrr::map2_lgl(qlq_c30, values, check_values)

  if (!all(have_expected)) {
    unexpeted <- have_expected[!have_expected] |>
      names() |>
      stringr::str_c(collapse = ", ")
    stop("Unexpected values in: ", unexpeted)
  }

  # Only after checking, all the columns are transformed to integer
  qlq_c30 <- qlq_c30 |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.integer))

  # Define the scoring table for transformation of questionnaire responses
  scoring_tbl <- tibble::tribble(
    ~scale, ~items, ~range, ~transformation,
    "ql2", 29:30, 6, qlq_c30_scores_transform,
    "pf2", 1:5, 3, qlq_c30_scores_transform_1,
    "rf2", 6:7, 3, qlq_c30_scores_transform_1,
    "ef", 21:24, 3, qlq_c30_scores_transform_1,
    "cf", c(20, 25), 3, qlq_c30_scores_transform_1,
    "sf", 26:27, 3, qlq_c30_scores_transform_1,
    "fa", c(10, 12, 18), 3, qlq_c30_scores_transform,
    "nv", 14:15, 3, qlq_c30_scores_transform,
    "pa", c(9, 19), 3, qlq_c30_scores_transform,
    "dy", 8, 3, qlq_c30_scores_transform,
    "sl", 11, 3, qlq_c30_scores_transform,
    "ap", 13, 3, qlq_c30_scores_transform,
    "co", 16, 3, qlq_c30_scores_transform,
    "di", 17, 3, qlq_c30_scores_transform,
    "fi", 28, 3, qlq_c30_scores_transform
  )

  # Calculate scores for each scale based on the scoring table
  scores <- purrr::map_dfr(
    as.data.frame(t(qlq_c30)), ~qlq_c30_scores(., scoring_tbl, completeness),
    .progress = "Calculating scores..."
  ) |>
    labelled::set_variable_labels(
      QL2 = "Global health status / Quality of life",
      PF2 = "Physical functioning",
      RF2 = "Role functioning",
      EF  = "Emotional functioning",
      CF  = "Cognitive functioning",
      SF  = "Social functioning",
      FA  = "Fatigue",
      NV  = "Nausea and vomiting",
      PA  = "Pain",
      DY  = "Dyspnoea",
      SL  = "Insomnia",
      AP  = "Appetite loss",
      CO  = "Constipation",
      DI  = "Diarrhoea",
      FI  = "Financial difficulties"
    )


  # Result
  ## Bind extra columns with calculated scores
  scores <- dplyr::bind_cols(extra_cols, scores)

  scales_names <- scores |>
    dplyr::select(QL2:FI) |>
    names()

  ## Add deltas if a grouping variable is provided
  if (!is.null(group_delta))  {

    scores <-
      scores |>
      dplyr::group_by(.data[[group_delta]]) |>
      dplyr::mutate(
        dplyr::across(
          QL2:FI, ~ . - dplyr::first(.), .names = "{.col}_delta"
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        dplyr::all_of(names(extra_cols)),
        tidyselect::starts_with(scales_names)
      )

  }

  ##attributes
  item_mapping <-
    tibble::tibble(
      item = 1:30,
      scale = c(
        "PF2", "PF2", "PF2", "PF2", "PF2",
        "RF2", "RF2", "DY", "PA", "FA", "SL",
        "FA", "AP", "NV", "NV", "CO", "DI",
        "FA","PA", "CF", "EF", "EF", "EF",
        "EF", "CF", "SF", "SF", "FI", "QL2", "QL2"
      ),
      column = names(qlq_c30_df[items_index])
    )
  attr(scores, "item_mapping") <- item_mapping

scores

}

