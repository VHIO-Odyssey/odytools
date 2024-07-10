# Función para calcular los scores de cada escala del EORTC QLQ-C30

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
#' should have at least 30 columns representing the questionnaire items, sorted
#' in the same order they are presented in the questionnaire. If extra columns are
#' present, the 30 item columns must be located at the end of the dataframe.
#'
#' @param qlq_c30 A dataframe containing the QLQ-C30 questionnaire responses.
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
#'         QLQ-C30 questionnaire,binded with any extra column.
#'
#' @export
ody_qlq_c30_v3 <- function(
    qlq_c30,
    completeness = c("half", "all", "none")) {

  completeness <- rlang::arg_match(completeness)

  # Check if dataframe has more than 30 columns
  if (ncol(qlq_c30) > 30) {
    message(
      "The provided data frame has ",  ncol(qlq_c30), " columns.\nThe last 30 columns are used for scoring."
    )
    extra_cols <- qlq_c30 |>
      dplyr::select(1:tidyselect::last_col(30))

    qlq_c30 <- qlq_c30 |>
      dplyr::select(tidyselect::last_col(29):tidyselect::last_col())

  } else {
    extra_cols <- NULL
  }

  # Verify that all item columns are integer
  if (!all(purrr::map_lgl(qlq_c30, is.integer))) {
    stop("All item columns in qlq_c30 must be numeric")
  }


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
      PF2 = "Functional - Physical functioning",
      RF2 = "Functional - Role functioning",
      EF  = "Functional - Emotional functioning",
      CF  = "Functional - Cognitive functioning",
      SF  = "Functional - Social functioning",
      FA  = "Symptom - Fatigue",
      NV  = "Symptom - Nausea and vomiting",
      PA  = "Symptom - Pain",
      DY  = "Symptom - Dyspnoea",
      SL  = "Symptom - Insomnia",
      AP  = "Symptom - Appetite loss",
      CO  = "Symptom - Constipation",
      DI  = "Symptom - Diarrhoea",
      FI  = "Symptom - Financial difficulties"
    )

  # Optionally bind the identifier column with calculated scores and return
  if (!is.null(extra_cols)) return(dplyr::bind_cols(extra_cols, scores))
  scores
}

