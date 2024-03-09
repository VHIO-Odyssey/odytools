# Función para calcular los scores de cada escala del EORTC QLQ-C30

# QLQ-C30 transformation functons
qlq_c30_scores_transform <- function(rs, range) ((rs - 1) / range) * 100

qlq_c30_scores_transform_1 <- function(rs, range) (1 - (rs - 1) / range) * 100

qlq_c30_scores_scale <- function(qlq_c30,
                                 items,
                                 range,
                                 transformation,
                                 na_rm) {

  if (all(is.na(qlq_c30[items]))) return(NA_real_)

  qlq_c30[items] |>
    mean(na.rm = na_rm) |>
    transformation(range)
}

qlq_c30_scores <- function(qlq_c30, scoring_tbl, na_rm) {

  purrr::pmap(
    scoring_tbl,
    function(scale, items, range, transformation) {
      tibble::tibble(
        "{stringr::str_to_upper(scale)}" := qlq_c30_scores_scale(
          qlq_c30, items, range, transformation, na_rm
        )
      )
    }
  ) |>
    purrr::reduce(dplyr::bind_cols)

}

#' Calculate QLQ-C30 Version 3 Scores
#'
#' This function takes a QLQ-C30 Version 3 questionnaire dataset and calculates
#' the scores for each scale according to predefined scoring rules. The dataset
#' should have 30 item columns representing questionnaire items and optionally
#' an identifier column.
#'
#' @param qlq_c30 A dataframe containing the QLQ-C30 questionnaire responses.
#'                This dataframe must contain exactly 30 or 31 columns. If 31
#'                columns are present, one column is treated as an identifier column.
#' @param id_col An optional parameter specifying the column index of the
#'               identifier column in `qlq_c30` dataframe. The default index is 1.
#'               This parameter is used only when `qlq_c30` contains 31 columns.
#' @param na_rm A logical value indicating whether NA values should be removed before
#'             calculating the scores. The default is FALSE, so all items needed
#'             to calculate a specific score must be non-NA.
#'
#' @return A dataframe with the calculated scores for each scale in the
#'         QLQ-C30 questionnaire, optionally binded with the identifier column.
#'
#' @export
ody_qlq_c30_v3 <- function(qlq_c30, id_col = 1, na_rm = FALSE) {

  # Check if dataframe has 31 columns, indicating an ID column is present
  if (ncol(qlq_c30) == 31) {
    message(
      "The provided data frame has 31 columns.\nColumn ",
      id_col, " is assumed as the ID column."
    )
    id_column <- qlq_c30 |>
      dplyr::select(tidyselect::all_of(id_col))
    qlq_c30 <- qlq_c30 |>
      dplyr::select(-tidyselect::all_of(id_col))
  } else {
    id_column <- NULL
  }

  # Verify that 30 item columns are present for scoring
  if (ncol(qlq_c30) != 30) stop("qlq_c30 must have 30 item columns")

  # Verify that all item columns are numeric
  if (!all(purrr::map_lgl(qlq_c30, is.numeric))) {
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
    as.data.frame(t(qlq_c30)), ~qlq_c30_scores(., scoring_tbl, na_rm),
    .progress = "Calculating scores..."
  )
  # Optionally bind the identifier column with calculated scores and return
  dplyr::bind_cols(id_column, scores)
}

